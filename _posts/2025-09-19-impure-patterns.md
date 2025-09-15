---
title: Using Pattern Synonyms and GHC's CSE
date: 2025-09-19
tags: [Haskell]
description: Leveraging GHC's CSE to optimise complex pattern matching
---

## Introduction

Earlier this year, as part of my PhD, I was working with packed data and Template Haskell. The result was a [Haskell library](https://github.com/Arthi-chaud/packed-data) that generates code which allows users to produce, traverse and deserialise packed data. You can find a dedicated [blog post here](/posts/packed/).

One of the main functions that's generated is the `case` function, which allows pattern matching on a packed ADT's constructor. For example, for a `Tree` type, the library would generate a `caseTree` function, and it could be used as follows:

```haskell

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Generated function
caseTree :: 
  PackedReader (a ': r) r b -> 
  PackedReader (Tree a ': Tree a ': r) r b -> 
  PackedReader (Tree a ': r) r b

-- User-defined function 
-- Computed the sum of the leaf values in a packed tree
sumTree :: PackedReader (Tree Int ': r) r Int
sumTree = caseTree 
  (\leaf -> unpack leaf)
  (\node -> do 
    left <- sumTree
    right <- sumTree
    return (left + right)
  )
```

The `caseTree` function takes two `PackedReader`s as parameter. If the packed `Tree` is a `Leaf`, then the first `PackedReader` will be executed, otherwise the second.

Under the hood, the `case` functions do pointer arithmetic, which we hide behind a typed interface. This allows users to have a compile-time guarantee that their byte-level operations are correct/safe.

While this interface seems usable enough, I felt like we could go a little further and use the native case-expression.

In this blog post, I will show how to use pattern synonyms to do complex and impure pattern matching, and showcase how GHC's common subexpression elimination (CSE) feature can optimise them.

> In this article, I will do extremely unsafe IO operations using `runRW#`. I do not recommend doing this. However, I think the methods applied here could be leveraged with safer computations.
{: .prompt-warning }

## Pattern Synonyms

In Haskell, most of the time, we _pattern match_ on literal values and constructors. For example, with the `Tree` type, a function could return a different value based on the input argument's constructor:

```haskell
f tree = case tree of
  Leaf _ -> "tree is a leaf"
  Node _ _ -> "tree is a node"
```

Additionally, we could also use _Pattern Synonyms_ to create custom patterns that represent the result of (usually light) pure computations. For example, we could define patterns to match on odd or even integers:

```haskell
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

isOdd n = n `mod` 2 == 1
isEven n = n `mod` 2 == 0

pattern Odd :: Int 
pattern Odd <- (isOdd -> True) 

pattern Even :: Int 
pattern Even <- (isEven -> False)

{-# COMPLETE Odd, Even #-}
```

We could use these patterns to check that a list only contains odd numbers:

```haskell
onlyOdds :: [Int] -> Bool
onlyOdds [] = True
onlyOdds (Odd : xs) = onlyOdds xs
onlyOdds (Even : xs) = False
```

If pattern synonyms seem a bit obscure, I recommend this [blog article](https://doisinkidney.com/posts/2018-04-12-pattern-synonyms.html), it provides good examples.

Our `Odd` pattern synonym runs a simple and pure computation (`isOdd`) and uses the result value to know if the match should succeed.
However, pattern synonyms only work with pure computations, we can't pattern match on, e.g., `IO Int` values.

## Using `PackedReader` as an interface for low-level operations

### The safe way

What about `Packed (Tree Int)`? Well, `Packed` values are just `ByteString`s under the hood. We could use the functions from the `bytestring` library to check the value of a byte at a given offset to know if the packed `Tree` is a `Leaf` or a `Node`.

But the computations that allows traversing packed values, `PackedReader`s, are a bit more complex: they are IO functions that threads a pointer (instead of a `ByteString`, for performance reasons)

```haskell
type PackedReader p t v = Ptr p -> IO (Ptr t, v)
```

Why is the IO here? `PackedReader` is an interface for byte-level operations, like raw memory access. In 'safe' Haskell, the latter can be done using `IO` operations (like `peek` and `poke`). So defining `PackedReader` as an IO computation is the most 'legal' to do this.

### _Yeet_-ing IO

Thankfully for us, Haskell exposes internal / unsafe functions for raw memory access. More specifically, it exposes the `runRW#` function, as well as primitives that allows us to peek into the memory, like `readWord8OffAddr#`.

If we get rid of the `IO` in the definition of `PackedReader`, would that help us with our pattern matching problem? Yes, because we can use `runRW#` to run IO operations outside the monad in pattern synonyms. Any Haskell developer will rightfully cringe at this idea. But let's say we are still doing things hypothetically at this point.

## Custom Pattern Synonyms

Let's redefine `PackedReader` and define the following pattern synonyms for our `Tree` type.

```haskell
type PackedReader p t v = Ptr p -> (Ptr t, v)

pattern PackedLeaf :: PackedFragment (a ': r) -> PackedFragment (Tree a ': r)
pattern PackedLeaf pf <- (readTreeTag -> (0, pf))

pattern PackedNode :: PackedFragment (Tree a ': Tree a ': r) -> PackedFragment (Tree a ': r)
pattern PackedNode pf <- (readTreeTag -> (1, pf))

readTreeTag :: PackedFragment (Tree a ': r) -> (Word8#, PackedFragment n) 
readTreeTag (PF ptr@(Ptr addr) i) = case runRW# (readWord8OffAddr# addr 0#) of
    (# _, t #) -> case t of
        0 -> (t, PF (ptr `plusPtr` 1) (i - 1))
        1 -> (t, PF (ptr `plusPtr` 1) (i - 1))
        _ -> error $ "Bag tag: Got " ++ show (W8# t)

{-# INLINE readTreeTag #-}
{-# INLINE PackedNode #-}
{-# INLINE PackedLeaf #-}
{-# COMPLETE PackedLeaf, PackedNode #-}
```

Let's see what going on there:

- We define two pattern synonyms, one for each constructor of the `Tree` ADT.
- In each pattern synonym
  - We call the `readTreeTag` function, which dereferences the pointer and reads the tag.
  - We pattern match on the tag. If the match succeeds, we return the shifted pointer, which we type accordingly to the packed value.
  - If the pattern match fails, it will fall back to the following case in the `case` expression.

Don't worry about having to write these functions for each data type. We can easily generate these functions using Template Haskell.

## Common Subexpression elimination

If we look hard into our custom pattern synonyms for `Tree`, we notice a major issue: we call the function `readTreeTag` twice, once in each pattern. However, the goal of the `packed` library is to be fast. We can't afford to run functions twice, especially if we produce tuples every time.

Thankfully, GHC, our lord and saviour, can do CSE: common subexpression elimination. To understand what that is, let's write our case expression using `if` statements to reflect what the produced low-level and imperative code will look like:

```haskell
-- Using our pattern synonyms:
sumTree' = \case 
  PackedLeaf leafPtr -> unpack leafPtr
  PackedNode nodePtr -> let 
    (nextPtr, leftSum) = sumTree'' nodePtr
    (nextPtr', rightSum) = sumTree'' nextPtr'
    in (nextPtr', leftSum + rightSum)

-- Without pattern matching (i.e. what the compiled code would behave like)
sumTree'' ptr =
    let res = (readTreeTag ptr)
    in if fst res == 0
        then unpack (snd res)
        else let res = (readTreeTag ptr)
              in if fst res == 1
                  then
                      let
                          (nextPtr, leftSum) = sumTree'' nodePtr
                          (nextPtr', rightSum) = sumTree'' nextPtr'
                       in
                          (nextPtr', leftSum + rightSum)
                  else error "Bad tag"
```

Theoretically, this call would call the `readTreeTag` function twice, producing two tuples which we might not need half of the time. Again, we can't afford to produce values we do not need, especially when the second value of the tuple is the same.

GHC's CSE algorithm will notice the redundancy, and optimise the code like this:

```haskell
sumTree'' ptr =
    let res = (readTreeTag ptr)
    in if fst res == 0
        then unpack (snd res)
        else if fst res == 1
            then
                let
                    (nextPtr, leftSum) = sumTree'' nodePtr
                    (nextPtr', rightSum) = sumTree'' nextPtr'
                 in
                    (nextPtr', leftSum + rightSum)
            else error "Bad tag"
```

Wow, no duplicate call to `readTreeTag` :tada:

This is theoretical. How can we know this for sure? We have a few tools that we can use:

- Using the `-ddump-simpl` option, GHC will print the core code, which is an optimised version of the program, in a language that is a subset of Haskell. When printing the code, we can observe that the code is optimised, and the `readTreeTag` is indeed called only once
- We had previously set up benchmark suites for the `case` functions. Running these benchmarks on code that uses our pattern synonyms, we notice that the execution times are the same, no slowdowns or speedups.

## Final result

Since we got rid of the `IO` in the definition of `PackedReader`, we consequently got rid of the monadic interface it offered: no more do-notations. We can recover this easily using the `Identity` monad:

```haskell
type PackedReader p t v = Ptr p -> Identity (Ptr t, v)
```

But we still have to explicitly pass the pointer around. By making `PackedReader` a `newtype` and writing a helper function `threadedWith`, we can thread it implicitly, like in the original `PackedReader`.

```haskell
newtype PackedReader p t v = PackedReader {
  runPackedReader :: Ptr p -> Identity (Ptr t, v)
  }

mkPackedReader = PackedReader
```

This means that, using our generated pattern synonyms we can write the `sumTree` function like this:

```haskell
sumTree = mkPackedReader $ \case 
  PackedLeaf leaf -> unpack leaf
  PackedNode node -> threadedWith node $ do
    left <- sumTree 
    right <- sumTree 
    return $ left + right
```

which feels a bit more natural than the generated `case` functions.

## Conclusion

In this blog post, I showcased how to rely on GHC's CSE feature to optimise pattern synonyms that do common operations.

Again, I used highly unsafe IO operations for the sake of performance. I do not recommend doing this in a production context.

Thanks for reading!
