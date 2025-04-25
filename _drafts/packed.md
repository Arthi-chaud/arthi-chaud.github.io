---
title: Packed Data support in Haskell 
date: 2025-XX-XX 
tags: [Meelo]
description: Packed Data x Haskell = Type-safety, performance and portability
---

Blog Post = TLDR of Paper published in ECOOP 25

# Introduction: Packed Data

When programs want to persist data or send it over the network, they need to serialise it (e.g. using JSON or XML). On the other hand, when a program receives data (e.g. from the network) or reads it from a file, data needs to be deserialised.

These de/serialisation steps are necessary because the in-memory representation/layout of the data cannot be used to save it to a file or send it over the network, mainly because of the pointers it may contain. 

Additionally, de/serialising data has a cost: it takes time, and the serialised version of the data is usually bigger than its in-memory representation.

Now, what if we didn't have to serialise the data before sending it to a client, and what if the client could use the data from the network as-is, without any marshalling steps?
We could save some time, on both the server and client side. 

Introducing the 'packed' data format, a binary format that allows this. Additionally, traversals on packed trees is proven to be faster than on 'unpacked' trees: as the fields of data structures are inlines, there are no pointer jumps, thus making the most of the L1 cache.

A few projects use or leverage this 'packed' approach. For example, Cap'n Proto[^1] is a popular library that allows packing data (i.e. build a binary buffer containing the data), using that buffer as-is (e.g. traverse it and 'unpack' a specific field of a structure).
However, no languages support packed data natively. Haskell does support compact normal forms (CNF), but the `Compact` data type does not allow using (e.g. deconstructing) packed values as-is. We should note the existence of Gibbon, a research compiler that accepts functional programs and generates programs that use packed data natively.

[^1]: [Cap'n Proto website](https://capnproto.org)

Unfortunately it looks like the use of packed data is limited to research projects, probably because it's kinda hard to support it.

In this post, I will introduce the `packed-data` Haskell library. It allows packing and unpacking data, as well as traversing packed data as-is (with a custom `case` function), with no marshalling step, thanks to the power of types.

This blog post is a short version of the paper published at ECOOP 2025, titled _Type-safe and portable support for packed data_.

## `packed-data`

Using Template Haskell, `packed-data` generates all the necessary code so that one can, for _any_ type:

- *pack* data (i.e. serialise the data into a binary format)
- *unpack* data (i.e. deserialise from that binary format)
- *traverse* the data, thanks to a `case` function that allows pattern matching on a data type's constructor.

Let's consider the example of a binary tree:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

We can use this Template Haskell entrypoint (from `Data.Packed`):

```haskell
$(mkPacked ''Tree [])
```

It generates instances of the following classes:

```haskell
class Packable a where
  write :: a -> NeedsWriter a r t

class Unpackable a where
  read :: PackedReader '[a] r a
```

along with the function

```haskell
caseTree :: (PackedReader '[a] r b) ->
            (PackedReader '[Tree a, Tree a] r b) ->
            (PackedReader '[Tree a] r b)
```

Ooh, these types are scary. Don't worry, we'll look into them right now.

### NeedsBuilder

To *build* packed data, we use an intermediary buffer (or a `ByteString.Builder`) which, thanks to types, can restrict what data it takes as input.

We didn't come up with that idea, it is heavily inspired by the examples from the _Linear Haskell_ paper [^1].

[^1]: https://dl.acm.org/doi/10.1145/3158093

The paper's authors defined the following `Needs` type, where
- `p` tells the types of the data needs so that the buffer is _reader_
- `t` gives the types of all the data packed in the buffer, once it's ready (i.e. when `p` is an empty list) 

```haskell
newtype Needs (p :: [Type]) (t :: [Type]) = Needs Builder
```

For example, `Needs '[Int] '[Tree Int]` is an incomplete buffer that needs an `Int` before we can reify it into a proper `Packed (Tree Int)`.
`Needs '[] '[Char, Char]` is ready to be reified, and the produces buffer will contain two `Char`s.

We expand on that idea to make the building of the buffers monadic. For example, with Template Haskell, we generate the following code (or something similar) to `write` a `Tree` into a `Needs`

```haskell
instance Packable Tree where
  write (Leaf n) = do 
    writeTag 0
    write n -- The library provides an instance of Packable Int
  write (Node t1 t2) = do
    writeTag 1
    write t1 -- We call the function recursively
    write t2
```

The `Needs` type is just a wrapper around a `Data.ByteString.Builder`. The buffer is not created when we call the `write` function. Instead, we need to call `finish`:

```haskell
finish :: Needs '[] t -> Packed t
```

Notice how the first type parameter of the input `Needs` is empty. It ensures that the buffer is full and ready. 

The library also provides the shorthand `pack` function:

```haskell
pack :: (Packable a) => Packed '[a]
pack  = finish . withEmptyNeeds . write
```

### PackedReader

`PackedReader` represents reading operations on packed data. It is a monad. More specifically, it's an indexed one. This means that the 'state' of the computation is reflected in its type.

For example, `PackedReader '[Int] '[Int, Int] Char` reads a single `Int` in a buffer where the *rest* of the buffer contains two other `Int`. This means that we could use that reader with a `Packed '[Int, Int, Int]`, but not a `Packed '[Char, Int, Int]`. The operation produces a `Char`

Why does this monad exist? Under the hood, the `PackedReader` [passes around a `Ptr`](https://hackage-content.haskell.org/package/packed-data-0.1.0.3/docs/src/Data.Packed.Reader.html#runPackedReader), and reads data using `peek`, in the IO monad. `PackedReader` abstract away pointer manipulations while also ensuring type-correct reading operations on packed buffers.

### `case` function

The `case` function is generated with Template Haskell.

For the `Tree` ADT, we generate the `caseTree` function. It takes as many `PackedReader` as parameter as there are constructors in `Tree`. Each `PackedReader` has a type that matches each constructor's type:

```haskell

data Tree a = Leaf a | Node (Tree a) (Tree a)

caseTree :: 
  -- The first constructor only has an 'a' 
  (PackedReader '[a] r b) -> 
  -- The second has two trees
  (PackedReader '[Tree a, Tree a] r b) -> 
  -- the resulting reader reads a 'Tree'
  (PackedReader '[Tree a] r b)
```

We can use the generated `caseTree` function to unpack a `Tree`:

```haskell
-- This function is generated by `mkPacked`
instance (Unpackable a) => Unpackable (Tree a) where
  read = caseTree 
    (\leaf -> do 
      n <- read -- 'n' has type Int
      return n
    )
    (\node -> do 
      left <- read -- 'left' is a 'Tree a' 
      right <- read -- same for 'right'
      return $ Node left right
    )
```

In the second lambda, we don't need to pass parameter to the two `read`s, because the first one changes the 'state' of the reading computation, and has shifted the pointer to the next subtree in the packed `Node`.

### Examples

You can find examples of tree traversals in the [package's repository](https://github.com/Arthi-chaud/packed-data).

## Benchmarks

How fast is all that stuff? To answer that question, we ran benchmarks on simple tree traversals: summing the values in a tree, getting the right-most value in a tree, evaluating an AST for an arithmetic expression, and incrementing the leaves values in a tree.

We compared the execution of these operations with C, 'Unpacked'/native Haskell and Gibbon, on trees of sizes between 1 and 20 (for example, a tree of size 5 has 2^5 = 32 leaves, dispatched symmetrically). We noticed the following:

- For summing the values, we notice a small speed-up (20%) compared to 'unpacked' Haskell.
- On the other hand, getting the right-most value in the tree is 5 times slower than native Haskell.
- Evaluating the AST is surprisingly faster (2.5x) compared to both Haskell and C.
- Finally, incrementing the values of a packed tree is as fast as incrementing a native, unpacked tree in Haskell. We think this is due to the fact that the `ByteString.Builder` does only one big memory allocation, while the native operation does one for each node. 

(You can run these benchmarks on your machine, the source code is available on [GitHub](https://github.com/Arthi-chaud/packed-data))

> We should note that these benchmark results vary depending on the architecture of the machine's CPU. ARM CPUs do not enjoy such speedups compared to Intel CPUs
{: .prompt-warning }

### Interpretation of the results

Well, we did get some speed-ups, but it's not consistent across our benchmark cases.

Using a packed layout should always provide faster traversals, as we avoid jumping using pointers, and make the most of the L1 cache.

We suspect that the monadic approach of the `PackedReader` leads to some computation overhead due to the intensive use of IO operations (like `peek`).

For fun, we implemented a non-monadic version of a traversal to get the right-most value in a packed tree. This version does not use `PackedReader` and uses `peek` without any abstraction. (The code is available [here](https://github.com/Arthi-chaud/packed-data/blob/33072fdaccd3f25cd416276780f2a2b2778e29d3/benchmark/tree/Traversals.hs#L70)).
It did speed up the traversal (30% faster than the `PackedReader`), but is still slower (4x) than native Haskell.

This confirms that there is a computing overhead causes by our monadic abstraction of pointer manipulation. 

## Future work and conclusion 
