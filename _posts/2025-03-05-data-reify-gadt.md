---
title: "Data.Reify for GADTs"
date: 2025-03-05
tags: [Haskell]
description: "'Reimplementing' data-reify for GADTs"
---

## Context

Some parts of my PhD research deal with languages' design and implementation. Eventually, I had to define an AST for a functional, first-order, typed and compiled language. This language would be implemented as a domain-specific language (DSL) in Haskell.

This language would be a prototype for a bigger project (an eDSL, but I'll keep that for later), so the idea is to implement the language, in a very naive way. The simpler, the better.

Since the language is functional, we have to support recursion (e.g. recursive functions). And since we want a typed AST, using a GADT seemed to be the way to go.

In theory, tackling recursion seemed quite straight forward, thanks to the `data-reify`[^data-reify] library.

[^data-reify]: `data-reify` [Hackage Page](https://hackage.haskell.org/package/data-reify)

## `data-reify`, what is it?


This library, result of the 'Type-safe observable sharing in Haskell' paper (A. Gill, 2009)[^paper], basically takes a (potentially infinitely) recursive object (e.g. an AST), and returns a graph, where each node is an AST. These ASTs' sub-expressions are turned into indexes, referring to other nodes in the graph.

Let's take the example of an infinite binary-tree (taken from the package's repository)[^tree-link]

[^tree-link] [Link](https://github.com/ku-fpg/data-reify/blob/6f4a5f18abad93b46e30d5064c14ecd78b583d59/examples/simplify.hs#L106)

```haskell
data Tree a =
    Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

loop1 :: Tree Int
loop1 = Node (Node (Leaf 1) loop1) loop2

loop2 :: Tree Int
loop2 = Node loop1 (Leaf 2)
```

`data-reify` would give us the following graph:

```text
Root: 1
1: Node (Unique 2) (Unique 4)
2: Node (Unique 3) (Unique 1)
3: Leaf 1
4: Node (Unique 1) (Unique 5)
5: Leaf 2
```

Using `Unique` values to refer to other nodes in the graph, we avoid explicit object recursion to represent a recursive function.
Operating on this now-graph `Tree` (like getting the left-most value) is trivial. 

This `Tree` example is very simple, but is enough to showcase what the library does.
Note that, in some example, the _reification_ of a recursive tree can lead to an infinite loop. This can be avoided if you define something like a let-binding in your AST, establishing sharing and avoid infinite tree expansion. 

Now, for `data-reify` to work, some boilerplate is necessary:

```haskell
data TreeF a t =
    LeafF a
  | ForkF t t
  deriving (Show, Functor, Foldable)

instance MuRef (Tree a) where
  type DeRef (Tree a) = TreeF a
  mapDeRef _     (Leaf v) = pure $ LeafF v
  mapDeRef child (Fork l r) = liftA2 ForkF (child l) (child r)
```

We need to define a `TreeF`, similar to the `Tree` ADT, but where the sub-expression have the type `t`, instead of `Tree a`. This is necessary for the graph to have `Unique` values, instead of `Tree`s.


Let's take a look at that `MuRef` type-class:

```haskell
class MuRef a where
  type DeRef a :: * -> *
  mapDeRef :: (Applicative f) =>
              (forall b . (MuRef b, DeRef a ~ DeRef b) => b -> f u)
              -> a
              -> f (DeRef a u)
```

`MuRef` is a type-class which takes a fully applied type as parameter (e.g. `Tree a`, not just `Tree`). `DeRef` is a type family that gives the type of the `Tree` where the subtrees can be anything (i.e. with the type-recursion factored out). Thus, `type DeRef (Tree a) = TreeF a`.
The `mapDeRef` definition is a bit scary. It's actually just `traverse` from the `Traversable` type-class: it takes a `Tree` and a function that can be applied to the `Tree`'s subtrees and give back an `Applicative`. Worded differently, it takes the `Tree` as input, apply the function to each subtree, and use `pure`, `liftA2` and/or `<*>` to lift the result in the `Applicative` returned by that function.

Once the instance of that type-class is written, we can use the 'main' function of this library:

```haskell
reifyGraph :: (MuRef s) => s -> IO (Graph (DeRef s))
```

Thus, given a `Tree`, the function would return a `Graph (TreeF s Unique)`.
The `Graph` type is just a pair of `[(Unique, (TreeF s) Unique)]` and a `Unit`, referring to the _root_ node of the graph.

TLDR: The main 'challenge' with `data-reify` is to understand what `DeRef` does and how to implement `mapDeRef`.

[^paper]: [`Type-safe observable sharing in Haskell`](https://dl.acm.org/doi/10.1145/1596638.1596653)

## Why it does not work as is

What about GADTs? A GADT-defined tree would be something like:

```haskell
data Tree e a where
  Leaf :: a -> Tree e a
  Node :: e a -> e a -> Tree e a
```

Where `a` is the type of the values in the `Tree` and `e` is a _type that needs a type_ (i.e. `e :: * -> *`). The latter defines the type of the subtrees (`e a` means that the subtree must have values of type `a`).

Using a GADT to define tree-like structure is a common way to define, say, ASTs. Haskell being a popular choice of languages implementation, defining ASTs is actually a common thing to do.

A 'problem' with this approach is that building AST is a bit tricky: `Node (Leaf 1) (Node (Leaf 2) (Leaf 3))` has type `TreeF (TreeF (TreeF e)) a`. See how the type information gives us the depth of the tree? Yeah, we don't want that. Instead, we will define and use a `Fix` type to _fix_ this (pun intended) (go checkout this awesome article for more [^fix-gadt])

Let's try to declare the instance of `MuRef` for our GADT 

```haskell
instance MuRef (Fix Tree a) where
  type DeRef (Fix Tree a) = Tree -- ...???
```

OK, first problem here: `Tree` has kind `(* -> *) -> * -> *`, not `* -> *`.
`Fix Tree` has the correct kind though, but the type parameters do not match semantically: the first `*` in `* -> *` is the kind of the sub-expression type. However, in our GADT, that sub-expression has kind `(* -> *)`, not `*` (We would also need to swap the type parameters, but it can easily be done though a type synonym or something).

The type of `MuRef` is also problematic, in the sense that its definition is not compatible with the fact that our sub-expressions are typed: it would not be `(forall b . (MuRef b, DeRef a ~ DeRef b) => b -> f u)` but `(forall b t . (MuRef (b t), DeRef (a t) ~ DeRef (b t)) => b t -> f u)`. But even then, this does not work because the sub-expressions of `a` may not have the same type as `a`. 

For example, consider this AST

```haskell
data AST e a where
  Fst :: e (a, a1) -> AST e a
  Snd:: e (a1, a) -> AST e a
```

`(a, a1)` is not equal to `a`, making the equality between `DeRef (a t)` and `DeRef (b t)` impossible if `b` is a sub-expression of `a` with a different `t`.

Let's see how we could overcome this.

### TLDR

In short:

- The `DeRef` type-family cannot be used with a GADT, as sub-expressions are typed, and the former's definition does not take that into account. `DeRef a` should have hind `(* -> *) -> * -> *` 
- The `mapDeRef`'s constraint uses type equality to ensure that it is applied to the correct type of sub-expression. However, this type equality cannot be used with GADTs  since we can't guarantee that the sub-expressions' semantic type is the same as the parent node's.

[^fix-gadt] [`Fixing GADTs`](http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html)

## Solution

After a couple of days tinkering, I found a (sad) solution: a full 'reimplementation'
Well, saying it's a full reimplementation is a bit of a stretch since we (fortunately) won't have to change the actual logic of `reifyGraph`, just the type definition.

### Graph

Let's start with the easiest part:

```haskell
type Unique = Int -- Same as in the original library

data Graph e a = Graph [(Unique, Node e)] Unique

data Node e = forall t. MkNode (DeRef e Terminal t)

newtype Terminal a = Terminal {unTerminal :: Unique}
```

`Graph` is similar to the original library's: it's still a map from `Unique` to AST/Tree `Node`.

`Node` is just a wrapper around `DeRef e` applied to `Terminal` and `t`, an existentially quantified type. This allows us to have a list of tuples where the second element is a `Node` of 'any' type.

`Terminal a` is a wrapper around a `Unique` is a phantom type, allowing us to say to the type-checker 'Hey!, this sub-expression in the graph has type `a`, if you evaluate this sub-expression, you'll get an `a`, I promise'.

### MuRef

```haskell
class MuRef (a :: Type -> Type) where
    type DeRef a :: (Type -> Type) -> Type -> Type
    type E a :: Type -> Type
    mapDeRef ::
        (Applicative f, e ~ E a) =>
        (forall t'. (MuRef e) => e t' -> f (u t')) ->
        a t ->
        f ((DeRef a) u t)
```

`MuRef a` has kind `* -> Constraint`, meaning that `a` does not contain the semantic type of the expression it represents. E.g., we would define `instance MuRef (Fix Tree)`, instead of `instance MuRef (Fix Tree a)`

`DeRef a` has a kind compatible with GADTs. For example, `DeRef (Fix Tree)` would be `Tree`

`E a` gives the type of the sub-expression of `a`. For `Fix Tree`, it would be `Fix Tree`. We'll use this type-family along with equality constraint to guarantee to `reifyGraph` that the input tree has monotonic subtree types.

Finally, `mapDeRef` takes a tree `a` with _type_ `t` and a function that takes a subtree of `a` (of type `e` with whatever _type_ `t'`) and returns a `f (u t')`.
The function returns, wrapped in an applicative, a tree like `a` but where the subtrees have type `u`. We still guarantee that the returned tree have the same _type_ as the input tree.

### reifyGraph

`reifyGraph`'s type becomes:

```haskell
reifyGraph ::
    (MuRef s, E (E s) ~ E s, DeRef (E s) ~ DeRef s) =>
    s a ->
    IO (Graph (DeRef s) a)
```

In this new definition, we enforce that the input tree's subtrees have the same type as the parent tree.
This might feel a bit restrictive, but the original definition of `mapDeRef` had the `DeRef b ~ DeRef a` constraint, which enforced a similar restriction.

We'll have to add this constraint to every subfunction of `reifyGraph`, but considering there are only three, I guess it's totally acceptable.

Passing a `Fix Tree a` returns a `Tree Terminal a`.

## Conclusion

Too bad we could not _reuse_ code from the original package and had to replicate it to just adapt the types to our needs, considering the internal logic is untouched.
But this choice was taken after a couple of days thinking about it. Considering the small size of the package, we could consider it a quick/dirty solution.

Defining a wrapper around the original `MuRef` type-class is not possible considering that at some point we would have to remove type information, and recover them would be a pain.

I have not tried to use this new version on non-GADT tree structures, that would be an interesting future work.

The code is available on [GitHub](https://github.com/Arthi-chaud/data-reify-gadt) and has been published on [Hackage](https://hackage.haskell.org/package/data-reify-gadt) as `data-reify-gadt`. The repository contains code examples, if you want to take a look.

The day after I published the package, I stumbled across `data-treify` [^data-treify], a 10-year-old library, which seems to tackle our issue. However, unlike what was presented in this article, it seems to be a full-reimplementation with a few more type information.

[^data-treify]: `data-treify` on [Hackage](https://hackage.haskell.org/package/data-treify)

