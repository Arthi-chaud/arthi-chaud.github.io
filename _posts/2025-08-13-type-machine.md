---
title: Type-machine
date: 2025-08-13
tags: [Haskell]
description: Using Template Haskell to derive the structure of records and simulate structural subtyping
---

_Code available on [GitHub](https://github.com/Arthi-chaud/type-machine), package available on [Hackage](https://hackage.haskell.org/package/type-machine)._

## Introduction

In Haskell, we usually model data using algebraic data types, like this:

```haskell
data Maybe a = Nothing | Just a 
```

Here we defined a `Maybe` type, which has two constructors, `Nothing` and `Just`.
The `Just` constructor has one argument, while `Nothing` as none.

It is common to consider these constructors' arguments as _fields_, which can be mainly distinguished by their position in the constructor's declaration.
When data types have many fields, it becomes a bit of a pain to select them, for example, in functions like these:

```haskell
data URL = URL 
  String              -- ^ Scheme 
  String              -- ^ Hostname 
  Maybe Int           -- ^ Port 
  String              -- ^ Path 
  [(String, String)]  -- ^ Query Parameters 
  Maybe String        -- ^ Fragment (#)

getPath :: URL -> String
getPath (URL _ _ _ path _ _) = path
```

Thankfully, Haskell supports a _record syntax_[^record][^fix], which allows _naming_ fields, like this:

[^fix]: A previous version of the post stated that this syntax was introduced in GHC 7.4.1, which is not true. Thanks to everyone who reported this mistake :).

```haskell
data URL = URL {
  scheme :: String,
  hostname :: String,
  port :: Maybe Int,
  path :: String,
  queryParams :: [(String, String)],
  fragment :: Maybe String,
}
```

This lets us write the `getPath` function much more concisely.

```haskell
getPath :: URL -> String
getPath = path
```

Unfortunately, since Haskell's type system is primarily nominal (=/= structural), we cannot set constraints that asks records to have a given set of fields. Yes, there is a [`HasField` typeclass](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html), but it is too verbose for my liking, and only allows selecting a field, not updating it:

```haskell
getName :: (HasField "name" a String) => a -> String
getName elem = getField @"name" elem
```

[Lenses](https://hackage.haskell.org/package/lens) do provide getters and setters, but not in a name-polymorphic way, like `HasField`. Alternatively, it is common to use heterogeneous lists to build records and work around the type system to simulate structural subtyping. However, we will see in the [microbenchmark](#microbenchmark) section why using these lists is not optimal.

Let's consider another strongly typed language, TypeScript, which does support structural subtyping. It also provides a set of [utility types](https://www.typescriptlang.org/docs/handbook/utility-types.html), which I will call _type-transformers_.

I drew inspiration from TypeScript, wrote a bit of Template Haskell, and developed what became `type-machine`, a Haskell library that allows deriving the structure of record types and generate constraints to simulate structural subtyping.

Here's what it currently looks like:
```haskell
{-# LANGUAGE DuplicateRecordFields #-}

type_ "Vector2" (record ["x", "y"] [t|Int|])
-- Generates
-- data Vector2 = Vector2 {
--	x :: Int,
--	y :: Int,
-- }

type_ "Vector3" (union <::> ''Vector2 <:> record ["z"] [t|Int|])
-- Generates
-- data Vector3 = Vector3 {
--	x :: Int,
--	y :: Int,
--	z :: Int,
-- }

defineIs ''Vector2
deriveIs ''Vector2 ''Vector3

translateX :: (IsVector2 a) => Int -> a -> a
translateX n v = setX (n + getX v) v

example = translateX -1 (Vector3 1 2 3) 
```

This blog post presents the library's features, shows a use-case with [Servant](https://www.servant.dev) and evaluates its impact both at compile time and runtime.

[^record]: [GHC's documentation: record syntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/traditional_record_syntax.html)

## Features

### `type_` and type-transformers

To derive a new type, we would use the `type_` function:

```haskell
type_ :: String -> TM Type -> Q [Dec]
```

The first argument is the name of the type to derive. The second argument to a `TM` computation that produces a `Type`. As any Template Haskell, it returns a `Q [Dec]`.

#### `Type` and `TM`

The `Type` ADT models the structure of a record type. Its definition is simple and straight forward[^coupling]:

```haskell
data Type = Type
    { name :: Name
    -- ^ Name of the data type
    , fields :: Map String BangType
    -- ^ Fields of the data type
    , typeParams :: [(String, Maybe Kind)]
    -- ^ Type parameter of the ADT
    }
```

[^coupling]: Note: `Kind` and `BangType` are types from the `template-haskell`. I don't like how tightly it couples `Type` with that library, but defining custom types for them in `type-machine` felt like it would've been redundant.

The `TM` monad (standing for `Type Machine`) is a simple type alias to `WriterT [String] Q`, where `[String]` represents potential error or warning messages issued during the computation.

#### Type-transformers

Type-transformers are functions that take a `Type` and return a `TM Type`.

The library comes with a small collection of such functions, inspired by TypeScript's usability types, defined in [`TypeMachine.Functions`](https://hackage-content.haskell.org/package/type-machine/docs/TypeMachine-Functions.html):

```haskell
data A = A { a :: Int, b :: Int, c :: Int }

type_ "Picked" (pick ["a", "c"] <::> ''A)
-- Generates the following
data Picked = Picked { a :: Int, c :: Int }

type_ "Omitted" (omit ["b", "c"] <::> ''A)
-- Generates the following
data Omitted = Omitted { a :: Int }

type_ "Record" (record ["a", "b"] [t|String|])
-- Generates the following
data Record = Record { a :: String, b :: String }

type_ "Intersected" (intersection <:> pick ["c"] ''A  <::> ''Record)
-- Generates the following
data Intersected = Intersected { a :: String, b :: String, c :: Int }

data B a :: B { f :: Maybe a } 
type_ "Applied" (apply [t|Int|] <::> ''B)
-- Generates the following
data Applied = Applied { f :: Maybe Int }
```

This is not an exhaustive list, but it should give you an idea of what these transformers do and how to use them. If you have workedwith TypeScript's type-level operations, you might find the syntax familiar.

#### Infix Operators

There are two [main infix operators](https://hackage-content.haskell.org/package/type-machine/docs/TypeMachine-TM-Syntax.html), `<:>` and `<::>`.

##### The `<:>` operator

Remember that type-transformers are usually functions with type `Type -> TM Type`? Well, `<:>` allows chaining type-transformers. So it is like the binding operator, but with a twist. Some functions like `intersection` take two `Type` values as parameter, so `>>=` wouldn't work.
The (somewhat ugly and unsatisfactory) workaround is to define a type of function whose parameters can be lifted in the `TM` monad.

```haskell
class LiftableTMFunction f where
    applyTM :: forall a b. (f ~ (a -> b)) => (a -> b) -> TM a -> b

instance LiftableTMFunction (a -> TM b) where
    applyTM f v = v >>= f

instance LiftableTMFunction (a -> b -> TM c) where
    applyTM f ma b = do
        a <- ma
        f a b

instance LiftableTMFunction (a -> b -> c -> TM d) where
    applyTM f ma b c = do
        a <- ma
        f a b c

-- Etc.
```

See how we have to define instances of the typeclass for each number of parameters? This is why I don't like it. Maybe we could generate these instances using Template Haskell.

##### The `<::>` operator

If type-transformers are functions with type `Type -> TM Type`, how could we pass a Template Haskell `Name` (prefixed with two single quotes) to them? This is what the `toType :: Name -> TM Type` function is for. However, using this function would add verbosity to the type-transformer expressions:

```haskell
type_ "Picked" (pick ["a", "c"] <:> toType ''A)
```

So, for brevity, I defined the `<::>` operator:

```haskell
t <::> n = t <:> toType n
```

##### Type-transformers aliases

Additionally, the [`TypeMachine.Infix`](https://hackage-content.haskell.org/package/type-machine/docs/TypeMachine-Infix.html) module provides additional infixes and aliases for the `union` (`&`) and `intersection` (`|`) type-transformers.

The collection of infixes includes:

- `<#|>`
- `<:#|>`
- `<#|:>`
- `<#&>`
- `<#&:>`
- `<:&#>`

The position of the `:` in the infix indicates which side of the infix accepts a `Name`, and the position of `#` shows which side will have priority in case of an overlap ([see `union`, `union'`, `intersection` and `intersection'`](https://hackage-content.haskell.org/package/type-machine/docs/TypeMachine-Functions.html#g:3)).

#### API

The definition of `TM`, `Type` and the infix operators are visible in the [package's API](https://hackage-content.haskell.org/package/type-machine), meaning that you can write your own (possibly more advanced) type-transformers.

### `defineIs` and `deriveIs`

We saw how to derive types using type-transformers. Now let's talk about how to get structural subtyping (almost) 'for free'.

For any record types, the `defineIs` function will generate a typeclass with:

- For each field, a getter and a setter function
- A function to transform a value into the target type.

The `deriveIs` function generates an instance of the typeclass defined by `defineIs` for the given type.

Here's an example :

```haskell
data Id = Id { value :: Int } 

defineIs ''Id

-- Generates the following
class IsId a where 
  getValue :: a -> Int
  setValue :: Int -> a -> a
  toId :: a -> Id

deriveIs ''Id ''Id

-- Generates the following
instance IsId Id where 
  getValue = value
  setId newValue id_ = id_{value = newValue }
  toId = id

data Id2 = Id2 { value2 :: Maybe Int }

deriveIs ''Id ''Id2
-- Fails, because Id2 does not have a 'value' field
```

The derivation algorithm tries to be smart: if a field's type is a Monoid, it will be able to fall back when a field is missing in the source type:

```haskell
defineIs ''Id2
deriveIs ''Id2 ''Id

-- Generates the following
instance IsId2 Id where 
  getValue2 = Nothing
  setId _ id_ = id_
  toId = Id Nothing
```

We can use these `Is` typeclasses to simulate structural subtyping:

```haskell
-- From the introduction
defineIs ''Vector2
deriveIs ''Vector2 ''Vector3

translateX :: (IsVector2 a) => Int -> a -> a
translateX n v = setX (n + getX v) v

example = translateX -1 (Vector3 1 2 3) 
```

### Limitations

Obviously the library has some shortcomings such as:

- Having to use the `DuplicateRecordFields` GHC extension
  - And having to deal with possible ambiguity when accessing a record's field
- The `type_` function only handles record ADTs with exactly one constructor
- I'm not really satisfied with having to pass a `Q Type` to the `record` type-transformer
  - And more generally, for advanced record manipulation, the programmer might need to be somewhat familiar with Template Haskell
- The `Is` typeclasses are useful, but are not a full replacement for the `HasField` typeclass. I would like a future version of `type-machine` to provide something like `defineConstraint`:

```haskell
defineConstraint "Has2DCoord" ["x", "y"] ''Vector2

-- Would generate the following constraint and getters/settings
type Has2DCoord a = (HasField "x" a Int, HasField "y" a Int)

setX :: (HasField "x" a Int) => Int -> a -> a
getX :: (HasField "x" a Int) => a -> Int

setY :: (HasField "y" a Int) => Int -> a -> a
getY :: (HasField "y" a Int) => a -> Int


translate2DCoord :: Has2DCoord a => n -> a -> a
translate2DCoord n vev = setY (+ n) (setX (+ n) vec) 
```

## Example

In web APIs, it is common to have a database model (say `UserRecord`), whose structure can be used to derive user-facing models, like in responses (`UserResponse`, a `UserRecord` without the password) or forms (`UserForm`, a `UserRecord` without an ID). We can define and derive these models like this:

```haskell
data UserRecord = { 
  id :: Int, 
  name :: String, 
  password :: String 
}

type_ "UserForm" (omit ["id"] <::> 'UserRecord)
type_ "UserResponse" (omit ["password"] <::> 'UserRecord)

defineIs UserResponse
deriveIs UserResponse UserRecord

deriving instance FromJSON UserForm
deriving instance ToJSON UserResponse
```

If we were to use Servant, we could define 2 endpoints, one to `POST` a new user, and one to `GET` one by ID:

```haskell
type UserApi =
        ReqBody '[JSON] UserForm :> Post '[JSON] UserResponse 
    :<|>
        ":id" :> Capture "id" Int :> Get '[JSON] UserResponse

server :: Server UserApi
server = createUser :<|> getUser
```

We could have a `saveUserRecord`, which creates, persists and returns a `UserRecord` from a `UserForm`. Since `UserForm` is not a subtype of `UserRecord` (as it has no ID), we can't use `deriveIs` to generate the function to transform the former into the latter.
However, since `UserRecord` is a subtype of `UserResponse`, we can use the generated `toUserResponse` function to do the conversion for us:

```haskell
createUser userForm = do 
    userRecord <- saveUserRecord userForm 
    let response = 
        toUserResponse userRecord 
    return response


saveUserRecord :: UserForm -> Db UserRecord
saveUserRecord userForm = do 
    newId <- getNextId 
    let record = UserRecord newId (name userForm) (password userForm)
    save record
    return record
```

We could do something similar when retrieving a `UserRecord`:

```haskell
getUser userId = do
    userRecord <- getUserRecord userId
    let response = toUserResponse userRecord
    return response
   
getUserRecord :: Int -> Db UserRecord
getUserRecord = getByPrimaryKey
```

You can find other example use-cases [in the repository](https://github.com/Arthi-chaud/type-machine/tree/main/examples)

## Microbenchmark

As mentioned in the introduction, it's not unusual to use heterogeneous lists to model records and simulate structural subtyping.

However, because heterogeneous lists are not first-class citizens to GHC, the compiler will not be able to optimize, say, selection. Thus accessing a field is like traversing a list, which is O(n).

If we compare the time it takes to build and traverse records defined using `type-machine`, [`extensible`](https://hackage.haskell.org/package/extensible) and [`superrecord`](https://hackage.haskell.org/package/superrecord) (using [Criterion](https://hackage.haskell.org/package/criterion)), we can see that `type-machine` is faster. This is not surprising, as we generate _native_ records.

| Library        | type-machine | extensible | superrecord |
|----------------|--------------|------------|-------------|
| Build time     | 21.67ns      | 24.32ns    | 27.96ns     |
| Traversal time | 22.43ns      | 168.3ns    | 309.4ns     |
| Compilation time | 5.07s        | 16.41s     | 6m38        |

Note: Benchmarks were run on an Intel machine with two 986 Xeon Gold 6244 CPUs at 3.60 GHz, with 32Gb of RAM, running Ubuntu 22.04 LTS, using GHC 9.10.1 and the latest versions of the two other libraries. All code for these benchmarks is available [on GitHub](https://github.com/Arthi-chaud/type-machine/tree/main/benchmarks)

## Conclusion


While I am quite happy with the type-transformers, I feel like more work need to be done with the support for structural subtyping. The next step would be to write a flexible alternative to `deriveIs`/`defineIs`, `deriveConstraint`. 

Using Template Haskell/meta-programming to enhance the performance of programs is a topic I am really interested in, and `type-machine` seems to be a nice example of how can leverage TH to make Haskell programs more efficient.

Feedback is welcome. Feel free to leave a comment below or open issues/pull requests on the repo!

## Links

Thanks for reading! If you're interested, check out `type-machine` on [GitHub](https://github.com/Arthi-chaud/type-machine) and [Hackage](https://hackage.haskell.org/package/type-machine).
