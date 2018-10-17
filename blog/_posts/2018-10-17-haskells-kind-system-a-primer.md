---
layout: post
title: Haskell's kind system - a primer
issueId: 3
---

<!-- markdownlint-disable MD002 -->

In this post we'll explore Haskell's kind system,
the similarities between types and kinds,
and show how these can be leveraged to write safer and more reusable code.

Here's what's in store for today:

- [Types and Kinds](#types-and-kinds)
- [Data constructors and type constructors](#data-constructors-and-type-constructors)
- [Type signatures and kind signatures](#type-signatures-and-kind-signatures)
- [HOFs and HKTs](#hofs-and-hkts)
- [Other kinds](#other-kinds)
  - [Unboxed/unlifted types](#unboxedunlifted-types)
  - [Constraints](#constraints)
  - [Datatype promotion](#datatype-promotion)
  - [GHC.TypeLits](#ghctypelits)
- [Kind polymorphism](#kind-polymorphism)
- [Levity polymorphism](#levity-polymorphism)

<!--more-->

NB: this article was written with GHC 8.4.3 in mind.

## Types and Kinds

Simply put:

> Just like values/terms can be classified into types, types can be classified into kinds.

The values `"hello"` and `"world"` are of type `String`. The values `True` and `False` are of type `Bool`.
Similarly, the types `String` and `Bool` are of kind `*`, pronounced "star".

{:.tex}
![State type](/img/diagrams/kind-system001.svg)

Just like we can use `:t`/`:type` in GHCi to check the type of a term, we can use `:k`/`:kind` to check the kind of a type.

```hs
λ> :t True
True :: Bool

λ> :k Bool
Bool :: *
```

In standard Haskell, all inhabited types (types that have at least 1 value) are of kind `*`.
So `Int`, `Int -> String`, `[Int]`, `Maybe Int`, `Either Int Int` are all of kind `*` because there's at least one term for each of these types[^void].

Not all types are inhabited though.
`Maybe` and `Either`, for example, are uninhabited.
There is no term of type `Maybe`, not even the infinite loop!

```hs
λ> x = undefined :: Maybe
<interactive>:9:18: error
    • Expecting one more argument to ‘Maybe’

λ> f x = f x :: Maybe
<interactive>:10:14: error:
    • Expecting one more argument to ‘Maybe’
```

Well, if `Maybe` and `Either` are not inhabited types, what are they then?
They're *type constructors*.

## Data constructors and type constructors

Just like we have data constructors for creating data, we also have type constructors for creating types.

```hs
λ> data Person = MkPerson { name :: String, age :: Int }

λ> :t MkPerson
MkPerson :: String -> Int -> Person
```

`MkPerson` is a data constructor that, given two values `name` and `age` of type `String` and `Int`, creates another value of type `Person`.
In other words, `MkPerson` is a value of type `String -> Int -> Person`.

```hs
λ> data Either a b = Left a | Right b

λ> :k Either
Either :: * -> * -> *
```

Similarly, `Either` is a type constructor that, given two types `a` and `b` of kind `*`, creates another type of kind `*`.
In other words, `Either` is a type of kind `* -> * -> *`.

Just like data constructors are curried and can be partially applied, so can type constructors.

```hs
λ> :t MkPerson
MkPerson :: String -> Int -> Person
λ> :t MkPerson "Diogo"
MkPerson "Diogo" :: Int -> Person
λ> :t MkPerson "Diogo" 29
MkPerson "Diogo" 29 :: Person
```

{:.tex}
![MkPerson data constructor](/img/diagrams/kind-system002.svg)

```hs
λ> :k Either
Either :: * -> * -> *
λ> :k Either String
Either String :: * -> *
λ> :k Either String Int
Either String Int :: *
```

{:.tex}
![Either type constructor](/img/diagrams/kind-system003.svg)

## Type signatures and kind signatures

Just like GHC is usually able to correctly infer the types of variables, it is also usually able
to correctly infer the kinds of type variables.

```hs
-- The inferred type of `x` is `Bool`
x = True

-- The inferred type of `y` is `String -> IO ()`
y = putStrLn
```

```hs
-- The inferred kind of `a` is `*`
data List a = Cons a (List a) | Nil

-- The inferred kind of `f` is `* -> *`
-- The inferred kind of `a` and `b` is `*`
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
```

And just like you can manually specify a variable's type, you can also manually specify a type variable's kind
using the `KindSignatures` extension.

```hs
x :: Bool
x = True

y :: String -> IO ()
y = putStrLn
```

```hs
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

data List (a :: *) = Cons a (List a) | Nil

class Functor (f :: * -> *) where
  fmap :: forall (a :: *) (b :: *). (a -> b) -> (f a -> f b)
```

The `ExplicitForAll` extension allows us to define each type variable explicitly, which I did here for clarity.

## HOFs and HKTs

Just like we can have higher-order functions (*HOFs*), functions that take other functions as arguments,
we can also have *higher-kinded types* (*HKTs*), types constructors that take other type constructors as arguments.

```hs
λ> :t map
map :: (a -> b) -> [a] -> [b]
```

Here, `map` is a function that takes another function of type `(a -> b)` and a list of type `[a]`.

```hs
λ> data NonEmpty f a = MkNonEmpty { head :: a, tail :: f a }

λ> :k NonEmpty
NonEmpty :: (* -> *) -> * -> *
```

Similarly, `NonEmpty` is a type constructor that takes another type constructor of kind `(* -> *)` and a type of kind `*`.

When applied to `[]` and `Bool`, we obtain the type `NonEmpty [] Bool`, a list of boolean values that is
guaranteed to have at least one value.

```hs
λ> :t MkNonEmpty True [False, True]
MkNonEmpty True [False, True] :: NonEmpty [] Bool
```

We can apply this type constructor to *any* two types, so long as their kinds match the expected kinds, e.g.:
`NonEmpty [] Int`, `NonEmpty Tree String` or `NonEmpty Vector Char`.

## Other kinds

### Unboxed/unlifted types

Remember when I said all inhabited types are of kind `*`?
Allow me to rephrase that: in standard Haskell, `*` is the kind of all inhabited *boxed* (or *lifted*) types.
However, in GHC's version of Haskell, there are also some inhabited types that don't fall under this umbrella: *unboxed* (or *unlifted*) types.

These are defined in the `GHC.Prim` module from the `ghc-prim` package.
By convention, all unlifted types end with a `#`, called the *magic hash*, enabled by the `MagicHash` extension.
Examples include `Char#` and `Int#`.
You can even have unboxed tuples `(# a, b #)` and unboxed sums `(# a | b #)`!

Each unlifted type has a kind that describes its runtime representation.
Is this a pointer to something in the heap?
Is it a signed/unsigned word-sized value?
The compiler then uses that type's kind to decide which machine code it needs to produce - this is called "kind-directed compilation".

Here are some examples:

{:.tex}
![Unboxed types and kinds](/img/diagrams/kind-system007.svg)

We'll come back to this `TYPE` thing later on.

### Constraints

There's also the `Constraint` kind that covers everything that can appear to the left of an `=>` arrow, including
typeclass constraints:

```hs
λ> :k Show
Show :: * -> Constraint

λ> :k Show Int
Show Int :: Constraint

λ> :k Functor
Functor :: (* -> *) -> Constraint

> :k Functor IO
Functor IO :: Constraint
```

With the `ConstraintKinds` extension, we can use constraints as first-class citizens, and that's amazingly useful.
If you'll recall, `Set` famously does not have an instance for the `Functor` typeclass.
The reason being `fmap :: (a -> b) -> f a -> f b` must work for all types `a` and `b`, but `Set.map` only works for "orderable" types,
i.e. it has an `Ord` constraint.

```hs
λ> :t Data.Set.map
Data.Set.map :: Ord b => (a -> b) -> Set a -> Set b
```

With `ConstraintKinds`, we can write a more generic `Functor` typeclass that abstracts over any constraint that may or may not exist.

```hs
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Kind (Constraint)
import qualified Data.Set as Set
import qualified Data.List as List

class GFunctor (c :: * -> Constraint) (f :: * -> *) | f -> c where
  gfmap :: c b => (a -> b) -> (f a -> f b)

instance GFunctor Ord Set.Set where
  gfmap = Set.map

instance GFunctor EmptyConstraint [] where
  gfmap = List.map
```

Where `EmptyConstraint` is a typeclass constraint that's trivially satisfied by all types.

```hs
{-# LANGUAGE FlexibleInstances #-}

class EmptyConstraint a
instance EmptyConstraint a
```

### Datatype promotion

In standard Haskell, the `data` keyword lets us define a custom type/type constructor, followed by a set of data constructors:

```hs
data ConnectionStatus = Open | Closed
```

{:.tex}
![ConnectionStatus type](/img/diagrams/kind-system004.svg)

But when GHC's `DataKinds` extension is enabled, the `data` keyword creates two additional things: a custom *kind*, followed by a set of types/type constructors.

```hs
{-# LANGUAGE DataKinds #-}

data ConnectionStatus = Open | Closed
```

In the code above, a new kind `ConnectionStatus` is created. This kind has exactly two *uninhabited* types, `'Open` and `'Closed`.

{:.tex}
![ConnectionStatus kind](/img/diagrams/kind-system005.svg)

We say the type `ConnectionStatus` has been *promoted* to a kind, and `Open` and `Closed` to types.
Notice how the promoted types `'Open` and `'Closed` are prefixed with a tick.
This tick can almost always be omitted, and only in rare scenarios will you need it for [disambiguation][datakinds-disambiguation].

With our new custom kind, we can define a type variable that can only ever be instantiated to `Open` or `Closed`, and nothing else.

```hs
{-# LANGUAGE KindSignatures #-}

data Connection (s :: ConnectionStatus) = MkConnection ...

newConnection     :: Address           -> Connection Closed
openConnection    :: Connection Closed -> Connection Open
closeConnection   :: Connection Open   -> Connection Closed
connectionAddress :: Connection s      -> Address
```

```hs
λ> :k Connection Int
<interactive>:1:12: error:
    • Expected kind ‘ConnectionStatus’, but ‘Int’ has kind ‘*’
```

By tagging the connection with its status, we can statically enforce rules such as "`closeConnection` cannot be called on an already closed connection".

This doesn't work just for our own custom data types though.
In the presence of `DataKinds`, `Bool`, for example, gets promoted to a kind as well.

```hs
λ> data F (b :: Bool) = MkF deriving Show

λ> MkF :: F 'True
MkF

λ> MkF :: F 'False
MkF

λ> MkF :: F 'Open
<interactive>:30:10: error:
    • Expected kind ‘Bool’, but ‘ 'Open’ has kind ‘ConnectionStatus’
```

### GHC.TypeLits

GHC provides two other very convenient kinds out of the box, hidden in the `GHC.TypeLits` module in *base*.

The first is `Symbol`, which is the kind for type-level strings.
It lets us use string literals like `"hello"` as a type.

{:.tex}
![Symbol kind](/img/diagrams/kind-system006.svg)

With it, we can *tag* a type with a string literal.

```hs
{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol)
import Data.Ratio ((%))

newtype Money (currency :: Symbol) = Money Rational

fivePence :: Money "GBP"
fivePence = Money (5 % 100)

twoEuros :: Money "EUR"
twoEuros = Money 2
```

Having the currency represented at the type-level, rather than at the term-level (like in `data Money = Money String Rational`), lets us statically ensure that monies of different currencies don't get mixed up, e.g. we can't add EUR and GBP together.

```hs
add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)
```

```hs
λ> add fivePence fivePence
Money (1 % 10)

λ> add fivePence twoEuros
<interactive>:8:15: error:
    • Couldn't match type ‘"EUR"’ with ‘"GBP"’
```

If we had the currency at the term-level instead, we'd have to do runtime checks and every function would have to signal the possibility of failure by returning `Maybe`, `Either`, `MonadError e m`, etc.

Little interesting fact: this is more than just a contrived example.
This is *exactly* how the *safe-money* library represents [dense monetary values][dense]!

When we don't need to perform arithmetic operations on money, storing the value as a `Rational` is a bit overkill.
For these scenarios, the library provides a different type, [`Discrete`][discrete], which is a simple wrapper around an `Integer`.

Here's the catch though: for reasons explained in *safe-money*'s [introductory blog post][scales], when representing monetary values as integers, keeping track of the currency is not enough.
We need to keep track of the scale as well.
When dealing with whole US dollars, the scale would be 1 to 1.
When dealing with cents, the scale would be 100 to 1 (because there are 100 cents in a dollar).

In order to represent the scale as a type, we need `Nat`, the kind for type-level natural numbers, also exported from `GHC.TypeLits`.
Similar to `Symbol`, it lets us use numeric literals as types.

```hs
{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol, Nat)

newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat))
  = Discrete Integer

oneDollar :: Discrete "USD" '(1, 1)
oneDollar = Discrete 1

oneDollarThirtyCents :: Discrete "USD" '(100, 1)
oneDollarThirtyCents = Discrete 130
```

Yes, you saw that right.
In `scale :: (Nat, Nat)`, `(,)` is the tuple type promoted to a kind via `DataKinds`.
And in `'(100, 1)`, `'(,)` is the tuple data constructor promoted to a type constructor.

## Kind polymorphism

Parametric polymorphism is ubiquitous in Haskell, it helps us abstract over types.
With the `PolyKinds` extension, we can go even higher and abstract over kinds as well!

Let's look at an example.

```hs
data Proxy a = MkProxy
```

Here, `Proxy` contains a type variable `a`.
It has a single data constructor with no arguments, which we can then *tag* with a type variable.
`Proxy` is very useful for passing around types when we have no values at hand[^data-proxy].

```hs
λ> intRepresentative = MkProxy :: Proxy Int

λ> stringRepresentative = MkProxy :: Proxy String
```

The problem is, by default, GHC will assume `a` is of kind `*`,
which means `Proxy` doesn't *reeaally* work for all types - just lifted ones.

```hs
λ> :k Proxy
Proxy :: * -> *

λ> maybeRepresentative = MkProxy :: Proxy Maybe
<interactive>:9:38: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
```

Normally, we'd have to create a bunch of `Proxy`-like types for every possible kind, like this:

```hs
data Proxy a = MkProxy
data Proxy1 (a :: * -> *) = MkProxy1
data Proxy2 (a :: * -> * -> *) = MkProxy2
data HKProxy1 (a :: (* -> *) -> * -> *) = MkHKProxy1

stringRepresentative   = MkProxy    :: Proxy String
maybeRepresentative    = MkProxy1   :: Proxy1 Maybe
eitherRepresentative   = MkProxy2   :: Proxy2 Either
nonEmptyRepresentative = MkHKProxy1 :: HKProxy1 NonEmpty
```

This is obviously unsustainable.
But with the `PolyKinds` extension enabled, we can create a `Proxy` that works for all types `a` and for all kinds `k`.

```hs
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

data Proxy (a :: k) = MkProxy

-- Now `a` can be anything at all
maybeRepresentative    = MkProxy :: Proxy Maybe
nonEmptyRepresentative = MkProxy :: Proxy NonEmpty
functionRepresentative = MkProxy :: Proxy (->)
helloRepresentative    = MkProxy :: Proxy "hello"
showRepresentative     = MkProxy :: Proxy Show
functorRepresentative  = MkProxy :: Proxy Functor
openRepresentative     = MkProxy :: Proxy 'Open
```

In fact, we could simply omit the kind signature altogether and GHC would still infer `a :: k`.

```hs
λ> :set -XPolyKinds
λ> data Proxy a = MkProxy

λ> :k Proxy
Proxy :: k -> *
```

Another instance of kind polymorphism being used in the wild can be found in Servant, a library for writing type-safe web APIs.
It has a type named `:>` that's used to combine components of an API into a complete description of an endpoint.
These components can vary in type and in kind.

```hs
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

data (:>) (a :: k) (b :: *)
infixr 4 :>
```

This is how you'd describe an endpoint that responds to `POST /books`.
We use `:>` to combine the types `"books" :: Symbol`, `ReqBody '[JSON] Book :: *` and `Post '[JSON] () :: *`.

```hs
type BooksAPI = "books" :> ReqBody '[JSON] Book :> Post '[JSON] ()
```

## Levity polymorphism

In some cases, it may be useful to abstract over lifted *and* unlifted types.
For example, consider the `error` "function", which takes an error message and throws an exception:

```hs
{-# LANGUAGE ExplicitForAll #-}

error :: forall a. String -> a
```

`error` is polymorphic in its return type, so it can be used (almost) anywhere.

```hs
increment :: Int -> Int
increment x = error "oops"
```

But because `a` is (by default) assumed to be a type of kind `*`, we can't use it in a place where an unlifted type is expected!

```hs
incrementUnlifted :: Int# -> Int#
incrementUnlifted x = error "oops"
```

```hs
<interactive>:22-40: error
   • Couldn't match a lifted type with an unlifted type
```

Armed with our new knowledge, our first attempt might be to turn on `PolyKinds` and introduce a kind variable `k`, but that won't work either.

```hs
error :: forall k (a :: k). String -> a
```

```hs
/Playground.hs:15:40: error:
    • Expected a type, but ‘a’ has kind ‘k’
    • In the type signature: error :: forall k (a :: k). String -> a
```

The reason this doesn't compile is that it doesn't make sense for `a :: k` to be anything other than an inhabited type.
Say `k` was `Symbol` and `a` was `"hello"`.
If the `"hello"` type is uninhabited, what value could such a function possibly return?

To make the `error` function work for all inhabited types, both lifted and unlifted, we need something called *levity polymorphism*.

The trick resides in the kind [`TYPE r`][type-r] we saw earlier.
This kind is parameterised over `r :: RuntimeRep`, which describes a type's runtime representation and can be one of the following:

```hs
data RuntimeRep = VecRep VecCount VecElem   -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]     -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]       -- ^ An unboxed sum of the given reps
                | LiftedRep       -- ^ lifted; represented by a pointer
                | UnliftedRep     -- ^ unlifted; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                | WordRep         -- ^ unsigned, word-sized value
                | Int64Rep        -- ^ signed, 64-bit value (on 32-bit only)
                | Word64Rep       -- ^ unsigned, 64-bit value (on 32-bit only)
                | AddrRep         -- ^ A pointer, but /not/ to a Haskell value
                | FloatRep        -- ^ a 32-bit floating point number
                | DoubleRep       -- ^ a 64-bit floating point number
```

We already saw how `TYPE 'IntRep` is the kind of unlifted integers, `TYPE 'FloatRep` is the kind of unlifted floats, etc.
But there's something here that stands out: `TYPE 'LiftedRep`. This is the kind for all lifted types -
in fact, the `*` kind we've been using all along is nothing more than [a synonym for `TYPE 'LiftedRep`][type-liftedrep-synonym]!

So it turns out we can use `TYPE r` not only to abstract over all unlifted types, but also over lifted ones.

```hs
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

import GHC.Exts (TYPE, RuntimeRep)

error :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
```

And now we can use `error` in both `increment` and `incrementUnlifted`.

Levity polymorphism has its restrictions - there are some places where it can't be used.
To learn where and why, watch Richard Eisenberg's [amazing talk][levity] on the subject.

## Wrap-up

By now you might be reasonably wondering: what's the point of all this? Why have a kind system, why even care about kinds?
Well, in languages where types cannot be classified at all, like Java or C#, *all* type variables `<T>` are of kind `*`.
You can't have a type variable of kind `* -> *`, like in `Functor<List>`.
Without being able to talk about kinds, we can't define abstractions like functors and monads, or even the `NonEmpty f a` data type we saw earlier.
Furthermore, with levity polymorphism, we can write abstractions that work with both boxed *and* unboxed types, like [this generic version of the `Num a` typeclass][levnum],
as opposed to Java where generic type variables can only ever be boxed types.

Having a kind system also opens the door to a world of type-level programming (which I hope to get more into in my next blog post),
enabling things like
extensible records,
Servant's type-level web APIs,
and just safer APIs in general, like `Connection s` or `Money c`.

Exploring the frontier of research, Simon Peyton Jones recently [discussed adding linearity to Haskell][linearity], which I imagine would mean
the function type `(->) a b` would have an additional type variable `l` of a new kind `Linearity`, composed of the types `Omega` and `One`.

Another question you might be asking is, if terms are classified into types, and types into kinds, are kinds classified into something else?
Well, in GHC 7 and earlier, kinds were classified into *sorts*.
All kinds had [the unique sort `BOX`][box],
which was internal to GHC and invisible to us, the developers.

But in version 8, GHC went a different way.
As you might have noticed, in this article I tried to expose the similarities between types and kinds: both can be higher-order, polymorphic, inferred, curried, etc.
This is not a coincidence!
With the advent of `TypeInType`, an extension introduced in GHC 8, types and kinds (and sorts) became one and the same!
Types can now be classified by other types. `3` is of type `Int`, `Int` is of type `*`, and `*` is of type `*`.
This unification is meant to pave the way for full dependent types in Haskell[^dep-types].

Also, in more recent literature, you might see the kind `*` being referred to as `Type` (do not confuse with `TYPE r`).
These are [synonyms][star-synonym] for now, and the plan is to gradually phase out `*` in favour of `Type`.

 [^void]:
    A notable exception is `data Void`, which cannot be constructed.
    There are no values of this type *per se*, and it's only inhabited by [terms that never complete successfully][bottom], such as `undefined` or the infinite loop `f x = f x`.

 [^data-proxy]:
    `Proxy` is already defined in the `Data.Proxy` module in *base*, we're defining our own here to show the logic behind its kind.
    To read more about `Data.Proxy` and its use cases, check out [Kwang Seo's blog post][data-proxy].

 [^dep-types]:
    If you want to learn more about dependent types, I recommend reading [Type-Driven Development with Idris][idris-book] or the recently released [The Little Typer][the-little-typer].

 [bottom]: https://wiki.haskell.org/Bottom
 [datakinds-disambiguation]: https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#distinguishing-between-types-and-constructors
 [dense]: https://hackage.haskell.org/package/safe-money-0.7/docs/Money.html#t:Dense
 [discrete]: https://hackage.haskell.org/package/safe-money-0.7/docs/Money.html#t:Discrete-39-
 [scales]: https://ren.zone/articles/safe-money#scales
 [box]: https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/promotion.html
 [idris-book]: https://www.manning.com/books/type-driven-development-with-idris
 [the-little-typer]: https://mitpress.mit.edu/books/little-typer
 [type-r]: https://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-Exts.html#t:TYPE
 [type-liftedrep-synonym]: https://hackage.haskell.org/package/ghc-prim-0.5.2.0/docs/GHC-Types.html#t:-42-
 [star-synonym]: https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Kind.html#t:Type
 [data-proxy]: https://kseo.github.io/posts/2017-01-15-data-proxy.html
 [linearity]: https://www.youtube.com/watch?v=t0mhvd3-60Y
 [levity]: https://www.youtube.com/watch?v=lSJwXZ7vWBw
 [levnum]: https://gist.github.com/dcastro/a7f9730981fa404415588224350dc918
