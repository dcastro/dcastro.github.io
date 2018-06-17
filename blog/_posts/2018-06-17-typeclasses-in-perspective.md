---
layout: post
title: Typeclasses in perspective
issueId: 2
---

<!-- markdownlint-disable MD002 -->

Typeclasses are typically taught by drawing analogies between them and method overloading (another form of ad hoc polymorphism),
and Java-like interfaces (a form of subtyping polymorphism).

Even though typeclasses are its own concept, it's only natural to want to compare them to other familiar constructs.
This can lead to a lot of confusion, and the line that separates these things can be blurry.

So in this post I'll try to focus on their differences and show that their similarities are only superficial.
<!--more-->
These are the points I'll be covering:

- [Extensibility](#extensibility)
- [Conditional implementation](#conditional-implementation)
- [Return-type polymorphism](#return-type-polymorphism)
- [Relations between types](#relations-between-types)
- [Dispatch](#dispatch)

Code samples are included in Haskell and Scala (using the [Ammonite repl][amm]).

NB: this post assumes the reader is already at least vaguely familiar with what typeclasses are.
If not, [here][haskell-typeclasses] (Haskell) and [here][scala-typeclasses] (Scala) are great starting points.

## Extensibility

Say we're trying to abstract over all the types that can be JSON encoded.

In Java or C#, one might reasonably start by defining an interface like this:

```java
interface Encodable {
  Json toJson();
}
```

And implementing it for a class `Person` is quite straightforward:

```java
class Person implements Encodable {
  public String getName() { ... }
  public Integer getAge() { ... }

  public Json toJson() { ... }
}
```

So now all we have to do is make every encodable type extend our interface.
But that's not always possible - we can't make types that we don't control extend our interface.
`Integer`, for example, is defined in the standard library, we can't just go and change it!

Typeclasses, on the other hand, give us the freedom to add this behaviour to types outside our control.

```hs
class Encodable a where
  toJson :: a -> Json

instance Encodable Int where
  toJson i = _
```

```scala
trait Encodable[A] {
  def toJson(a: A): Json
}

object Encodable {
  implicit val encodableInt = new Encodable[Int] {
      def toJson(i: Int) = ???
    }
}
```

## Conditional implementation

Let's keep going with our example. Say we now want to serialize lists.
We know we can't change the built-in `List<A>` type, but let's pretend we can for a second.

```java
class List<A> implements Encodable {
  public Json toJson() { ... }
}
```

Turns out, it's impossible to implement the `toJson` method for `List<A>`, because not all lists are encodable.
We can only encode lists of other encodable things, like `List<Person>`, `List<String>` or `List<Boolean>`.
But serializing a `List<DbConnection>`, or a `List<Function<String, String>>` is nonsense!

What we want to express is this: a `List<A>` is encodable so long as `A` is encodable.
And a `Pair<A, B>` is encodable so long as `A` and `B` are encodable.
But the Java type system is not expressive enough, it doesn't let us express this.

Typeclasses make this trivial.

```hs
instance Encodable a => Encodable [a] where
  toJson list = _

instance (Encodable a, Encodable b) => Encodable (a, b) where
  toJson (a, b) = _
```

```scala
implicit def encodableList[A : Encodable] =
  new Encodable[List[A]] {
    def toJson(list: List[A]) = ???
  }

implicit def encodablePair[A : Encodable, B : Encodable] =
  new Encodable[(A, B)] {
    def toJson(pair: (A, B)) = ???
  }
```

## Return-type polymorphism

Clearly, subtyping polymorphism just won't do in this instance.
So let's try ad hoc polymorphism instead.
With it, we can write functions that work on *some* types, and behave slightly differently for each one.

Most languages implement a simple form of ad hoc polymorphism generally called function/method/operator overloading.

```java
Json toJson(String s) { ... }
Json toJson(Integer i) { ... }

<A>    Function<List<A>, Json>    toJson(Function<A, Json> fa) { ... }
<A, B> Function<Pair<A, B>, Json> toJson(Function<A, Json> fa, Function<B, Json> fb) { ... }
```

This seems to work well enough! We can now convert built-in types to json, and we can conditionally convert lists to json if and only if their elements are also convertible.
We do have to bear the burden of:

- explicitly composing `toJson`s together (e.g. `toJson(this::toJson, this::toJson)`),
- explicitly passing these functions around whenever we need to abstract over "encodable things".

We can sorta kinda maybe live with it, as long as we keep our type-fu to a minimum.
Dealing with even slightly more complex types, like lists of pairs, will *not* be fun.

But can we do the same with `fromJson`, a function that parses a `Json`?
Let's give it a go.

```java
Optional<String> fromJson(Json s) { ... }
Optional<Integer> fromJson(Json s) { ... }
```

Sadly, this doesn't compile.
Java and most C-like languages lack what's called *return-type polymorphism*.
Overloads can have different parameters, but they cannot differ (solely) in their return types.

With typeclasses, all we have to do is take `Encodable` and flip the arrow:

```hs
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)

class Decodable a where
  fromJson :: Json -> Maybe a

instance Decodable Int where
  fromJson json = Just 5

instance Decodable Text where
  fromJson json = Just "hello"
```

```scala
trait Decodable[A] {
  def fromJson(j: Json): Option[A]
}

object Decodable {
  def fromJson[A](j: Json)(implicit A: Decodable[A]) =
    A.fromJson(j)

  implicit val decodableInt =
    new Decodable[Int] {
      def fromJson(j: Json) = Some(5)
    }

  implicit val decodableString =
    new Decodable[String] {
      def fromJson(j: Json) = Some("hello")
    }
}
```

Type inference will in most cases be able to disambiguate between overloads (more so in Haskell than in Scala),
but on the off-chance it can't, there's nothing a little type annotation can't fix.

```hs
λ> fromJson json :: Maybe Int
Just 5

λ> fromJson json :: Maybe Text
Just "hello"
```

```scala
@ import Decodable._

@ fromJson[Int](json)
res0: Some[Int] = Some(5)

@ fromJson[String](json)
res1: Some[String] = Some("hello")
```

Another issue with using method overloading as a means of abstraction
is that it quickly falls over when our abstraction has more than 1 method.

As an exercise, try to think how `Decodable` would look like if defined as a Java-like interface.

## Relations between types

Both typeclasses we've looked at so far have one type parameter, `a`.
In these examples, typeclasses are used to *describe that type*: `a` can be turned into `Json`, or `a` can be obtained from a `Json`.

However, typeclasses can have multiple type parameters.
These not only describe types, they describe *relations between types*.

We can, for example, define a typeclass that describes two types `a` and `b`, such that any value
of type `a` can be safely, losslessly converted to a value of type `b`.

```hs
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Int

class Cast a b where
  cast :: a -> b

instance Cast Int Int64 where
  cast x = fromIntegral x

instance Cast a b => Cast [a] [b] where
  cast xs = map cast xs
```

```scala
trait Cast[A, B] {
  def cast(a: A): B
}

object Cast {
  implicit val castIntLong = new Cast[Int, Long] {
    def cast(x: Int) = x.toLong
  }

  implicit def castList[A, B](implicit AB: Cast[A, B]) =
    new Cast[List[A], List[B]] {
      def cast(xs: List[A]) = xs.map(AB.cast)
    }
}
```

A classic example is a typeclass that describes how two things can be multiplied to produce a third thing
(where all these 3 things can be of different types):

```hs
class Mult a b c where
  mult :: a -> b -> c

instance Mult Int Int Int where mult x y = x * y

instance Mult Euros Int Euros where {- ... -}
instance Mult Int Euros Euros where {- ... -}

instance Mult Metres Metres SquareMetres where {- ... -}

instance Mult MilesPerHour Hours Miles where {- ... -}

instance Mult Matrix Matrix Matrix where {- ... -}
instance Mult Matrix Vector Vector where {- ... -}
instance Mult Matrix Int    Matrix where {- ... -}
```

With Haskell's [functional dependencies][fundeps], we can further refine `Mult` by stating that the result type `c` is uniquely determined
from the operand types `a` and `b`. From the Haskell wiki:

> When you know the types of the things that you're multiplying, the result type should be determined from that information alone.

```haskell
{-# LANGUAGE FunctionalDependencies #-}

class Mult a b c | a b -> c where
  mult :: a -> b -> c
```

This greatly improves type inference.
In Scala, this can be emulated by making `C` an abstract type member instead.

```scala
trait Mult[A, B] {
  type C
  def mult(a: A, b: B): C
}
```

## Dispatch

Lastly, there's one more glaring difference between interfaces and typeclasses that must be pointed out.

Interfaces, and subtyping polymorphism in general, rely on dynamic dispatch.
This means that the process of selecting which method to call happens at runtime, based on information available only at runtime.

Typeclasses rely on static dispatch. Which methods/functions get called is fully resolved at compile-time.

Why does this matter? Let's try implementing a function that increments every element of a collection, using the `Seq[A]` Scala "interface".

```scala
def addOne(xs: Seq[Int]): Seq[Int] = xs.map(_ + 1)

val result: Seq[Int] = addOne(List(1,2,3))
```

Now here's a different, equally valid, implementation and invocation of the same function:

```scala
def addOne(xs: Seq[Int]): Seq[Int] =
  if (Random.nextBoolean) xs.map(_ + 1).to[Vector]
  else                    xs.map(_ + 1).to[Queue]

val result: Seq[Int] = addOne( if (Random.nextBoolean) List(1,2,3) else Stream(1,2,3) )
```

Notice how we can't really tell which `map` method is going to be called. It could be `List.map`, `Stream.map`, etc.
This will have to be figured out at runtime - hence, dynamic dispatch.

Two things worth highlighting:

- the caller doesn't know what exactly he's getting back. It could be a vector, or a queue, an array, etc.
- the callee decides what to return at runtime.

Now let's redefine `addOne`, but this time with the [`Functor` typeclass][cats-functor] instead:

```hs
addOne :: Functor f => f Int -> f Int
addOne xs = fmap (+1) xs

λ> addOne [1,2,3]
[2,3,4]
```

```scala
@ import $ivy.`org.typelevel::cats-core:1.1.0`, cats._, cats.implicits._

@ def addOne[F[_]: Functor](xs: F[Int]): F[Int] = xs.map(_ + 1)

@ val result = addOne(List(1, 2, 3))
result: List[Int] = List(2, 3, 4)
```

This time, the tables have turned:

- the caller decides and knows exactly what he's getting back at compile-time,
- the callee doesn't get a say in what's returned.

This is because the type parameter `f`/`F` must be bound to a specific functor *at compile-time*.
In this example, we implicitly bound `f`/`F` to `[]`/`List` - everything's been fully resolved, there's nothing left to decide at runtime.

The key takeaway here is this: with dynamic dispatch, we're able to do more things at runtime,
but there's less we can say about our program's behaviour at compile-time.

Flexibility comes at the expense of analytical power. [Constraints liberate, liberties constrain][runar].

## Credits

Thanks to [George Wilson][george-wilson] for his amazing [talk][george-wilson-talk] which mentions a couple of the points
I've reiterated here, and got me thinking what other differences there were between typeclasses and other forms of polymorphism.

 [haskell-typeclasses]: http://book.realworldhaskell.org/read/using-typeclasses.html
 [scala-typeclasses]: https://leanpub.com/fpmortals/read#leanpub-auto-polymorphic-functions
 [fundeps]: https://wiki.haskell.org/Functional_dependencies
 [amm]: http://ammonite.io/
 [cats-functor]: https://typelevel.org/cats/typeclasses/functor.html
 [george-wilson]: https://twitter.com/GeorgeTalksCode
 [george-wilson-talk]: https://www.youtube.com/watch?v=2EdQFCP5mZ8
 [runar]: https://www.youtube.com/watch?v=GqmsQeSzMdw
