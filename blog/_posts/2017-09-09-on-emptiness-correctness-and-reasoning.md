---
layout: post
title: On emptiness, correctness and reasoning
issueId: 1
---

<!-- markdownlint-disable MD002 -->

`Null`. Ever since its debut in Algol W back in 1965, most programming languages have adopted this concept of nullability by default.
That is, a variable of (pretty much) every type can be assigned this special `null` value that represents the absence of an actual value.

Since then, its own maker has coined it his [billion dollar mistake][billion dollar mistake].
Nowadays, it's common knowledge that `null` is a source of headaches, due to:

* Being able to travel silently through the code before [exploding][prob-bomb] in your face,
* [Subverting the type system][prob-subvert],
* Inducing mental burden for having to remember/find out/guess which functions may return `null`,
* Forcing you to [defensively null check every single argument][prob-aspnet] (especially when writing a library), and [test it][prob-aspnet-tests],
* Causing [ambiguity][prob-guava] when used to simultaneously express two things, such as `map.get(key)` returning `null` to express a missing entry in a map *or* an existing entry with no value.

It's even responsible for [a bug in Java's own type system][type system bug]!

> Well, amazingly enough, it turns out null pointers don’t just cause bugs in programs, they cause bugs in type systems too!
> [...] But unlike most null-pointer bugs, this one took 12 years to discover.
>
> \- Ross Tate

Today, I won't bother going into those issues which have already been thoroughly debated.
Instead, I'll take a step back and discuss why `null` inhibits correctness and hinders one's ability to reason about code.

## Correctness

The point of having a type system is to have it verify the correctness of our code, to have it proof that certain properties of our system hold.
In order to do that, we embed as many rules as possible in it, we tell it what is legal and what isn't.
Our job is not only to model a given domain and to make legal states representable but also to [make illegal states unrepresentable][illegal-states].

And we do that by carefully choosing and modelling our data types.

This is why you don't use a `double` to represent a person's age, or a string whose content is expected to be numeric - you use an integer. Or even better, a [natural number][nat type].

We might even use a non-empty list type (`NonEmptyList[T]` in Scalaz, `NonEmpty t` in Haskell, or `NonEmpty f t` in PureScript) to represent a list that is guaranteed to have at least one element, such as the set of stars that make up a constellation.
A function that takes a `NonEmptyList t` *cannot* be passed a possibly empty `List t` - we have made an illegal state unrepresentable!

<!-- markdownlint-disable MD033 -->
<blockquote class="twitter-tweet" data-lang="en-gb"><p lang="en" dir="ltr">&quot;Should I use String in this method signature?&quot;<br>&quot;Is the Mandarin edition of the complete works of Shakespeare valid input?&quot;</p>&mdash; kenbot (@KenScambler) <a href="https://twitter.com/KenScambler/status/621933432365432832">17 July 2015</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>
<!-- markdownlint-enable MD033 -->

Making every type nullable by default goes blatantly against this goal. It assumes the absence of a value is a valid state for all types, and it most definitely is not. A table *must* have a height, a file *must* have a filename, a product *must* have a price, and an alphabet *must* have letters.

Not only is that assumption wrong, but most languages don't even have a built-in way of opting out of nullability! In C#, barring value types, you can't declare something to be non-nullable. There's no non-nullable `string`!

This imposes a limit on how expressive we can be. There are strictly fewer things you can tell the compiler about the domain you're modelling.
Ergo, there are strictly fewer ways in which the compiler can help you not shoot yourself in the foot.

## Reasoning

Parametricity is a property that allows you to derive free theorems about a polymorphic function just by looking at its type.

> Write down the definition of a polymorphic function on a piece of paper. Tell me its type, but be careful not to let me see the function's definition. I will tell you a theorem that this function satisfies.
>
> \- Philip Wadler, June 1989 in "Theorems for Free"

Take a look at the following polymorphic function `f`, which takes a list of some generic type `t` and returns a list of the same type `t`:

```hs
f: List t -> List t
```

We don't know how it's implemented, and we don't care about its name. But from its type alone, we can derive the following theorem:

> Every element of type `t` in the output list must appear in the input list.

The reasoning is that `f` must work with lists of *any* type `t`. Since `f` knows nothing about `t`, it can only a) rearrange the elements in the input list, and/or b) remove elements from it. It can't create instances of `t` out of thin air.

Here's another theorem:

> For all types `A` and `B`, and every function `g: A -> B`{:.language-hs}, we have `map g (f list) = f (map g list)`{:.language-hs}

That is, applying `f` to a list and then transforming each of its elements using `map g`, must yield the same result as transforming each of its elements using `map g` and _then_ applying `f` to the transformed list. These two expressions are interchangeable, and we can refactor at will.

Let's test this out. Let:

* `f: List t -> List t`{:.language-hs} be `reverse`, a polymorphic function that reverses the order of the elements in a list
* `g: String -> Int`{:.language-hs} be `stringLength`, a function that returns the length of a string

The free theorem says that reversing a list and then getting the length of each string, is the exact same as getting the length of each string and then reversing the result.

<!-- $$
\begin{gather*}
    \text{["Diogo", "Tom", "Serg"]} & \xrightarrow{\text{map stringLength}} & [5, 3, 4] & \xrightarrow{\text{reverse}} & [4, 3, 5]\\
    \text{["Diogo", "Tom", "Serg"]} & \xrightarrow{\text{reverse}} & \text{["Serg", "Tom", "Diogo"]} & \xrightarrow{\text{map stringLength}} & [4, 3, 5] \\
\end{gather*}
$$ -->

```text
                         reverse                           map stringLength
["Diogo", "Tom", "Serg"] -------> ["Serg", "Tom", "Diogo"] ----------------> [4, 3, 5]
```

```text
                             map stringLength                   reverse
["Diogo", "Tom", "Serg"] ------------------------> [5, 3, 4] --------------> [4, 3, 5]
```

Hey presto!
And we know this to be true for *any* `f: List t -> List t`{:.language-hs} and `g: A -> B`{:.language-hs}!

The principle at play is this: the less a function knows about its arguments, the less it can do. The less it can do, the easier it is to reason about it. Parametricity gives you analytical power.

Give it a go. This function takes two arguments, a `List t` and a function `t -> Bool`{:.language-hs}, and returns another `List t`. Can you guess what it does?

```hs
someFunction: (List t, t -> Bool) -> List t
```

Admittedly, these examples are a bit simple, but parametricity really pays off when trying to understand functions you've never seen before and how they could interact with others.

However, we get into all sorts of problems when we try to subvert the type system with, e.g., reflection, type casting, and, of course, `null`.
Let's revisit our theorems above, but this time we'll throw `null` into the mix:

* `f` might now return the input list with a handful of `null`s at the end, or it might simply return `null`. Elements on the output list are not guaranteed to be in the input list anymore, if there's even an output list at all.
* Let:
  * `f` be `appendNull`, a function that appends `null` to the input list
  * `g` be `stringLength`, a function that returns the length of a string, or 0 if given `null`

```hs
map stringLength (appendNull ["Diogo", "Tom", "Serg"]) = [5, 3, 4, 0]
appendNull (map stringLength ["Diogo", "Tom", "Serg"]) = [5, 3, 4, null]
```

Our theorems no longer hold. The analytical power has gone out the window.

## There ought to be a better way

The solution is, of course, to lift this problem to the type level. If nullability becomes an explicit type/attribute of a type, then you can leverage the type system to enforce soundness.

The most common way of lifting nullability is to simply introduce a polymorphic type `Option[T]`/`Optional<T>`/`Maybe t`.
I won't go into too much detail here, because there are plenty of great resources about it out there already, but the gist of it is that an instance of, say, `Maybe String` can either be a `Just "hello"` (think of it as a box containing a value `"hello"`) or `Nothing` (an empty box).
Its interface forces you to unwrap the box to find out if there's something inside and to handle both cases, usually through pattern matching or a reduce/fold function. You can't handle just *one* case.

By capturing the notion of nullability as a proper type, we can now express precisely whether something is optional or not, and have the type checker ensure we use it **correctly**.
Furthermore, since `t` and `Maybe t` are now distinct types, if a function `f` were to add "empty boxes" to the input list, it would have to be typed as `f: List t -> List (Maybe t)`{:.language-hs}. And if it were to simply not return a list at all, it would be typed as `f: List t -> Maybe (List t)`{:.language-hs}.
Those functions would themselves generate their own sets of free theorems, and we'd be able to safely **reason** about them, just as we would about any other polymorphic function.

Many programming languages have adopted this approach: Haskell, Scala, PureScript, Elm, F#, OCaml, Rust, to name a few.
It's been added to Java in its eighth release ([with][java optional 1] · [questionable][java optional 2] · [success][java optional 3]), and is also available as a [third-party library][functional-java].
There's a very good [port for C#][option-csharp] as well, and I'm willing to bet you can find one for whatever your language of choice is.

Others have chosen a more *ad hoc* solution by adding special syntax to the language.
In Swift and Kotlin, types are not nullable by default, and nullable types are explicitly labelled with a question mark, e.g. `String?`.
There's also a [proposal][csharp proposal] to add something similar to C#'s reference types.

However, retrofitting non-nullability into a language has its thorns.
Yes, it'll let you write safer code from then on, but what about existing code?
Even though you *can* add syntax to a language or a new type to the standard library, changing the entire public surface of the existing standard library and third-party libraries to use that new syntax/type is a *major* breaking change.

Eric Lippert, former member of the C# language design team, [explained in great detail][retrofit-probs] the difficulties of retrofitting this feature, concluding:

> Non-nullability is the sort of thing you want baked into a type system from day one, not something you want to retrofit in 12 years later.
>
> \- Eric Lippert

 [retrofit-probs]: http://archive.fo/FkYfR
 [illegal-states]: https://vimeo.com/14313378
 [java optional 1]: https://gist.github.com/oxbowlakes/8d13fae255412e00c59ae6f536a84773
 [java optional 2]: https://developer.atlassian.com/blog/2015/08/optional-broken/
 [java optional 3]: https://stackoverflow.com/q/24547673/857807
 [csharp proposal]: https://github.com/dotnet/csharplang/blob/master/proposals/nullable-reference-types.md
 [type system bug]: https://hackernoon.com/java-is-unsound-28c84cb2b3f
 [billion dollar mistake]: https://en.wikipedia.org/wiki/Null_pointer#History
 [nat type]: https://www.stackage.org/haddock/lts-9.2/base-4.9.1.0/Numeric-Natural.html
 [option-csharp]: https://github.com/louthy/language-ext
 [functional-java]: http://www.functionaljava.org/

 [prob-subvert]: https://www.lucidchart.com/techblog/2015/08/31/the-worst-mistake-of-computer-science/
 [prob-bomb]: https://softwareengineering.stackexchange.com/a/12785/101308
 [prob-aspnet]: https://github.com/aspnet/AspNetWebStack/blob/v3.2.3/src/System.Web.Http/Controllers/HttpControllerContext.cs#L23
 [prob-aspnet-tests]: https://github.com/aspnet/AspNetWebStack/blob/v3.2.3/test/System.Web.Http.Test/Controllers/HttpControllerContextTest.cs#L40
 [prob-guava]: https://github.com/google/guava/wiki/UsingAndAvoidingNullExplained
