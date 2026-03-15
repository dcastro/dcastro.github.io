---
layout: post
title:  The hidden perils of MonadBaseControl
issueId: 17
---

\begin{code}%hidden
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumDecimals #-}

module Mbc where

import Control.Monad.State (StateT, modify, get, runStateT, put, MonadState)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith, restoreM, StM, control)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX qualified as Time
import Control.Concurrent.Async qualified as Async
import Control.Exception (bracket)
import Control.Exception.Lifted qualified as Lifted
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Catch qualified as Exceptions
import Control.Monad.Base (liftBase)

-- $setup
-- >>> import Control.Monad.State
-- >>> import Control.Monad.Except
-- >>> import Control.Concurrent (threadDelay)
-- >>> :set -XGHC2024 -XNumDecimals
\end{code}

[`MonadBaseControl`][mbc] is notoriously tricky to use correctly.
It's really easy to misuse and end up introducing subtle unexpected behaviour or downright bugs, even in the hands of the more experienced developers.

The goal of this article is to establish a clear mental model of how to work with `MonadBaseControl`, recognize its dangers, and how to avoid them.

Lastly, I'll leave you with recommendations for best practices, when to reach out for `MonadBaseControl`, and when not to.

  <!--more-->

This post assumes basic familiarity with `MonadBaseControl`.
If you've never used it, I wholeheartedly recommend reading Alexis King's [Demystifying MonadBaseControl][alexis-mbc] first
and then coming back to this article.
I will be reiterating some of the pitfalls mentioned in her article, expanding upon those, and diving into others.

Note: this page is available as a [Literate Haskell file][lhs-src].
Refer to that module to find the GHC extensions and imports used throughout the article.

Before we begin, let's define a couple of helper functions to make our examples easier to read:

\begin{code}
-- | Append a value to the state.
appendToState :: MonadState [a] m => a -> m ()
appendToState a =
  -- For the purpose of this article, excuse the inefficiency.
  -- A `DList` or `Seq` would be more appropriate.
  modify (<> [a])

-- | Print the current state with a label for context.
printState :: Show s => String -> StateT s IO ()
printState context = do
  st <- get
  liftIO $ putStrLn $ "State observed from '" <> context <> "': " <> show st
\end{code}

A quick refresher
---

Say you had this function; it takes an `IO` action as input and returns another `IO` action.

\begin{code}
foo :: forall a. IO a -> IO a
\end{code}

\begin{code}%hidden
foo = undefined
\end{code}

If we wanted to call `foo` with an action of type `StateT s IO a`, we could "lift" it like this:

\begin{code}
fooState :: forall a s. StateT s IO a -> StateT s IO a
fooState stateAction = do
  inputState <- get
  let ioAction = runStateT stateAction inputState :: IO (a, s)
  (a, outputState) <- liftBase $ foo @(a, s) ioAction
  put outputState
  pure a
\end{code}

That is, we need to:

1. Use `get` to capture the input state.
2. Run the `StateT s IO a` action with the input state, yielding an `IO (a, s)` action.
3. Call `foo` with the `IO (a, s)` action.
4. Restore the output state with `put`.

Observe how `foo`'s type parameter is instantiated to `@(a, s)`, materializing as `foo :: IO (a, s) -> IO (a, s)`.
In essence, we're _threading_ the state through `foo`, and then restoring it afterwards.

`MonadBaseControl` abstracts over this pattern and provides a general-purpose way of lifting functions like `foo` into some transformer stack.

\begin{code}
foo' :: forall m a. (MonadBaseControl IO m) => m a -> m a
foo' action = do
  st <- liftBaseWith \(runInBase :: m a -> IO (StM m a)) -> do
    let ioAction = runInBase action :: IO (StM m a)
    foo @(StM m a) ioAction
  restoreM st
\end{code}

Notice the parallels between this and the `StateT` version:

1. Use `liftBaseWith` to capture the input state; this gives us `runInBase`, a closure over that state.
2. `runInBase` will run an `m a` action with the input state, yielding an `IO (StM m a)` action.
3. Call `foo` with the `IO (StM m a)` action [^1].
4. Restore the output state with `restoreM`.

Again, we're instantiating `foo` as `foo :: IO (StM m a) -> IO (StM m a)`, allowing the state to be threaded through.

The `monad-control` package gives us the machinery to do this, and the packages
`lifted-base` and `lifted-async` build on top of it to give us the lifted version of commonly used functions
from the `base` and `async` packages, respectively.


Discarded state
---

To start off simple, let's take a look at this function:

\begin{code}
whenJust_ :: Maybe b -> (b -> IO a) -> IO ()
whenJust_ Nothing _ = pure ()
whenJust_ (Just x) f = void $ f x
\end{code}

A naive first attempt at lifting it might look like this:

\begin{code}
whenJust_' :: forall m a b. (MonadBaseControl IO m) => Maybe b -> (b -> m a) -> m ()
whenJust_' mb f = do
  liftBaseWith \runInBase ->
    whenJust_ mb (runInBase . f)
\end{code}

This typechecks, but it won't work as expected.
Remember: lifting a function with `MonadBaseControl` implies threading the state through it, getting the output state back, and then restoring it.

However, even though `whenJust_` does take a polymorphic action `IO a`, it unfortunately returns `IO ()`.
This means we simply can't get our output state back, which means we can't possibly call `restoreM`!

As a result, all state modifications will be discarded:

\begin{code}
-- | >>> execStateT testWhenJust1 [0]
-- [0]
testWhenJust1 :: StateT [Int] IO ()
testWhenJust1 = do
  whenJust_' (Just 1) \x -> do
    appendToState x
\end{code}

The only way to preserve state modifications is if `whenJust_` returns the `a` produced by the input action `IO a`.

In most situations, you can modify the function being lifted, or reimplement it, to allow the state to flow through.
Luckily, in this instance, that's fairly easy to fix:

\begin{code}
whenJust :: Maybe b -> (b -> IO a) -> IO (Maybe a)
whenJust Nothing _ = pure Nothing
whenJust (Just x) f = Just <$> f x

whenJust_'' :: forall m a b. (MonadBaseControl IO m) => Maybe b -> (b -> m a) -> m ()
whenJust_'' mb f = do
  stMaybe :: Maybe (StM m a) <- liftBaseWith \runInBase ->
    whenJust mb (runInBase . f)
  case stMaybe of
    Just st -> do
      _ :: a <- restoreM st
      pure ()
    Nothing ->
      pure ()
\end{code}

Unlike `whenJust_`, `whenJust` does allow `a` to flow through.
The input action produces an `a`, which is then wrapped in a `Maybe` and returned.
We can now capture the output state when `Just` is returned, and restore it.

\begin{code}
-- | >>> execStateT testWhenJust2 [0]
-- [0,1]
testWhenJust2 :: StateT [Int] IO ()
testWhenJust2 = do
  whenJust_'' (Just 1) \x -> do
    appendToState x
\end{code}

Threading state
---

While lifting a function with 1 input action is usually straightforward, lifting a function with 2 or more can get really thorny.
Let's have a look at a more nuanced (and rather contrived) example and try lifting `logDuration`:

\begin{code}
-- | >>> logDuration (threadDelay 1_e6) (\d -> putStrLn $ "Took " <> show d)
-- Took ...s
logDuration :: IO a -> (NominalDiffTime -> IO b) -> IO a
logDuration action logFn = do
  (a, duration) <- timed action
  _ <- logFn duration
  pure a

timed :: IO a -> IO (a, NominalDiffTime)
\end{code}

\begin{code}%hidden this implementation is not relevant
timed action = do
  start <- Time.getPOSIXTime
  a <- action
  end <- Time.getPOSIXTime
  pure (a, end - start)
\end{code}


To avoid the trap described in the last section,
we're going to be using the higher-order combinator [`control`][control],
which ensures we _do_ call `restoreM`.

\begin{code}
logDuration' :: (MonadBaseControl IO m) => m a -> (NominalDiffTime -> m b) -> m a
logDuration' action logFn = do
  control \runInBase ->
    logDuration (runInBase action) (runInBase . logFn)
\end{code}

However, there are 2 problems with this.

First, the input state is being forked and passed into both `action` and `logFn`.
Recall that `runInBase` is a closure that captures the input state.
And because we're applying it twice, once to `action` and once to `logFn`,
both actions will see the same input state.

\begin{code}
-- | >>> evalStateT testLogDuration1 [0]
-- State observed from 'action': [0]
-- State observed from 'logFn': [0]
testLogDuration1 :: StateT [Int] IO ()
testLogDuration1 = do
  logDuration'
    (printState "action" >> appendToState 1)
    (\_ -> printState "logFn")
\end{code}


This is not the behaviour most users would expect. A more sensible implementation would thread the output state of `action` into `logFn`.

The second problem is that, even though we _are_ using `restoreM` to restore the output state,
we're only restoring the output state of `action`. The output state of `logFn` is being discarded.

\begin{code}
-- | >>> execStateT testLogDuration2 [0]
-- [0]
testLogDuration2 :: StateT [Int] IO ()
testLogDuration2 = do
  logDuration'
    (pure ())
    (\_ -> appendToState 1)
\end{code}

Why? Let's have a closer look at the type of `logDuration`.
Note how it takes 2 input actions, `IO a` and `NominalDiffTime -> IO b`, but only returns the output `a` of the first action.
`b` is never returned, so its state cannot be restored.

Just as before, we need to break the function apart and reimplement it in terms of its primitives.

`logDuration` is defined in terms of `timed`, which takes a single input action.
Its type is `IO a -> IO (a, NominalDiffTime)`, so it allows the state to flow through.

\begin{code}
logDuration'' :: (MonadBaseControl IO m) => m a -> (NominalDiffTime -> m b) -> m a
logDuration'' action logFn = do
  (st, duration) <- liftBaseWith \runInBase -> do
    timed (runInBase action)
  a <- restoreM st
  _ <- logFn duration
  pure a
\end{code}

Now the input state is observed by `action`, `action`'s output state is observed by `logFn`,
and `logFn`'s output state will be observed by the caller.

\begin{code}
-- | >>> execStateT testLogDuration3 [0]
-- State observed from 'action': [0]
-- State observed from 'logFn': [0,1]
-- [0,1,2]
testLogDuration3 :: StateT [Int] IO ()
testLogDuration3 = do
  logDuration''
    (printState "action" >> appendToState 1)
    (\_ -> printState "logFn" >> appendToState 2)
\end{code}

Brick walls
---

So far, we've learned to be mindful of how the state flows through the function,
how it's captured and restored, and how we can "massage" the function's definition to make things work.

Still, there are times when we'll hit a wall and find functions that are just impossible to lift
with `MonadBaseControl` in a satisfactory way.

Two very common pitfalls are functions related to concurrency and exception handling.

<h3 id="concurrently">concurrently</h3>

Concurrency is a rather obvious issue, and [`concurrently`][concurrently] illustrates it well.

```hs
concurrently :: IO a -> IO b -> IO (a, b)
```

At a fundamental level, the input state must be forked and given to both branches and, once they're done,
we must only keep the state of one branch.

In the implementation below, I arbitrarily chose to always keep the state of the second branch, _regardless of which action finishes first_.
This is exactly how `concurrently` is implemented in the `lifted-async` package.

\begin{code}
-- | >>> execStateT (concurrently' (appendToState 1) (appendToState 2)) []
-- [2]
concurrently' :: (MonadBaseControl IO m) => m a -> m b -> m (a, b)
concurrently' ma mb = do
  (stateA, stateB) <- liftBaseWith \runInBase -> do
    Async.withAsync (runInBase ma) \asyncA ->
      Async.withAsync (runInBase mb) \asyncB -> do
        Async.waitBoth asyncA asyncB

  a <- restoreM stateA -- here we restore the output state of the 1st branch, but then...
  b <- restoreM stateB -- ... we immediately overwrite it with the output state of the 2nd branch.
  pure (a, b)
\end{code}

<h3 id="bracket">bracket</h3>

The issues with mixing exception handling and `MonadBaseControl` are a bit more subtle and deceiving.
Let's have a look at `bracket` to understand why.

```hs
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
```

If we start out with `control` and "follow the types", we'll get this:

\begin{code}
bracket' :: (MonadBaseControl IO m) => m a -> (a -> m b) -> (a -> m c) -> m c
bracket' acquire release use =
  control $ \runInBase ->
    bracket
      (runInBase acquire)
      (\st -> runInBase $ restoreM st >>= release)
      (\st -> runInBase $ restoreM st >>= use)
\end{code}

In fact, this is the exact example given in the docs for [`control`][control].

To understand it, it helps to see how exactly `bracket`'s type parameters are being instantiated here:

```hs
bracket
  :: IO (StM m a)              -- acquire
  -> (StM m a -> IO (StM m b)) -- release
  -> (StM m a -> IO (StM m c)) -- use
  -> IO (StM m c)
```

Let's break it down:

* The input state is captured and passed to our `acquire` action.
* The output state of `acquire` (`StM m a`) is passed to both `use` and `release`; `restoreM st >>= ...` ensures our `use` and `release` actions will see it.
* The output state of `release` (`StM m b`) is discarded.
* The output state of `use` (`StM m c`) is returned by `bracket` and will be restored by `control`.

There are 2 issues with this implementation:

* `release` runs after `acquire` and `use`, but _only_ sees the output state of `acquire`, not `use`.
* The output state of `release` is discarded.

\begin{code}
-- | >>> execStateT testBracket1 []
-- State observed from 'release': ["acquire"]
-- ["acquire","use"]
testBracket1 :: StateT [String] IO ()
testBracket1 =
  bracket'
    (appendToState "acquire")
    (\_ -> printState "release" >> appendToState "release")
    (\_ -> appendToState "use")
\end{code}

This is not how we want the state to be threaded.
Again, we'll break the function apart and reimplement it in terms of its primitives.
[`bracket` is defined][bracket-src] using `mask` and `onException`, so we'll redefine it using lifted versions
of those same functions from the `lifted-base` package (which do behave sensibly).

\begin{code}
bracket'' :: (MonadBaseControl IO m) => m a -> (a -> m b) -> (a -> m c) -> m c
bracket'' acquire release use =
  Lifted.mask \restore -> do
    a <- acquire
    c <- restore (use a) `Lifted.onException` release a
    _ <- release a
    pure c
\end{code}

Now we can observe the state being threaded correctly through all 3 actions:

\begin{code}
-- | >>> execStateT testBracket2 []
-- State observed from 'acquire': []
-- State observed from 'use': ["acquire"]
-- State observed from 'release': ["acquire","use"]
-- ["acquire","use","release"]
testBracket2 :: StateT [String] IO ()
testBracket2 =
  bracket''
    (printState "acquire" >> appendToState "acquire")
    (\_ -> printState "release" >> appendToState "release")
    (\_ -> printState "use" >> appendToState "use")
\end{code}

Looking good, right? Well... there's a very insidious bug hiding in there.

It works great when run on `StateT`, but if we run it on `ExceptT`, we run into trouble.
If the `use` function exits with `throwError`, then that effect will cause `bracket''` to short-circuit and skip the `release` handler!

\begin{code}
-- | >>> runExceptT testBracketExcept
-- acquire
-- use
-- Left "use error"
testBracketExcept :: ExceptT String IO ()
testBracketExcept = do
  bracket''
    (liftIO (putStrLn "acquire"))
    (\_ -> liftIO (putStrLn "release"))
    (\_ -> liftIO (putStrLn "use") >> throwError "use error")
\end{code}

The `use` function does not throw an exception (so `` `Lifted.onException` release a`` is never run) and exits early (before `_ <- release a` has a chance to run).

In an attempt to fix the threading of the state, we ended up making things much worse and broke `bracket`'s semantics!

The issue here is that we want the output state of `use` to be passed to `release`, _except_ when dealing with
transformers with multiple exit points like `ExceptT` and `MaybeT`.

In the latter case, in order to preserve bracket's semantics, we want to:

1. Run `use`, but don't restore its output state yet.
2. Run `release`.
3. If both `use` and `release` exited with an error, rethrow `release`'s.
4. Otherwise, restore `use`'s output state.

We simply cannot do this with `MonadBaseControl`.
A function lifted with `MonadBaseControl` has to capture/restore state uniformly for all possible transformers.
But what we want here is to have _multiple_ implementations of `bracket`, one for each concrete monad transformer, that
decides how best to capture/restore state.

And that is precisely how the `exceptions` package solves this problem.
It defines a `MonadMask` typeclass that, among other things, describes the semantics of an abstract [`generalBracket`][generalBracket].
The instances for `StateT`, `ExceptT`, `MaybeT`, etc., then implement the correct state threading behaviour for each
transformer, while upholding the prescribed semantics.

\begin{code}%hidden
-- | >>> runStateT (runExceptT testBracketFromExceptions) []
-- (Left "use error",["acquire","use","release"])
testBracketFromExceptions :: ExceptT String (StateT [String] IO) ()
testBracketFromExceptions = do
  Exceptions.bracket
    (appendToState "acquire")
    (\_ -> appendToState "release")
    (\_ -> appendToState "use" >> throwError "use error")
\end{code}


Conclusion
---

The good ol' "if it compiles, it works" just doesn't apply when dealing with `MonadBaseControl`.
It's not just a matter of making the types line up; it's a matter of semantics.

If you can get away with using only stateless transformers, do it!
None of the issues described here apply to stateless transformers (e.g., `ReaderT`, `LogT`).
Forking state is innocuous, and there's no output state to restore afterwards.

You often can avoid stateful transformers by replacing `StateT s` with mutable variables such as `ReaderT (IORef s)`, and `ExceptT` with runtime exceptions.

You can constrain your functions with `StM m a ~ a` to rule out stateful transformers.
This is what the "safe" module [`Control.Concurrent.Async.Lifted.Safe`][lifted-async-safe] from
the `lifted-async` package does, and you should prefer it over `Control.Concurrent.Async.Lifted`.

Another alternative is [`MonadUnliftIO`][monad-unliftio].
It's roughly equivalent to `MonadBaseControl` with `StM m a ~ a`, but with a simpler API.
The downside is that the base monad is constrained to `IO`.

Nevertheless, if you must support stateful transformers (e.g., `StateT`, `ExceptT`, `MaybeT`),
lifting functions with only 1 input action, like `withMVar`, is usually easy enough.
Just remember to _always_ restore the output state using `restoreM`.
Prefer using higher-order combinators like `control` and `liftBaseOp`, if possible.

Lifting functions with 2 or more input actions, on the other hand, is when things get complicated.
Having to use the `runInBase` closure more than once is a dead giveaway that something might be off.
Reimplementing the function in terms of simpler functions that only take 1 input action each _can_ sometimes work, but it's not guaranteed.

For exception handling, I'd recommend avoiding `MonadBaseControl` and `lifted-base`.
Instead, go with the `exceptions` package or, better yet, `safe-exceptions` for [safer handling of async exceptions][safe-exceptions].
You get the best of both worlds: power (it supports stateful transformers) and safety (it behaves sensibly with regard to state).

---

`MonadBaseControl` is a big hammer; wield it wisely.




\begin{code}%hidden
{-
  Note for myself:

  > Having to use the `runInBase` closure more than once is a dead giveaway that something might be off.

  Here I'm saing "might be off" instead of "is off" because there are some situations
  where using `runInBase` more than once is perfectly fine.
  For example, when sequencing two actions one after the other.
  This is a very very narrow use case, but it is valid.
-}
sequential :: IO a -> (a -> IO b) -> IO b
sequential ma mb = do
  a <- ma
  mb a

sequential' :: forall m a b. (MonadBaseControl IO m) => m a -> (a -> m b) -> m b
sequential' ma mb = do
  control \runInBase -> do
    sequential @(StM m a) @(StM m b)
      (runInBase ma)
      (\st -> runInBase $ restoreM st >>= mb)

-- | >>> execStateT testSequential [0]
-- State observed from 'ma': [0]
-- State observed from 'mb': [0,1]
-- [0,1,2]
testSequential :: StateT [Int] IO ()
testSequential = do
  sequential'
    do
      printState "ma"
      (appendToState 1)
    \() -> do
      printState "mb"
      (appendToState 2)
\end{code}

[mbc]: https://hackage.haskell.org/package/monad-control/docs/Control-Monad-Trans-Control.html#t:MonadBaseControl
[control]: https://hackage.haskell.org/package/monad-control-1.0.3.1/docs/Control-Monad-Trans-Control.html#v:control
[concurrently]: https://hackage.haskell.org/package/async/docs/Control-Concurrent-Async.html#v:concurrently
[bracket-src]: https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.Control.Exception.Base.html#bracket
[generalBracket]: https://hackage-content.haskell.org/package/exceptions/docs/Control-Monad-Catch.html#v:generalBracket
[lifted-async-safe]: https://hackage-content.haskell.org/package/lifted-async/docs/Control-Concurrent-Async-Lifted-Safe.html
[monad-unliftio]: https://hackage.haskell.org/package/unliftio
[alexis-mbc]: https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/
[lhs-src]: https://github.com/dcastro/dcastro.github.io/blob/master/blog-src/src/Mbc.lhs
[safe-exceptions]: https://github.com/fpco/safe-exceptions?tab=readme-ov-file#goals

---

[^1]:
  `StM m a` is an associated type family of `MonadBaseControl`.
  It represents a value `a` enriched with the state of a monad `m`.
  For example, `StM (StateT s IO) a ~ (a, s)`, and `StM (ExceptT e IO) a ~ Either e a`.
  For stateless monads, `StM m a` evaluates to `a`: `StM (ReaderT r IO) a ~ a`.
