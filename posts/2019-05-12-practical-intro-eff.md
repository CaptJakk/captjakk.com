---
title: A Practical Introduction to Freer Monads (using Eff)
---

# Background

For the remainder of this post I'm going to assume that you roughly understand what a monad is, or
can at least understand how you would use one in a codebase that has an actual `main` method. If
not, fear not, there are a number of wonderful resources out there to teach you this, however,
unless you have that context, the rest of this post will seem useless, complicated, or both.

# Motivation

I could sit here and talk about the theoretical underpinnings that make Free/r monads interesting,
but there are far more qualified people than I to talk about such things. And while they are
certainly interesting in their own right, I want to take a step back, forget all about the theory,
and revisit something more concrete. And while you may have never exactly encountered the scenarios
I'm about to lay out, the essence of the frustration should seem eerily familiar.

## Requirements Thrash

Have you ever gotten the requirements of a project, coded it, delivered it to the stakeholder(s), 
and had them accept it without a fuss the first time around? Yeah, me neither. They always want to 
tweak something between that v0 and whatever ends up being the stable solution for the time being.

Now, of course, this is fine. We want to satisfy our customers and write software that actually does what people want it to do, but when designing this stuff, there are certain decisions you can make that make your own life difficult if you try to change it later.

In most cases, when people ask you to make something, there's a very small set of its 
implementation that they care about and that's usually the original API that they actually 
specify. Technical debates about whether you should store the data in Postgres or on the 
Filesystem, or debates about whether caching is done in memory or in Redis, are things _you_ get 
to decide. _Your PM's don't give a shit._

So given that you're building software for them in the first place, why would you spend any time on
the implementation details before getting the high level semantics down right?

Of course that stuff still has to get done before you can actually ship the code but a demo is
worth a thousand requirements meetings. People realistically don't know what they want until they
see it, so can we somehow show them a version of what the system will look like before we get to
all the grimy engineering details of making it fault tolerant, performant, etc.?

hmm...

Before we answer that, let's take a look at another situation.

## Testing

Testing is an interesting subject to talk about in Haskell because with such a sophisticated type
system we often find that when our software compiles it will just work. Now this isn't technically
true because any monomorphic function `Foo -> Bar -> Baz` can have many different implementations
that satisfy that type signature, and almost certainly at least one of them is wrong.

So while there are entire categories of tests we don't have to write that people writing Ruby or JS
have to, the number of tests we still have to write is still nonzero. Now, for pure functions we 
have some pretty world-class tooling such as `quickcheck` and `hedgehog` which I've been favoring 
more recently, but these things are primarily focused on testing _data transformations_.

However, sometimes we want to be able to write a test that ensures that _actions_ produce other
actions that may not have a representation in the return type of your function. After all, how
would you go about testing whether a function `Foo -> IO Bar` worked correctly? If it was supposed
to log the value of type `Foo` before grabbing the right `Bar` out of the database and returning
it, how do we make sure that log event happened?

It'd be nice if we could plug and play logging implementations depending on whether we were in a
test environment or the real application. But to do that we need to be able to parameterize part of
that function. The trouble is that we _know_ this function needs to take a `Foo` as an argument and
yield a `Bar` as a result. So what else is there to parameterize? Can we parameterize the monad
it's running in to be `Foo -> m Bar` and then depending on the environment instantiate `m` with
either `IO` or some test monad?

This is roughly how the strategy of mocking things works in OOP. But we can't let them have nicer
things than us. Is there a way we can accomplish all the same things?

Let's visit one final frustration before we get to the answer.

## Prove you can't Launch Nukes™

If you spend even a little bit of time in Haskell you'll start to lean pretty heavily on type
signatures to get an idea of what a particular piece of code is doing. `Asset -> Price` probably
gives you the price of that asset, which is loads better than a comparable signature
`String -> Double`. Not only because it constrains the input and output types, but also makes a
good faith effort to describe what the function is doing in a very "TL;DR" manner.

So what is the least descriptive type signature ever? Well, given that Haskell is a general purpose
programming language, and than Turing Completeness makes it such that anything that is computable
should be expressible it stands to reason that the type signature of our `main` method is about the
most useless type signature ever, since _any program at all_ can satisfy it. So what is that type signature?

`IO ()`

Any program at all can inhabit that type. Which means without scrutinizing its contents we have no
idea what it does. And while `()` is a somewhat worthless return type since it only has one
inhabitant, it's not the scariest part of this type signature. The structurally similar
`Identity ()` is a lot clearer about what it can or more importantly _can't_ do.

So why is `IO` so scary? Because it's more or less like giving root to someone. Once given control,
it can do whatever it wants before giving control back to the caller.

Nevertheless, if we want to write useful programs we need to be able to do things that require 
`IO`. But what we'd want to do is constrain the _types_ of `IO` it can do, and make it clear in
the type signature that those are all it requires.

So we want some system of specifying which types of `IO`, henceforth referred to as _effects_, in
such a way that if we needed to add more effects to that function we could easily do it, but still
be forced to say that is what we are doing.

Enter Eff.

# What is Eff?

Eff is a structure with some beautiful theoretical underpinnings that allows us to deal with the
above phenomena in a tractable and scalable way. It's main value proposition is bisecting your
effectful code into a "what" and a "how". Along with a method of choosing the "how" at a different
call site than the what. There are numerous implementations of this idea, and the one that we'll be
referencing throughout the rest of this post is [freer-simple]().

Your business logic cares about the "what", but your execution environment is what cares about the
"how".

## Minimum Viable Eff effect

```
data Console a where -- GADT that defines the types of operations in this API
    GetLine :: Console String
    PutLine :: String -> Console ()
makeEffect ''Console -- TH code that generates the functions you'll use in business logic
```

So what's going on here? We have a datatype that describes some `Console` effect that has two
operations: `GetLine` which is some effectful way of getting a `String`, and `PutLine` which takes 
a `String` and does something with it and gives you back `()`

But the magic is not in the datatype it's in the following function definitions that are generated
automatically by the Template Haskell `makeEffect` declaration:

```
getLine :: Member Console r => Eff r String
putLine :: Member Console r => String -> Eff r ()
```

What this does is it takes the constructors for that datatype and "injects" them into the `Eff r`
monad that is completely polymorphic in r with a constraint that the Console effect is in there
somewhere. Keep in mind, we haven't said shit about how this thing is supposed to get or put lines
anywhere. We've just said, "hey, we want to do get and put to the console, and we'll worry about 
how to do it some other time". So let's consider the following program

```
greetBot :: Member Console r => Eff r ()
greetBot = do
    putLine "What is your name?"
    name <- getLine
    putLine $ "Hello, " <> name <> "!\n"
    greetBot
```

Neat. This program, from a structural standpoint looks like how we would code a bot that repeatedly
asks for your name and then greets you. We aren't bogged down with the details about how to get
that string or send out the greeting. The code only specifies the high level design of the program.
The skeptical reader might say, well we can do that without all this Eff machinery by just pulling
out `getLine :: IO String` and `putLine :: String -> IO ()` to their own function. And not only
that, but `base` already does this for us. So what have we really accomplished?

The answer is that not only have we packed that logic elsewhere, but we haven't even committed to
a particular implementation yet! There are no typed holes, no `undefined`s and we still can have a
program that typechecks without having committed to these details.

That said, this program is still incomplete and won't yet run, precisely because we haven't
actually told it how to handle these gets and puts.

So what are we going to do in the regular program case? The aforementioned functions in base will
do just fine I think:

```
consoleToIO :: Console a -> IO a
consoleToIO action = case action of
    GetLine -> Prelude.getLine
    putLine s -> Prelude.putStr s
```

And `freer-simple` gives some combinators to be able to take the above action mapping and use it
in the context of the `Eff` machinery.

```
translate :: (forall a. f a -> g a) -> Eff (f ': r) b -> Eff (g ': r) b
translate = ...

runM :: Eff '[m] a -> m a
runM = ...

-- to close the gap
interpretConsoleInIO :: Eff '[Console] a -> IO a
interpretConsoleInIO = runM . translate consoleToIO

main :: IO () -- this translation to IO happens at the edge of our program
main = interpretConsoleInIO greetBot
```

Great! But how do we test it? I promised testing capabilities, I should deliver on it. To really do
that we need to tweak the original program just a bit so we can just test a single iteration of it.

```
greetBot :: Member Console r => Eff r ()
greetBot = greetBot' greetBot' -- alternatively `fix greetBot'`

greetBot' :: Member Console r => Eff r () -> Eff r ()
greetBot' rec = do
    putLine "What is your name?"
    name <- getLine
    putLine $ "Hello, " <> name <> "!\n"
    rec
```

We have to do this because if we try to test a program that loops forever the test will never
terminate itself. So we'll actually be testing greetBot' here.

What is a natural way we might want to test this? Well, the main invariant here is that the thing
emitted over the put should at least contain the name obtained via the get. Let's write out a
property test for this.

```
-- hedgehog property test
prop_nameMatchesGreeting :: Property
prop_nameMatchesGreeting = property $ do
    name <- forAll nameGenerator

    _ -- uhhh, what goes here?
```

So we've run into our first issue, we want to be able to supply a name that was given to us from
the test environment to our program directly. So we want `greetBot'` to _read_ for its `getLine`
call and _write_ for its `putLine` call. Can we interpret our Console action into more than one
effect? Turns out yes.

```
-- freer simple provides some out of the box reader and writer effects that behave the same way
-- that their identically named monads in `base` behave
consoleToReaderAndWriter :: ( Member (Reader String) r
                            , Member (Writer [String]) r
                            ) => Console a -> Eff r a
consoleToReaderAndWriter action = case action of
    GetLine -> ask
    PutLine s -> tell s
```

And with the appropriate combinators from `freer-simple` we can interpet this down to a pure value!

```
-- used to get from Console to Reader AND Writer
reinterpret2 :: (forall a. f a -> Eff (g ': h ': r) a) -> Eff (f ': r) b -> Eff (g ': h ': r) b
reinterpret2 = ...

-- used to discharge reader
runReader :: env -> Eff (Reader env ': r) a -> Eff r a
runReader = ...

-- used to discharge writer
runWriter :: Monoid w => Eff (Writer w ': r) a -> Eff r (a, w)
runWriter = ...

-- used to discharge Eff machinery around a pure value
run :: Eff '[] a -> a
run = ...

-- to close the gap between our mapping and the function we want
interpretConsoleInReaderWriter :: String -> Eff '[Console] a -> (a, [String])
interpretConsoleInReaderWriter env =
    run . 
    runReader env . 
    runWriter . 
    reinterpret2 consoleToReaderWriter
```

OK. So now that we've defined our testing interpreter we're ready to complete that property test.

```
-- hedgehog property test
prop_nameMatchesGreeting :: Property
prop_nameMatchesGreeting = property $ do
    name <- forAll nameGenerator

    let consoleOutput = snd $ interpretConsoleInReaderWriter name (greetBot' $ pure ())

    case consoleOutput of
        [] -> failure -- nothing was emitted
        (line:_) -> assert $ name `isInfixOf` line
```

Boom! We just wrote a test that tests effects working properly within the context of our business
logic.

Let's recap what just happened. With quite minimal overhead we defined a new capability `Console`
to be used throughout our application. We defined the interpreter we want it to use in the
production environment, as well as an interpreter that allows us to control inputs and measure
outputs in our test environment. Additionally, we gained the ability to write business logic
without committing to a `Console` implementation. And finally, our business logic more explicitly
states the capabilities it needs.

## Can we do this to everything?

The short answer here is yes. You absolutely can go ham on making effect algebras for everything in
your entire codebase, but every effect you introduce gives you some extra overhead. So my rule of
thumb is this: If you have some well defined semantics for your API, or you need to be able to mock
it out for testing, it's a pretty good candidate for an Eff effect. Otherwise, you probably lose
more than you gain from this.

All that said, some people have taken this much further and have some really interesting results.

# Time for the Majors

OK. So the example above is pretty compelling (at least to me), but when was the last time you
actually wrote a program that only did reads and writes to the console. It was probably the first
thing you learned to do when you learned to code so it doesn't really accurately reflect the
problems you deal with in industrial software, right?

Wrong. There are some reasons that you may not want to use this technique in production and I'll
get to those at the end, but inability to express all of the things that you need is not on that
list.

## Problem Statement

