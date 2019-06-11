---
title: A Practical Introduction to Freer Monads (Eff)
---

# Background

For the remainder of this post I'm going to assume that you roughly understand what a monad is, or
can at least understand how you would use one in a codebase that has an actual `main` method. If
not, fear not, there are a number of wonderful resources out there to teach you this, however,
unless you have that context, the rest of this post will seem useless, complicated, or both.

# Motivation

I could sit here and talk about the theoretical underpinnings that make Free/r monads interesting,
but there are far more qualified people than I to talk about such things. And while they are
certainly interesting in their own right, I want to take a step back, de-emphasize the theory,
and talk about something more concrete. And while you may have never exactly encountered the scenarios
I'm about to lay out, the essence of the frustration should seem eerily familiar.

## Requirements Thrash

Have you ever gotten the requirements of a project, coded it, delivered it to the stakeholder(s), 
and had them accept it without a fuss the first time around? Yeah, me neither. They always want to 
tweak something between that v0 you hand them before arriving at whatever becomes the stable solution
for the time being

Now, of course, this is fine. We want to satisfy our customers and write software that actually does what people 
want it to do, but when designing this stuff, there are certain decisions you can make that make your own life 
difficult if you try to change it later.

In most cases, when people ask you to make something, there's a very small set of its 
implementation that they care about, and that's usually the original API that they actually 
specify. Technical debates about whether you should store the data in Postgres or on the 
Filesystem, or debates about whether caching is done in memory or in Redis, are things _you_ get 
to decide. _Your PM's don't give a shit._

So given that you're building software for them in the first place, why would you spend any time on
the implementation details before getting the high level semantics down right?

Of course, that stuff still has to get done before you can actually ship the code, but a demo is
worth a thousand requirements meetings. People realistically don't know what they want until they
see it, so can we somehow show them a version of what the system will look like before we get to
all the grimy engineering details of making it fault tolerant, performant, etc.?

hmm...

Before we answer that, let's take a look at another situation.

## Testing

Testing is an interesting subject to talk about in Haskell because with such a sophisticated type
system we often find that when our software compiles it will "just work". Now this isn't technically
true because any monomorphic function `Foo -> Bar -> Baz` can have many different implementations
that satisfy that type signature, and almost certainly, at least one of them is wrong.

So while there are entire categories of tests we don't have to write that people writing Ruby or JS
have to, the number of tests we have to write is still nonzero. Now, for pure functions we have some
pretty world-class tooling such as `quickcheck` and `hedgehog` which I've been favoring more recently,
but these things are primarily focused on testing _data transformations_.

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

## Prove you can't _Launch Nukes™_

If you spend even a little bit of time in Haskell you'll start to lean pretty heavily on type
signatures to get an idea of what a particular piece of code is doing. `Asset -> Price` probably
gives you the price of that asset, which is loads better than a comparable signature
`String -> Double`. Not only because it constrains the input and output types, but also makes a
good faith effort to describe what the function is doing in a very "TL;DR" manner.

So what is the least descriptive type signature ever?

Well, given that Haskell is a general purpose programming language, and than Turing Completeness
makes it such that anything that is computable should be expressible, it stands to reason that the
type signature of our `main` method is about the most useless type signature ever, since
_any program at all_ can satisfy it. So what is that type signature?

```haskell
IO ()
```

Any program at all can inhabit that type. This means without scrutinizing its contents we have no
idea what it does. And while `()` is a somewhat worthless return type since it only has one
inhabitant, it's not the scariest part of this type signature. The structurally similar
`Identity ()` is a lot clearer about what it can, or more importantly _can't_, do.

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
effectful code into a "what" and a "how", along with a method of choosing the "how" at a different
call site than the what. There are numerous implementations of this idea, and the one that we'll be
referencing throughout the rest of this post is [freer-simple](https://hackage.haskell.org/package/freer-simple).

Your business logic cares about the "what", but your execution environment is what cares about the
"how".

## Minimum Viable Eff effect

```haskell
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

```haskell
getLine :: Member Console r => Eff r String
getLine = send GetLine

putLine :: Member Console r => String -> Eff r ()
putLine = send . PutLine
```

What is happening here is that `r` is a _type-level list_ of effects, and the `Member` constraint
is saying that `Console` must appear in that list somewhere. Finally, `send` is merely allowing us
to use these effects together with each other in a "mix and match" fashion, without having to worry
about the machinery that keeps all of this type-safe.

What this does is it takes the constructors for that datatype and "injects" them into the `Eff r`
monad that is completely polymorphic in r with a constraint that the Console effect is in there
somewhere.

Keep in mind, we haven't said shit about how this thing is supposed to get or put lines
anywhere. We've just said, "hey, we want to do get and put to the console, and we'll worry about 
how to do it some other time". So let's consider the following program

```haskell
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

```haskell
consoleToIO :: Console a -> IO a
consoleToIO action = case action of
    GetLine -> Prelude.getLine
    PutLine s -> Prelude.putStrLn s
```

This is all great, but `greetBot` isn't actually a program of type `Console a`. Instead, it is one
that that has the type `Eff r a` where the only requirement on `r` is that it is a list that contains
`Console` in it somewhere. The minimum concretion of `greetBot` could have type `Eff '[Console] ()`, but
the point here is that it is not _limited_ to that, and can be combined at will with other effects, that,
in conjunction, build up a much larger list.

Once this list is built, though, we need a way to independently interpret these effects. We also need to
do this in such a way that we can define the handler with no knowledge of anything besides the source and
target effects. We want this so that our effects can remain isolated from one another but can be composed
together to interpret more complicated programs.

This is where the value of effect libraries such as `freer-simple` start to shine.

`freer-simple` gives us some functions to be able to take the above action mapping and use it in the
context of the `Eff` machinery.

```haskell
translate :: (forall a. f a -> g a) -> Eff (f ': r) b -> Eff (g ': r) b
translate = _

runM :: Eff '[m] a -> m a
runM = _

-- to close the gap
interpretConsoleInIO :: Eff '[Console] a -> IO a
interpretConsoleInIO = runM . translate consoleToIO

main :: IO () -- this translation to IO happens at the edge of our program
main = interpretConsoleInIO greetBot
```

Great! But how do we test it? I promised testing capabilities, I should deliver on it. To really do
that we need to tweak the original program just a bit so we can just test a single iteration of it.

```haskell
greetBot :: Member Console r => Eff r ()
greetBot = fix greetBot'

greetBot' :: Member Console r => Eff r () -> Eff r ()
greetBot' continue = do
    putLine "What is your name?"
    name <- getLine
    putLine $ "Hello, " <> name <> "!\n"
    continue
```

We have to do this because if we try to test a program that loops forever the test will never
terminate itself. So we'll actually be testing greetBot' here.

What is a natural way we might want to test this? Well, the main invariant here is that the thing
emitted over the put should at least contain the name obtained via the get. Let's write out a
property test for this.

```haskell
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

```haskell
-- freer simple provides some out of the box reader and writer effects that behave the same way
-- that their identically named monads in `base` behave
consoleToReaderAndWriter :: ( Member (Reader String) r
                            , Member (Writer [String]) r
                            ) => Console a -> Eff r a
consoleToReaderAndWriter action = case action of
    GetLine -> ask
    PutLine s -> tell s
```

And with the appropriate functions from `freer-simple` we can interpet this down to a pure value!

```haskell
-- used to get from Console to Reader AND Writer
reinterpret2 :: (forall a. f a -> Eff (g ': h ': r) a) -> Eff (f ': r) b -> Eff (g ': h ': r) b
reinterpret2 = _

-- used to discharge reader
runReader :: env -> Eff (Reader env ': r) a -> Eff r a
runReader = _

-- used to discharge writer
runWriter :: Monoid w => Eff (Writer w ': r) a -> Eff r (a, w)
runWriter = _

-- used to discharge Eff machinery around a pure value
run :: Eff '[] a -> a
run = _

-- to close the gap between our mapping and the function we want
interpretConsoleInReaderWriter :: String -> Eff '[Console] a -> (a, [String])
interpretConsoleInReaderWriter env =
    run . 
    runReader env . 
    runWriter . 
    reinterpret2 consoleToReaderWriter
```

OK. So now that we've defined our testing interpreter we're ready to complete that property test.

```haskell
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

So what we want to do is create a server that continuously fetches prices from third parties, 
aggregates them some way, saves them, and then serves up the result on request.

It might be tempting to say that a web service that does this seems too simple to be useful, 
however, if any of my colleagues were reading this, they'd tell you it looks awfully similar to a 
service we have currently running in production.

## Let's write some new effects!

Ok. So immediately what jumps out at me is that since the problem statement was intentionally vague
about the third parties in question, and the method of saving, those are the candidates for...wait 
for it..._free-monadification_. 

```haskell
data AssetPairing = _
data Price = _
data Exchange = _

data PriceFeed a where
    FetchPrice :: Exchange -> AssetPairing -> PriceFeed Price
makeEffect ''PriceFeed

data PriceStore a where
    SavePrice :: AssetPairing -> Price -> PriceStore ()
    GetMostRecentPrice :: AssetPairing -> PriceStore Price
makeEffect ''PriceStore
```

## Time to make the PM's happy

With just the code above we're actually ready to write our business logic.

For the daemons continuously fetching and saving we have this:

```haskell
allExchanges :: [Exchange]
allExchanges = _

getPricesFromAllSources :: Member PriceFeed effs => AssetPairing -> Eff effs [Price]
getPricesFromAllSources assetPairing = for allExchanges $ \exchange ->
   fetchPrice exchange assetPairing

aggregatePrices :: [Price] -> Price
aggregatePrices = _ -- some fold

fetchAndSave :: (Member PriceFeed effs, Member PriceStore effs) => AssetPairing -> Eff effs ()
fetchAndSave assetPairing = do
    prices <- getPricesFromAllSources assetPairing
    let agg = aggregatePrices prices
    savePrice assetPairing agg
```

And for our request handler we have this embarrassingly small piece of code here. And since we
actually want to wire this up to a real Yesod handler, let's go ahead and do just that.

```haskell
getPriceH :: AssetPairing -> Handler Value
getPriceH assetPairing = ??? $ fmap toJSON $ getMostRecentPrice assetPairing
```

The astute reader might notice that we're in the wrong monad here. We need to go from our `Eff`
defined logic to the actual handler here.

The above code definitely cheats. Freer monads don't save us from having to write all the grimy
engineering details, but it _does_ save us from having to interleave those details, or even commit
to them. But when we actually wire into the web application, it's time to make a commitment. After
all we can't avoid specifying how these prices will get fetched and saved in a real production
environment.

## Make it work

So what will our interpreters look like?

Well, since we're fetching these prices from external parties, theres pretty much no avoiding
going straight to IO, possibly with some sort of configuration for an api key.

```haskell
type (~>) f g = forall a. f a -> g a -- from freer-simple
data ExchangeConf = _
gdaxApiKey :: ExchangeConf -> String
data GDAXResponse = _
gdaxRespToPrice :: GDAXResponse -> Price

asks :: Member (Reader) r effs => (r -> a) -> Eff effs a
asks = _ -- from freer-simple

priceFeedToRIO :: (Member (Reader ExchangeConf) effs, LastMember IO effs) => PriceFeed ~> Eff effs
priceFeedToRIO action = case action of
    FetchPrice exchange pairing -> case exchange of
        GDAX -> do
            -- GDAX actually doesn't require an api key for their price api, but I'm -- making this up because enough
            -- third party services require some sort of auth that this felt like it'd be more helpful
            key <- asks gdaxApiKey
            initReq <- sendM . parseRequest $ "GET http://api.pro.coinbase.com/products/"
                <> show pairing
                <> "/ticker?apiKey="
                <> key
            gdaxRespToPrice <$> sendM (httpJson initReq)
```

## Test it

Great. We now have a way to legitimately fetch prices from a real place. But do we want to hit GDAX
from our CI pipeline?

```haskell
type ExchangeTestbed = HashMap (Exchange, AssetPairing) Price

priceFeedToReader :: (Member (Reader ExchangeTestbed) effs) => PriceFeed ~> Eff effs
priceFeedToReader action = case action of
    FetchPrice exchange pairing -> do
        hm <- ask
        -- It's a test interpreter for a conference talk, I'm cheating totality here
        let price = fromJust $ lookup (exchange, pairing) 
        pure price
```

So now we can test that our business logic saves the right data because we can control what data it
gets to begin with.

## Interpreters are reusable

What does the PriceStore interpreter look like? Well it depends on how we want to store the data.
Here we have some choices: an sql database (postgres), redis, live memory, or some combination of
those.

```haskell
priceStoreToPostgres :: ( Member (Reader ConnectionPool) effs
                        , LastMember IO effs
                        ) => PriceStore ~> Eff effs
priceStoreToPostgres action = do
    pool <- ask
    Persistent.runSqlPool $ case action of
        SavePrice pairing price -> insert _ -- left as exercise
        GetMostRecentPrice pairing -> selectFirst _ -- left as exercise

type PriceCache = TVar (HashMap AssetPairing Price)
priceStoreToPriceCache :: ( Member (Reader PriceCache) effs
                          , LastMember IO effs
                          ) => PriceStore ~> Eff effs
priceStoreToPriceCache action = do
    cache <- ask
    case action of
    SavePrice pairing price ->
        sendM $ atomically $ do
            cacheState <- readTVar cache
            let newCacheState = insert pairing price cacheState
            writeTVar cache newCacheState
    GetMostRecentPrice pairing ->
        sendM $ readTVarIO cache

priceStoreToPGandCache :: ( Member (Reader ConnectionPool) effs
                          , Member (Reader PriceCache) effs
                          , LastMember IO effs
                          ) => PriceStore ~> Eff effs
priceStoreToPGandCache action = case action of
    SavePrice _ _ -> do
        priceStoreToPostgres action
        priceStoreToPriceCache action
    GetMostRecentPrice _ ->
        -- no pg here because we're just reading, gotta go fast
        priceStoreToPriceCache action 
```

Wow. So we just wrote two separate effects handlers and wrote a third one in terms of the other two.
Hopefully this conveys that something you might encounter in a real world codebase can be turned
into this style. This is still perhaps a simpler problem than the typical industry grade version,
but it's still more than a toy and should demonstrate the type of value you would get from doing
something like this.

# Why shouldn't I use this

Alright alright, is it too good to be true? Just barely. The reasons why you may choose not to use
this style in a production Haskell codebase are as follows:

* Monadic sections of your code can be slower
* Resource bracketing can't be expressed this way

But hope is not lost, there is an alternative library that [Sandy Maguire](https://reasonablypolymorphic.com/)
just published called [polysemy](https://hackage.haskell.org/package/polysemy) that pretty much
fixes both of these problems. The only reason I didn't write this post with that as the library
being studied is because I haven't had a chance to play with it in a production codebase yet.

# Conclusion

Freer monads have made my code way more testable, better documented, and much better decomposed
than it used to be without. I am by no means saying this is the only way for you to accomplish
these things but it has certainly improved my code quality by quite a margin, and yet it remains
practical enough for us to deploy real-world services that use this technique to production. If
you have had a tough time testing IO code or find that you get this sense of fear when you see
a type signature of `a -> IO b`, maybe give this a shot and see if it solves your problems.

It is also worth noting that this technique can be introduced at the edges of your existing
services without it infecting everything else, however the ergonomics of it skyrocket when you
refactor your whole codebase to use this technique. Happy coding.

Until next time.

Peace.