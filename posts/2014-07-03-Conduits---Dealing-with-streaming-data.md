---
title: Conduits - Dealing with streaming data
description: Introduction to a easy to use event driven streaming library in Haskell.
tags: Haskell, Conduits, programming
---

__Most programmers already know how hard it can be to deal with streaming data in a nice and efficient way. The conduit package addresses the problems of working with streaming data and offers a solution to work, transform, consume streams of data in a event driven way. Conduit also takes care of the lifetime of data streams and organises the data do be proceeded with constant memory usage. In my opinion all this makes it worth to take a closer look to that neat solution.__

The package and module reference can be found at [hackage](http://hackage.haskell.org/conduit). If you\'re interested in real word packages who use _conduit_ to manage streaming data take a look at: [http-conduit](http://hackage.haskell.org/http-conduit) or [xml-conduit](http://hackage.haskell.org/xml-conduit).

Let\'s start with an introductory example:

~~~~ {.haskell .numberLines}
import Data.Conduit
import qualified Data.Conduit.List as CL


source :: Source IO Int
source = CL.sourceList [1..20]

conduit :: Conduit Int IO Int
conduit = do
    first <- await
    case first of
        (Just f) -> yield (f ++ 1) >> conduit
    
showMe :: Conduit Int IO String
showMe = CL.map show

sink :: Sink String IO ()
sink = CL.mapM putStrLn

main :: IO ()
main = source $$$$ conduit =$$= showMe =$$ sink
~~~~

## Let's start

Grasping the concept of _conduit_ isn\'t hard. Imagine a tap which is connected to pipes which finally ends in a drainage. The pipes itself can be a complex construction with some machines processing what ever comes trough the pipes.
In the _conduit_ package there is a type which is something like a tap, it\'s called `Source`. As the name implies, it generates data (for example reading data from a file descriptor or socket). There is also a drainage like type: The `Sink` which consumes data and performs actions on received data (for example print it to the screen). The last one of the triplet is `Conduit`. `Conduits` are the machines in the construction which maybe transform the data or manage how the data flows.
With these three types you\'ll able to write the most of the _conduit_ based implementations you might need.

Looking at the haskell definitions offers that all three types are merely wrappers around one generic transformer monad.

~~~~ {.haskell}
-- Source has no input type (because it only generates data) and no return type
type Source m o = ConduitM () o m ()

-- A Sink only takes an input type and produces a result
type Sink i m r = ConduitM i Void m r

-- Take an input as well as an output type but does not return an explicit result
type Conduit i m o = ConduitM i o m ()
~~~~

Currently `i`, `m`, `o`, `r` makes no sense without guessing. So let\'s take a look at 
the definition of the monad transformer `ConduitM`:

`newtype ConduitM i o m r = ConduitM { ... }`

- __i__  - input type
- __o__  - output type
- __m__  - monad to transform
- __r__  - the return value

The abstraction to one generic transformer makes it possible to reuse a lot of code and make programming _conduits_ a lot more pleasant (as you will see later).

We now understand the basic monadic types of the _conduit_ package. But with only the types we\'re not able to do anything useful.

## Glue together?

~~~~ {.haskell}
main = source $$$$ conduit =$$= anotherConduit =$$ sink
-- equivalent
main = source $$= conduit =$$= anotherConduit $$$$ sink
~~~~

What\'s currently missing are the pipes in _conduits_. Something is needed to glue a `Source`, some `Conduits` and a `Sink` together. Here Conduits introduces here
two new names. First the package provides some functions which tie two types together and return a new one. These functions called `Fuse` operators. The second new name is `Connector` which is a function which _connects_ a `Source` and a `Sink` together and produce a useful output.


__Fuses__:

- `=$$`  fuse a `Conduit` and `Sink` together and create a new `Sink`
 
- `$$=`  fuse a `Source` and `Conduit` together and create a new `Source`
 
- `=$$=` combines two `Conduits` and creates a new `Conduit`

__Connector__:

- `$$$$` connects a `Source` and a `Sink`

_Just remember: You can look up the full reference at [hackage](http://hackage.haskell.org/package/conduit/docs/Data-Conduit.html)_

Now we\'re able to put some `Conduits`, `Sources` and `Sinks` together. But the real interesting part is, how to implement `ConduitM` like functions (`Sources`, `Sinks` and especially `Conduits`).

## It\'s all about the flow!

Before looking into implementing conduit based functions, we should think about the data flow and how the data is being processed. Imagine the tap, pipes and the drainage again: The data flows from the tap through the pipes in the drainage. In a conduit structure the same is happening. Data flows from the `Source` to the `Sink` via maybe some `Conduits`, from now on we call this _downstream_.

In conduits the working flow is vice versa (_upstream_). The first element which start to work is the `Sink`.

It will `await` some data. Because there is currently none, it pauses and asks the next element _upstream_ for some data. In most cases this is a `Conduit`. Here the same procedure takes place. The `Conduit` `awaits` data but there is none it pauses and asks the next element _upstream_. The same procedure keeps happening until a `Source` is reached.

The `Source` might have data available. It `yields` some data _downstream_ where the data is processed by the next waiting _conduit_. Now all elements which are `awaiting` data are successively called and `yield` data _downstream_ until the `Sink` returns the final value or for example prints it to the screen.

Now, the basic functions aren\'t hard do understand.

~~~~ {.haskell}
-- Pull data from upstream
await :: Monad m => Consumer i m (Maybe i)

-- Put data to downstream
yield :: Monad m => o -> ConduitM i o m ()

-- Put data back to upstream
leftover :: i -> ConduitM i o m ()
~~~~

With these three functions, which of course work in __all__ types (because of `ConduitM`), we\'re finally able to create our own machines (`Conduits`) which transform data in the way we want.

Let\'s look at a simple example:

~~~~ {.haskell .lineNumbers}

import Control.Monad.IO.Class (liftIO)
import System.IO
import Data.Conduit

source :: Source IO String
source = do
    yield "moep"
    yield "hoep"
    yield "floep"
    yield "knoep"

conduit :: Conduit String IO String
conduit = do
    m <- await
    case m of 
      (Just msg) -> isFloep msg >> conduit
      _          -> return ()
    where
      isFloep fl
        | fl == "floep" = yield "I don not like floep" >> conduit
        | otherwise     = yield fl >> conduit
        
sink :: Sink String IO ()
sink = do
    m <- await
    case m of
      (Just msg) -> liftIO (putStrLn m) >> sink
      _          -> return ()
      
main :: IO ()
main = source $$= conduit $$$$ sink
~~~~
Output:

```
»» runhaskell cond1.hs
moep
hoep
I do not like floep
knoep
```

## The easier way around

You might have noticed that in the implementation of _conduit_ first an `await` is called which returns a `Maybe` value. The function also uses recursion. Handleing this and dealing with `Maybe` values all the time is tedious. That\'s why there is a nice helper function you should know of:

~~~~ {.haskell}
awaitForever :: Monad m => (i -> ConduitM i o m r) -> ConduitM i o m ()
~~~~

`awaitForever` calles a function `(i -> ConduitM i o m r)` every time data is available.

Most nice things never come alone (at least in this case). Some utility functions are defined in `Data.Conduit.List` (reference [here](http://hackage.haskell.org/package/conduit/Data-Conduit-List.html)). It\'s advisable to import it as _qualified_ module, because it redefines some standard functions.

With `awaitForever` and `Data.Conduit.List` the above example can be simplified to:

~~~~ {.haskell .numberLines}
import Control.Monad.IO.Class (liftIO)
import System.IO
import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO String
source = CL.sourceList ["moep","hoep","floep","knoep"]

conduit :: Conduit String IO String
conduit = awaitForever isFloep
    where
        isFloep fl
          | fl == "floep" = yield "I do not like floep"
          | otherwise     = yield fl

sink :: String IO ()
sink = CL.mapM (liftIO . putStrLn)
~~~~

## The `Producer` and the `Consumer` - just a generalization

Look again at the definition of `await`:

~~~~ {.haskell}
await :: Monad m => Consumer i m (Maybe i)
~~~~

We expected a `ConduitM` as return type, but apparently it\'s not. That\'s because there are two more wrappers around `ConduitM`. They called `Consumer` and `Producer`.

Because in some cases we need to generalise some parts of `ConduitM`. Take a look at the definitions:

~~~~ {.haskell}
type Producer m o = forall i. ConduitM i o m ()

type Consumer i m r = forall o. ConduitM i o m r
~~~~

`Producer` is a generalized `Source`. It\'s needed if a function should take any input and also give a output. `Consumer` is the counterpart of `Producer` and a generalization of a `Sink`. It can take any output and also feeds some input.

Too make it more clear here again the definition of `Source` and `Sink`:

~~~~ {.haskell}
type Source m o = ConduitM () o m () -- can't take any input

type Sink i m r = ConduitM i Void m r  -- doesn't have any output
~~~~

With this generalisations it\'s possible to combine for example `Sources` and `Conduits`.

~~~~ {.haskell .numberLines}
doubleTime :: Producer IO Int
doubleTime = awaitForever $$ \x -> CL.sourceList [x,x]

takeTwice :: Consumer Int IO [Int]
takeTwice = CL.take 2
~~~~

If you want to convert a `Source` to a `Producer` and a `Sink` to a `Consumer` there is `toProducer` and `toConsumer`.


## Finalization

One important thing is still missing. If any `Conduit` or `Sink` fails or returns the conduit immediately returns the value. But what happens to allocated/opened objects (for example an open file descriptor)? The `conduit` package offers a solution called _finalization_.
If any element in a _conduit_ system fails, the finalization callback which was set, is triggered. This callback is overwritten by any _upstream_ element.

You can manage finalization with these functions:

~~~~ {.haskell}
-- adds a finalize callback
addCleanup :: Monad m => (Bool -> m ()) -> Conduit i o m r -> Conduit i o m r

-- yield or a finalize function
yieldOr :: Monad m => o -> m () -> Conduit i o m ()

-- run a component and guarantee exception safety
bracketP :: MonadResource m => IO a -> (a -> IO ()) -> (a -> ConduitM i o m r) -> ConduitM i o m r
~~~~

Using `addCleanup` to close a file handle:

~~~~ {.haskell .numberLines}

cleanupSource :: Source IO Char
cleanupSource = do
    hdl <- liftIO $$ openFile "foobar.txt" ReadMode
    addCleanup cleanupCb (readChar hdl)
    where
      cleanupCb    = hClose handle
      readChar hdl = do
        isEof <- liftIO $$ hIsEOF hdl
        if isEof
            return () -- we're done now
            else (liftIO $$ hGetChar hdl >>= yield) >> readChar hdl
~~~~

`addCleanup` is nice and easy to use. But there is still a major problem. What happens if one of the handle functions (like `openFile`) throws an exception? To overcome this there is `bracketP` on the one side and a complete package `ResourceT` on the other side. 
If you are more interested in `ResourceT` take a look at [resourcet](https://hackage.haskell.org/package/resourcet).

## Resumable Conduits

In some cases it would be nice to pause a data procession and resume it later. Thats why conduits implements `ResumableSource`. Using the special functions (which looks similar to the normal one) it is possible to build resumable conduit systems like normal ones. The `$$$$+` operator generates a `ResumableSource` out of a `Source` and a `Sink`. After other operations the processing can be continued with `$$$$++`. At the end, to avoid delayed cleanup, call `$$$$+-` which triggers _finalization_. For the sake of completeness there is also a generalization of `ResumableSource` which is called `ResumableConduit` but we will not looke into this generalisation.



That\'s it!

I hope I could help understanding the basics of the _conduit_ package.


## Further reading

- [FPComplete: Conduit Overview](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview)
- [Yesod Blog: Conduits Buffering](http://www.yesodweb.com/blog/2012/01/conduits-buffering)

- [Yesod Blog: Core flaws of pipes and conduit](http://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit)

- [https://hackage.haskell.org/package/conduit](https://hackage.haskell.org/package/conduit)
- [https://hackage.haskell.org/package/conduit-extra](https://hackage.haskell.org/package/conduit-extra)
- [http://hackage.haskell.org/package/http-conduit](http://hackage.haskell.org/package/http-conduit)
- [https://hackage.haskell.org/package/resourcet](https://hackage.haskell.org/package/resourcet)


