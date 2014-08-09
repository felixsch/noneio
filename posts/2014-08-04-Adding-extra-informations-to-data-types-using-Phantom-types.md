---
title: Adding extra informations to data types using Phantom types
description: 
tags: Haskell, Phantom types, programming
---
___Phantom types_ are ordinary `data` or `newtype` types with an extra parameter on the left side of the data definition which is _not_ used on the right side.__
```{.haskell}
data PhantomizedType a = PhantomizedType
```

###But why is this useful?

In some cases functions, within an implementation excepting that data comes in an explicit state. For example if a web page takes user input. This input needs to be sanitized to remove malicious input. All functions which are called after the sanitize function expecting that the information is already sanitized. This information is usually shipped implicit via control structures, flags, etc. The problem: You need to detect if data is not well formed and handle this error on runtime.

_Phantom types_ eliminate this problem + lifting error detection to compile time!

An instructional example:

```{.haskell .numberLines}
import Data.Char

type Op = (Int -> Int -> Int)
type Message = String

julius :: Op -> Int -> String -> String
julius _  _ []      = []
julius op n (x:xs) = letter x : julius op n xs
    where
        letter = chr . (`op` n) . ord

encrypt :: Int -> Message -> Message
encrypt = julius (+)

decrypt :: Int -> Message -> Message
decrypt = julius (-)

send :: Message -> IO ()
send = putStrLn

receive :: IO Message
receive = readLn

main :: IO ()
main = send $$ encrypt 3 "Hello world"
```

As you can see in this example data is "encrypted" and "sent" to `stdout`. This is just fine.
`Message` is a type synonym to `String`. Imagine `send` should __only__ send an encrypted string to `stdout` and `decrypt` should ensure that `Message` is encrypted and not cleartext. Achieving this is tedious and error prone.
With phantom types we can add this information to the type system which checks at compile time(!) that everything is in right order.

Rewriting the message declaration using Phantom types is easy:

```{.haskell}
  data Message a = Message String
  
  data Encrypted
  data Cleartext
  
  newMessage :: String -> Message Cleartext
  newMessage = Message
```
In combination with `empty types` we're able to add information about the state of the `Message` to the type system (awesome!).

Transforming the example using Phantom Types is not hard:

```{.haskell .numberLines}
julius :: Op -> Int -> String -> String
julius _  _ []      = []
julius op n (x:xs) = letter x : julius op n xs
    where
        letter = chr . (`op` n) . ord

encrypt :: Int -> Message Cleartext -> Message Encrypted
encrypt n (Message m) = Message $$ julius (+) n m

decrypt :: Int -> Message Encrypted -> Message Cleartext
decrypt n (Message m) = Message $$ julius (-) n m

send :: Message Encrypted -> IO ()
send (Message m) = putStrLn m

receive :: IO (Message Encrypted)
receive = Message <$$> readLn

main :: IO ()
main = send $$ encrypt 7 $$ newMessage "Hello Haskell"
```

With phantom types it is not possible to send a cleartext message to `stdout`.

## Why not use `type` instead and what is the runtime cost?

It's possible to use a type synonym instead of `data` or `newtype`. But it makes no sense because the compiler first expand's type synonyms. This means `Message Cleartext` and `Message Encrypted` would both be expanded to `String`, which defeats the use case of _Phantom types_.

Guess what: _Phantom types_ comes without runtime costs because there are erased by the compiler at compile time. This means they come for free (at runtime).

## There's another way around

_Phantom types_ are not the only way to achieve this extra information in haskell's type system. With _GADTs_ it's also possible to implement the same behavior:

```{.haskell .numberLines}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

import Control.Applicative
import Data.Char

type Op = (Int -> Int -> Int)

data MState = Cleartext | Encrypted

data Message :: MState -> * where
    EncryptedMessage :: String -> Message Encrypted
    CleartextMessage :: String -> Message Cleartext

newMessage :: String -> Message Cleartext
newMessage = CleartextMessage


julius :: Op -> Int -> String -> String
julius _  _ []      = []
julius op n (x:xs) = letter x : julius op n xs
    where
        letter = chr . (`op` n) . ord

encrypt :: Int -> Message Cleartext -> Message Encrypted
encrypt n (CleartextMessage m) = EncryptedMessage $$ julius (+) n m

decrypt :: Int -> Message Encrypted -> Message Cleartext
decrypt n (EncryptedMessage m) = CleartextMessage $$ julius (-) n m

send :: Message Encrypted -> IO ()
send (EncryptedMessage m) = putStrLn m

receive :: IO (Message Encrypted)
receive = EncryptedMessage <$$> readLn

main :: IO ()
main = send $$ encrypt 7 $$ newMessage "Hello Haskell"
```


