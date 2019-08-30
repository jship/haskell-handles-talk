---
title: Handle Pattern
author: Jason Shipman
patat:
  wrap: true
  margins:
    left: 10
    right: 10
  incrementalLists: true
...

# Handle Pattern in Haskell

## Outline

<!-- Set terminal width x height to 100x32, and remember to ENHANCE!!!  -->

Points we're gonna hit:

- 🤔 What is it?
- 🧐 What does it look like?
- 😀 Why do I like it?

. . .

What we're not gonna hit:

- 🔥 An effect system flame war

# 🤔 What is it?

## High-level gist

The handle pattern is one of many approaches to writing code that does
IO, with an emphasis on:

- Encapsulating state in records
- Protecting changes to state behind functions, instead of exposing state
  directly
- Grouping records alongside those functions that modify their state

. . .

So this is an OOP talk and not an FP one?

. . .

`(╯°□°）╯︵ ┻━┻ `

. . .

The `Handle` pattern certainly borrows some OOP concepts, but the ones it
does borrow are fairly common across software engineering - regardless of
paradigm.

. . .

The pattern also advocates for a couple conventions:

- A well-defined module layout
- Static, always-parsable config

. . .

Not invented here!

<https://jaspervdj.be/posts/2018-03-08-handle-pattern.html>

# 🧐 What does it look like?

## Context

The implementation of the pattern we'll be walking through is for an
extremely simplified user database of sorts.

The point here is to explain the module structure and conventions of
the pattern and not to write a bunch of IO code that actually talks to
`postgres`.

. . .

```haskell
module UserDatabase
  ( Config(..)

  , Handle

  , new
  , close
  , withHandle

  , createUser
  , deleteUser
  ) where
```

. . .

Note that `Handle` modules are designed to be imported qualified. We don't have functions like `userDatabaseNew`, `userDatabaseCreateUser`, etc.

## Extensions

The following extensions will be used in our `Handle` implementation out of
convenience, but these are **not** required or part of the pattern in any way.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
```

## Imports

The most exciting slide in the whole talk.

```haskell
import Data.Aeson ((.:))
import Database.PostgreSQL.Config (PGPool, PostgresConf)
import Types (User(User), Email)  -- just some dummy types
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Config as PostgreSQL.Config
```

## Config

Before we can create a `Handle`, we need to specify a `Config` our `Handle` will use.

. . .

It is important that the fields in our `Config` are all static as this allows for:

- Painless `Config` creation in pure code
- Reading `Config` values straight from JSON, YAML, or w/e our application uses

. . .

```haskell
data Config = Config
  { postgresConf :: PostgresConf
  -- ...  more static fields go here
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON UserDatabase.Config" $ \obj ->
    fmap Config (obj .: "database")
```

## The Handle itself

The `Handle` itself can store whatever static and state-y fields we care about:

```haskell
data Handle = Handle
  { connectionPool :: PGPool
  -- ...  more static and state-y fields go here
  }
```

## Creating a Handle

A `new` function provides the single point for `Handle` creation.

. . .

Pretty much straight from the blog post:

```haskell
new
  :: Config         -- 1. Config
  -> _              -- 2. Dependencies (if any)
  -> IO Handle      -- 3. Result
```

. . .

The `new` function takes in the `Config` as well as any dependencies,
and initializes the `Handle`'s fields, providing a `Handle` value back.

```haskell
new :: Config -> IO Handle
new Config { postgresConf } = do
  connectionPool <- PostgreSQL.Config.createPGPool postgresConf
  pure Handle
    { connectionPool
    }
```

## Creating a Handle (cont'd)

Note that as we did not export the `Handle` constructor, clients of our
module can (so far) only get a `Handle` value via calling `new`.

```haskell
module UserDatabase
  ( -- ...
    Handle  -- constructor ain't exported!
    -- ...
  ) where
```

## Closing a Handle

A `close` function provides the single point for `Handle` clean up. Many
`Handle` implementations do not actually require special behavior in the
implementation of `close`, and can instead just rely on good ol' GC:

```haskell
close :: Handle -> IO ()
close = const (pure ())  -- or mempty if that's more your style
```

. . .

Including a `close` function as part of the module's interface provides
convenient backwards-compatibility if the `Handle` later becomes more
complex, and resource-freeing control is needed/would be useful for
clients.

## Create/close safety

A `withHandle` function provides a bracketed interface on top of `new`
and `close`:

```haskell
withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = Exception.bracket (new config) close
```

. . .

It has a similar argument order as our `new` function:

```haskell
withHandle
  :: Config           -- 1. Config
  -> _                -- 2. Dependencies (if any)
  -> (Handle -> IO a) -- 3. Callback
  -> IO a             -- 4. Result
```

. . .

When implementing the pattern, we may choose to only provide
`withHandle` in the "public" module and tuck `new` and `close` away in
an internal module.

. . .

This allows for clients in the general case to reach for the convenient
and safer `withHandle`, but also provides clients the power to work with
more exotic `Handle` lifetimes if needed via `new` and `close`.

## The Handle's specific interface

Functions to do the things **always** take in the `Handle` itself as the
first argument. We'll see why this is important later.

```haskell
createUser :: Handle -> Email -> IO User
createUser _handle _email = do
  -- actually make the user in postgres!
  pure User

deleteUser :: Handle -> Email -> IO ()
deleteUser _handle _email = do
  -- actually delete the user from postgres!
  pure ()
```

## Using the Handle

We create a `Config` value somehow, call `withHandle`, and then use the
functions comprising the `Handle`'s interface:

```haskell
module Main (main) where

import Types (Email(Email))
import qualified UserDatabase

main :: IO ()
main = do
  -- Read this in from a file or w/e
  let config = UserDatabase.Config { UserDatabase.postgresConf = undefined }

  UserDatabase.withHandle config $ \userDatabase -> do
    let dummyEmail = Email
    _user <- UserDatabase.createUser userDatabase dummyEmail
    UserDatabase.deleteUser userDatabase dummyEmail
```

## Extending the Handle

What if we want to extend our user database code somehow? Maybe we want
to add logging?

. . .

We have a natural process to do so, by adding a logger as a
dependency.

. . .

Remember our `new` function:

. . .

```haskell
new
  :: Config         -- 1. Config
  -> _              -- 2. Dependencies (if any)
  -> IO Handle      -- 3. Result
```

## Extending the Handle (cont'd)

Let's assume we have some logger that also happens to be implemented as
a `Handle`.

. . .

We first need to update our `Handle` type itself to carry the logger
around:

```haskell
-- ... import stuff
import qualified Logger

data Handle = Handle
  { connectionPool :: PGPool
  , logger :: Logger.Handle
  }
```

. . .

Then we update our `new` function to take in the logger as a dependency:

```haskell
new :: Config -> Logger.Handle -> IO Handle
new Config { postgresConf } logger = do
  connectionPool <- PostgreSQL.Config.createPGPool postgresConf
  pure Handle
    { connectionPool
    , logger
    }
```

## Extending the Handle (cont'd)

And finally, we update our `withHandle` function to pass the logger
dependency on to `new`:

```haskell
withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config logger = Exception.bracket (new config logger) close
```

. . .

Now, our `Handle`'s specific interface functions have convenient access
to a logger:

```haskell
createUser :: Handle -> Email -> IO User
createUser Handle { logger } email = do
  Logger.log logger $ "Creating user w/ email " <> show email
  -- actually make the user in postgres!
  pure User
```

## Using the extended Handle

Same as before, except now we initialize a logger handle too and pass that in.

```haskell
module Main (main) where

import Types (Email(Email))
import qualified Logger
import qualified UserDatabase

main :: IO ()
main = do
  -- Read these in from files or w/e
  let loggerConfig = Logger.Config { Logger.minLogLevel = undefined }
  let userDbConfig = UserDatabase.Config { UserDatabase.postgresConf = undefined }

  Logger.withHandle loggerConfig $ \logger ->
    UserDatabase.withHandle userDbConfig logger $ \userDatabase -> do
      let dummyEmail = Email
      _user <- UserDatabase.createUser userDatabase dummyEmail
      UserDatabase.deleteUser userDatabase dummyEmail
```

. . .

If the "staircasing" of `withHandle` calls gets on our nerves, we can
clean that up with `managed`/`Codensity`, but that is outside the scope
of this talk!

## Extending the Config

If we need to account for some new config parameter, our `Handle`'s
`Config` is also straightforward to extend.

. . .

We just need to update our `Config` type and the `FromJSON` instance:

```haskell
data Config = Config
  { postgresConf :: PostgresConf
  , retryCount  :: Int
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON UserDatabase.Config" $ \obj ->
    liftA2 Config (obj .: "database") (obj .: "retryCount")
```

## "Polymorphic" Handles

We may find ourselves needing to create multiple implementations of
a `Handle`'s specific interface, so in our case, implementations of
`createUser` and `deleteUser`.

. . .

When we need to more strongly separate the interface from the
implementation, we can do this by converting our `Handle` to a record of
functions (plus an action!):

. . .

```haskell
module UserDatabase
  ( Handle(..)
  ) where

import Types (User, Email) -- just some dummy types

data Handle = Handle
  { createUser :: Email -> IO User
  , deleteUser :: Email -> IO ()

  , close :: IO ()
  }
```

. . .

Here we have taken everything out of the `UserDatabase` module except
the `Handle`.  We are also now exporting the `Handle`'s constructor and
fields.

## "Polymorphic" Handles (cont'd)

We can create a specific implementation of our interface in its own module:

```haskell
module UserDatabase.Postgres
  ( module UserDatabase -- Re-export the Handle type, constructor, and fields
  , Config(..)
  , new
  , withHandle
  ) where
```

. . .

Our `Config` code stays the same, and it lives in the specific
implementation's module:

```haskell
data Config = Config
  { postgresConf :: PostgresConf
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON UserDatabase.Postgres.Config" $ \obj ->
    fmap Config (obj .: "database")
```

## "Polymorphic" Handles (cont'd)

Like our `Config` code, our `withHandle` function stays the same.  It
also lives in the specific implementation's module:

```haskell
withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config logger = Exception.bracket (new config logger) close
```

## "Polymorphic" Handles (cont'd)

Our `new` function now initializes the record of functions:

```haskell
new :: Config -> Logger.Handle -> IO Handle
new Config { postgresConf } _logger = do
  _connectionPool <- PostgreSQL.Config.createPGPool postgresConf
  pure Handle
    { createUser = \_email -> do
        -- actually make the user in postgres!
        pure User
    , deleteUser = \_email -> do
        -- actually delete the user from postgres!
        pure ()

    , close = pure ()
    }
```

## "Polymorphic" Handles (cont'd)

If the inline definitions of `createUser` and `deleteUser` get
hard to read (and `close` for that matter), we can split out their
implementations:

```haskell
new :: Config -> Logger.Handle -> IO Handle
new Config { postgresConf } logger = do
  connectionPool <- PostgreSQL.Config.createPGPool postgresConf
  pure Handle
    { createUser = createUserWith connectionPool logger
    , deleteUser = deleteUserWith connectionPool logger

    , close = pure ()
    }

createUserWith :: PGPool -> Logger.Handle -> Email -> IO User
createUserWith _connectionPool _logger _email = do
  -- actually make the user in postgres!
  pure User

deleteUserWith :: PGPool -> Logger.Handle -> Email -> IO ()
deleteUserWith _connectionPool _logger _email = do
  -- actually delete the user in postgres!
  pure ()
```

## Using a "polymorphic" handle

Back when we were talking about a `Handle`'s specific interface
functions, we said that functions to do the things **always** take in
the `Handle` itself as the first argument.

. . .

This requirement is what enables us to switch to "polymorphic"
handles when we want or need to do so.

. . .

In general, when switching to "polymorphic" handles, an import or two
gets changed, as well as the creation of the `Config`.  The creation
of the `Config` has to change because the `Config` is specific to a
particular implementation of the `Handle`'s interface.

. . .

In the case of our `postgres`-backed example where we only have one
implementation currently, we only need to change an `import` line in our
application code.


## Using a "polymorphic" handle (cont'd)

We import our specific implementation now, but the rest of the
application code stays the same:

```haskell
module Main (main) where

import Types (Email(Email))
import qualified Logger
import qualified UserDatabase.Postgres as UserDatabase  -- ta-da!

main :: IO ()
main = do
  -- Read these in from files or w/e
  let loggerConfig = Logger.Config { Logger.minLogLevel = undefined }
  let userDbConfig = UserDatabase.Config { UserDatabase.postgresConf = undefined }

  Logger.withHandle loggerConfig $ \logger ->
    UserDatabase.withHandle userDbConfig logger $ \userDatabase -> do
      let dummyEmail = Email
      _user <- UserDatabase.createUser userDatabase dummyEmail
      UserDatabase.deleteUser userDatabase dummyEmail
```

## When to use a "polymorphic" handle

It is up to us to use our best judgment on when to use "polymorphic"
handles.

. . .

Often times, we don't know the precise interface we should
be implementing against. In these cases, it may be better to YAGNI
"polymorphic" handles, at least until the interface is better
understood.

. . .

If the interface is well-understood, "polymorphic" handles can be
useful as we can wholesale swap out implementations at compile-time
or run-time.

. . .

"Polymorphic" handles also enable us to implement specific `Handle`
implementations in terms of other `Handle` implementations. For example,
consider that our existing user database `Handle` supported a new
`findUser` function:

. . .

```haskell
data Handle = Handle
  { -- ...
  , findUser   :: Email -> IO User
  , -- ...
  }
```

. . .

We could then implement a new user database `Handle` that caches the
found users, but delegates to our existing user database `Handle` for
actual communication with `postgres`.

## Quick Aside: Generalized, "polymorphic" handles

We may prefer to generalize "polymorphic" handles a bit:

. . .

```haskell
data HandleM m = Handle
  { createUser :: Email -> m User
  , deleteUser :: Email -> m ()
  , close :: m ()
  }
```

. . .

With this approach, we can recover our `IO`-based `Handle` via a type
synonym:

. . .

```haskell
type Handle = HandleM IO
```

. . .

**In my opinion**, generalizing a "polymorphic" handle is a case of
YAGNI and the type parameter nukes the simplicity of just passing
`Handle` values around.

. . .

For example, type `HandleM IO` is different from type `HandleM (Writer
Stuff)`, `HandleM (ReaderT Things IO)`, etc. We could get clever writing
natural transformations to do all sorts of conversions between `HandleM
f`'s and `HandleM g`'s, but it soon starts to feel like too much
complexity for my taste.

. . .

In the non-generalized case, `Handle` is all there is, which is
(arguably) considerably simpler.

## Things to watch out for

The `withHandle` function provides a desirable amount of safety over
using `new` and `close` directly, but `withHandle`, `new`, and `close`
are all unsafe to a certain extent.

. . .

We could accidentally do a "use-after-close", a "double-close", a
"double-new-without-close", etc.

. . .

Here is an example of the "use-after-close" problem:

. . .

```haskell
Logger.withHandle loggerConfig $ \logger -> do
  -- Uh-oh, we leaked the handle outside of the `withHandle` callback!
  userDatabase <- UserDatabase.withHandle userDbConfig logger pure
  UserDatabase.deleteUser userDatabase dummyEmail
```

. . .

It is hard to introduce these types of problems on accident though, so
**in my opinion**, the simplicity of the pattern far outweighs the risk
of introducing these problems.

# 😀 Why do I like it?

## It's opinions all the way down

In most cases, I like my Haskell like I don't like my movies:

. . .

> - Boring 😴
> - Free of surprises ☺️

. . .

For effectful, IO-based code, the `Handle` pattern goes above and beyond
being boring and free of surprises.

It also:

- Is exceedingly simple
- Provides great type errors
- Comes right out and says "Sup world, I'm doing IO!"
- Eliminates the retrofitting/option paralysis problem around the
  addition of config and state parameters
- Allows me to precisely decide the lifetime for state based on my
  application's needs
- Allows me to separate my interface and implementations when I deem
  it necessary/valuable, and no sooner
- Is all just good ol' `Haskell98`

## End

```
 ______________________
< Questions? Comments? >
 ----------------------
\                             .       .
 \                           / `.   .' "
  \                  .---.  <    > <    >  .---.
   \                 |    \  \ - ~ ~ - /  /    |
         _____          ..-~             ~-..-~
        |     |   \~~~\.'                    `./~~~/
       ---------   \__/                        \__/
      .'  O    \     /               /       \  "
     (_____,    `._.'               |         }  \/~~~/
      `----.          /       }     |        /    \__/
            `-.      |       /      |       /      `. ,~~|
                ~-.__|      /_ - ~ ^|      /- _      `..-'
                     |     /        |     /     ~-.     `-. _  _  _
                     |_____|        |_____|         ~ - . _ _ _ _ _>
```
