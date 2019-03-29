# coerce-role

Trying to derive something and `role`s got you down?
Use this library/trick to work around it!

This library uses a trick from [How QuantifiedConstraints can let us put join back in Monad](https://ryanglscott.github.io/2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad/). 
It turns out, this trick is useful for many more things!
The first time I used it, it was for deriving the `MonadUnliftIO` class.
I wrote my own little monad transformer and tried to derive `MonadUnliftIO` using `GeneralizedNewtypeDeriving`:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import UnliftIO

newtype MyMonad m a = MyMonad (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)
```

However, GHC did not like this.
It gave me an error:

```
/home/matt/Projects/coerce-role/test/Spec.hs:10:53: error:
    • Couldn't match representation of type ‘m (UnliftIO (MyIO m))’
                               with that of ‘m (UnliftIO m)’
        arising from the coercion of the method ‘askUnliftIO’
          from type ‘m (UnliftIO m)’ to type ‘MyIO m (UnliftIO (MyIO m))’
      NB: We cannot know what roles the parameters to ‘m’ have;
        we must assume that the role is nominal
    • When deriving the instance for (MonadUnliftIO (MyIO m))
   |        
10 |     deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)
   |                                                     ^^^^^^^^^^^^^
```

Fortunately, this is the same error from the above blog post!
So I did a `StandaloneDeriving` instance to specify the context:

```haskell
{-# LANGUAGE RankNTypes, QuantifiedConstraints, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

import UnliftIO
import Data.Coerce (Coercible)

newtype MyMonad m a = MyMonad (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (MonadUnliftIO m, forall a b. Coercible a b => Coercible (m a) (m b)) => MonadUnliftIO (MyIO m)
```

This is kind of a lot of extra stuff to add.
The third time I ended up needing this, I decided to write a library and save myself the boilerplate.

Now, you can write:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Main where

import UnliftIO

import CoerceRole

newtype MyIO m a = MyIO (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (MonadUnliftIO m, CoerceRole m) => MonadUnliftIO (MyIO m)
```

which is a lot simpler and easier.
