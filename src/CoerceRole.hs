{-# LANGUAGE FlexibleInstances, QuantifiedConstraints #-}

module CoerceRole where

import Data.Coerce (Coercible)

-- | Sometimes you are trying to derive type classes, but you get "role
-- errors". This type class allows you to derive them successfully by
-- asserting that the "role" of the thing you're trying to coerce is safe
-- to do so.
--
-- @since 0.0.1.0
class (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f
instance (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f

