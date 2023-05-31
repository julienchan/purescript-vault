module Data.Vault.Internal
  ( Unique
  , newUnique
  , UniqueMap
  , empty
  , lookup
  , insert
  , union
  , deleteUnique
  ) where

import Effect (Effect)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe)

foreign import data Unique :: Type

foreign import newUnique :: Effect Unique

foreign import data UniqueMap :: Type -> Type

foreign import empty :: forall a. UniqueMap a

foreign import deleteUnique
  :: forall a
   . Fn.Fn2
      Unique
      (UniqueMap a)
      (UniqueMap a)

foreign import lookup
  :: forall a
   . Fn.Fn4
      (forall x. Maybe x)
      (forall x. x -> Maybe x)
      Unique
      (UniqueMap a)
      (Maybe a)

foreign import insert
  :: forall a
   . Fn.Fn3
      Unique
      a
      (UniqueMap a)
      (UniqueMap a)

foreign import union
  :: forall a
   . Fn.Fn2
      (UniqueMap a)
      (UniqueMap a)
      (UniqueMap a)
