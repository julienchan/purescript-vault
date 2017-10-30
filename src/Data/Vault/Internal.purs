module Data.Vault.Internal
  ( Unique
  , newUnique
  , UniqueMap
  , empty
  , lookup
  , insert
  , union
  , delete
  ) where

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe)

foreign import data Unique :: Type

foreign import newUnique :: forall eff. Eff eff Unique

foreign import data UniqueMap :: Type -> Type

foreign import empty :: forall a. UniqueMap a

foreign import delete
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
