module Data.Vault
  ( Vault
  , Key
  , empty
  , newKey
  , insert
  , lookup
  , delete
  , union
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, modifyRef')

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Vault.Internal as V

-- | A persistent store for values of arbitrary types.
newtype Vault = Vault (V.UniqueMap Item)

-- | Key for an item in vault
data Key a = Key V.Unique (Item' a)

type Item = Eff (ref :: REF) Unit

type Item' a = Ref (Maybe a)

-- | The empty vault
empty :: Vault
empty = Vault V.empty

-- | Create a new key for use with a vault.
newKey :: forall eff a. Eff (ref :: REF | eff) (Key a)
newKey = do
  k <- V.newUnique
  ref <- newRef Nothing
  pure $ Key k ref

-- | Insert a value for a given key. Overwrites any previous value.
insert :: forall a. Key a -> a -> Vault -> Vault
insert (Key k ref) a (Vault m) = Vault (Fn.runFn3 V.insert k (writeRef ref (Just a)) m)

-- | Delete a key from the vault.
delete :: forall a. Key a -> Vault -> Vault
delete (Key k _) (Vault m) = Vault (Fn.runFn2 V.delete k m)

-- | Lookup the value of a key in the vault.
lookup :: forall a. Key a -> Vault -> Maybe a
lookup (Key k ref) (Vault m) = case Fn.runFn4 V.lookup Nothing Just k m of
  Nothing  -> Nothing
  Just act -> unsafePerformEff do
    _ <- act
    modifyRef' ref rollItem

union :: Vault -> Vault -> Vault
union (Vault m) (Vault n) = Vault (Fn.runFn2 V.union m n)

rollItem :: forall a. Maybe a -> { state :: Maybe a, value :: Maybe a }
rollItem a = { state: Nothing, value: a }
