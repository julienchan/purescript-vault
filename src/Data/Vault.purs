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

import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Vault.Internal (Unique, newUnique)

-- | A persistent store for values of arbitrary types.
newtype Vault = Vault (M.Map Unique Item)

-- | Key for an item in vault
data Key a = Key Unique (Item' a)

type Item = Eff (ref :: REF) Unit

type Item' a = Ref (Maybe a)

-- | The empty vault
empty :: Vault
empty = Vault M.empty

-- | Create a new key for use with a vault.
newKey :: forall eff a. Eff (ref :: REF | eff) (Key a)
newKey = do
  k <- newUnique
  ref <- newRef Nothing
  pure $ Key k ref

-- | Insert a value for a given key. Overwrites any previous value.
insert :: forall a. Key a -> a -> Vault -> Vault
insert (Key k ref) a (Vault m) = Vault (M.insert k (writeRef ref (Just a)) m)

-- | Delete a key from the vault.
delete :: forall a. Key a -> Vault -> Vault
delete (Key k _) (Vault m) = Vault (M.delete k m)

-- | Lookup the value of a key in the vault.
lookup :: forall a. Key a -> Vault -> Maybe a
lookup (Key k ref) (Vault m) = case M.lookup k m of
  Nothing  -> Nothing
  Just act -> unsafePerformEff do
    _ <- act
    modifyRef' ref rollItem

union :: Vault -> Vault -> Vault
union (Vault m) (Vault n) = Vault (M.union m n)

rollItem :: forall a. Maybe a -> { state :: Maybe a, value :: Maybe a }
rollItem a = { state: Nothing, value: a }
