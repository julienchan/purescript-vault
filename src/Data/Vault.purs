module Data.Vault
  ( Vault
  , Key
  , empty
  , newKey
  , insert
  , lookup
  , delete
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, modifyRef')

import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Vault.Internal (Unique, newUnique)

newtype Vault = Vault (M.Map Unique Item)

data Key a = Key Unique (Item' a)

type Item = Eff (ref :: REF) Unit

type Item' a = Ref (Maybe a)

-- | The empty vault
empty :: Vault
empty = Vault M.empty

newKey :: forall eff a. Eff (ref :: REF | eff) (Key a)
newKey = do
  k <- newUnique
  ref <- newRef Nothing
  pure $ Key k ref

insert :: forall a. Key a -> a -> Vault -> Vault
insert (Key k ref) a = over Vault $ M.insert k (writeRef ref (Just a))

delete :: forall a. Key a -> Vault -> Vault
delete (Key k _) = over Vault $ M.delete k

lookup :: forall eff a. Key a -> Vault -> Eff (ref :: REF | eff) (Maybe a)
lookup (Key k ref) (Vault m) = case M.lookup k m of
  Nothing  -> pure Nothing
  Just act -> do
    _ <- coerceVaultItem act
    modifyRef' ref rollItem

coerceVaultItem :: forall eff a. Eff (ref :: REF) a -> Eff (ref :: REF | eff) a
coerceVaultItem = unsafeCoerceEff

rollItem :: forall a. Maybe a -> { state :: Maybe a, value :: Maybe a }
rollItem a = { state: Nothing, value: a }

derive instance newtypeVault :: Newtype Vault _

instance eqKey :: Eq (Key a) where
  eq (Key a _) (Key b _) = a == b

instance ordKey :: Ord (Key a) where
  compare (Key a _) (Key b _) = compare a b
