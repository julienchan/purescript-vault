module Data.Vault.Internal
  ( Unique
  , newUnique
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, modifyRef')

-- | Store as Number, so it can handle 53-bit signed integer
newtype Unique = Unique Number

derive newtype instance eqUnique :: Eq Unique
derive newtype instance ordUnique :: Ord Unique

uniqueSource :: Ref Number
uniqueSource = unsafePerformEff $ newRef 0.00

incrementSource :: Number -> { state :: Number, value :: Number }
incrementSource x = { state: x + one, value: x}

newUnique :: forall eff. Eff (ref :: REF | eff) Unique
newUnique = do
  r <- modifyRef' uniqueSource incrementSource
  pure $ Unique r
