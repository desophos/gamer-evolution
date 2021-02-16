{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import Data.Typeable
import Evolution (Agent(..))

instance (Typeable a, Typeable b) => Show (a -> b) where
    showsPrec _ _ = showParen True $ showString "_ :: " . (showsTypeRep . typeRep) p
        where p = Proxy :: Proxy (a -> b)

deriving instance (Show a, Typeable a) => Show (Agent a)
