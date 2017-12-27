{-# LANGUAGE GADTs, TypeOperators, ConstraintKinds, UndecidableInstances, FlexibleContexts #-}
module Saint.TypeBuildingBlocks where

import Saint.Types

data IntT tu a where
  Int :: IntT tu Int

instance HasIntegers (IntT tu) where
  int = Int

instance TypeEquality (IntT tu) where
  Int ?= Int = return Refl 

data MaybeT tu a where
  Maybe :: tu a -> MaybeT tu (Maybe a)

instance TypeEquality tu => TypeEquality (MaybeT tu) where
  Maybe a ?= Maybe b = do
    Refl <- a ?= b
    return Refl
