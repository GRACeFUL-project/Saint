{-# LANGUAGE GADTs
           , TypeOperators
           , FlexibleContexts
           , KindSignatures
           , IncoherentInstances
           , UndecidableInstances
           , RankNTypes
#-}
module Saint.TypeBuildingBlocks where

import Prelude hiding (maybe)

import Saint.Types
import Saint.CoProducts

data A0 t (tu :: * -> *) a where
  A0 :: A0 t tu t

instance TypeEquality (A0 t tu) where
  A0 ?= A0 = return Refl 

instance (A0 Int) :< t => HasInts (Type t) where
  int = Base (inject A0)

data A1 f tu a where
  A1 :: tu a -> A1 f tu (f a)

instance TypeEquality tu => TypeEquality (A1 f tu) where
  A1 a ?= A1 b = do
    Refl <- a ?= b
    return Refl

bool :: A0 Bool :< t => Type t Bool
bool = Base (inject A0)

double :: A0 Double :< t => Type t Double
double = Base (inject A0)

maybe :: A1 Maybe :< t => Type t a -> Type t (Maybe a)
maybe = Base . inject . A1

list :: A1 [] :< t => Type t a -> Type t [a]
list = Base . inject . A1
