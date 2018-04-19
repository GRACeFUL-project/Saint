{-# LANGUAGE GADTs, TypeOperators, ConstraintKinds, UndecidableInstances, FlexibleContexts, KindSignatures #-}
module Saint.Types where

import Data.Either

import Saint.CoProducts

data a :~: b where
  Refl :: a :~: a

instance Show (a :~: b) where
  show Refl = "Refl"

class IsTypeRep tr where
  toSType :: tr a -> SType tr

class TypeEquality tr where
  (?=) :: tr a -> tr b -> Either String (a :~: b)

class HasFunctions tr where
  (-->) :: tr a -> tr b -> tr (a -> b)

infixr 1 -->

class HasInts tr where
  int :: tr Int

-- type FullType tr = (IsTypeRep tr, TypeEquality tr, HasFunctions tr, HasInts tr, Show (SType tr))
type FullType tr = (IsTypeRep tr, TypeEquality tr, HasFunctions tr, HasInts tr)

data SType tr where
  SBase :: tr a  -> SType tr
  STVar :: Int   -> SType tr
  SFun  :: SType tr -> SType tr -> SType tr

instance TypeEquality tr => Eq (SType tr) where
  SBase t  == SBase t'  = isRight $ t ?= t'
  STVar v  == STVar v'  = v == v'
  SFun a b == SFun c d  = a == c && b == d
  _ == _ = False

normST :: HasFunctions tr => SType tr -> Either String (SType tr)
normST (SFun a b) = do
  SBase a' <- normST a
  SBase b' <- normST b
  return $ SBase (a' --> b')
normST (STVar v) = Left "The impossible happened"
normST t = return t

data AnnTypeRep t a where
  Base   :: t (AnnTypeRep t) a -> AnnTypeRep t a
  Tag    :: String -> AnnTypeRep t a -> AnnTypeRep t a
  (:->)  :: AnnTypeRep t a -> AnnTypeRep t b -> AnnTypeRep t (a -> b)

infixr 9 :->

instance TypeEquality (t (AnnTypeRep t)) => TypeEquality (AnnTypeRep t) where
  a ?= b = case (a, b) of
    (Base a, Base b) -> do
      Refl <- a ?= b
      return Refl

    (Tag s a, _) -> a ?= b
    (_, Tag s b) -> a ?= b

    (a :-> b, x :-> y) -> do
      Refl <- a ?= x
      Refl <- b ?= y
      return Refl

    (_,          _)       -> Left "Type error"

instance HasFunctions (AnnTypeRep t) where
  (-->) = (:->)

instance IsTypeRep (AnnTypeRep t) where
  toSType (a :-> b) = SFun (toSType a) (toSType b)
  toSType a         = SBase a

instance (TypeEquality (f t), TypeEquality (g t)) => TypeEquality (CoProduct f g t) where
  InL f ?= InL f' = do
    Refl <- f ?= f'
    return Refl
  InR g ?= InR g' = do
    Refl <- g ?= g'
    return Refl
  _ ?= _         = Left "Type error"
