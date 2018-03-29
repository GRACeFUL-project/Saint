{-# LANGUAGE GADTs, TypeOperators, ConstraintKinds, UndecidableInstances, FlexibleContexts, KindSignatures #-}
module Saint.Types where

import Data.Either

import Saint.CoProducts

data a :~: b where
  Refl :: a :~: a

instance Show (a :~: b) where
  show Refl = "Refl"

class IsType ty where
  toSomeType :: ty a -> SomeType ty

class TypeEquality ty where
  (?=) :: ty a -> ty b -> Either String (a :~: b)

class HasFunctions ty where
  (-->) :: ty a -> ty b -> ty (a -> b)

infixr 1 -->

class HasInts ty where
  int :: ty Int

type FullType ty = (IsType ty, TypeEquality ty, HasFunctions ty, HasInts ty)

data SomeType ty where
  SomeBase :: ty a        -> SomeType ty
  SomeVar  :: Int         -> SomeType ty
  SomeFun  :: SomeType ty -> SomeType ty -> SomeType ty

instance TypeEquality ty => Eq (SomeType ty) where
  SomeBase t  == SomeBase t' = isRight $ t ?= t'
  SomeVar  v  == SomeVar  v' = v == v'
  SomeFun a b == SomeFun c d = a == c && b == d
  _ == _ = False

normST :: HasFunctions ty => SomeType ty -> Either String (SomeType ty)
normST (SomeFun a b) = do
  SomeBase a' <- normST a
  SomeBase b' <- normST b
  return $ SomeBase (a' --> b')
normST (SomeVar v) = Left "The impossible happened"
normST t = return t

data Type t a where
  Base   :: t (Type t) a -> Type t a
  Tag    :: String -> Type t a -> Type t a
  (:->)  :: Type t a -> Type t b -> Type t (a -> b)

infixr 9 :->

instance TypeEquality (t (Type t)) => TypeEquality (Type t) where
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

instance HasFunctions (Type t) where
  (-->) = (:->)

instance IsType (Type t) where
  toSomeType (a :-> b) = SomeFun (toSomeType a) (toSomeType b)
  toSomeType a         = SomeBase a

instance (TypeEquality (f t), TypeEquality (g t)) => TypeEquality (CoProduct f g t) where
  InL f ?= InL f' = do
    Refl <- f ?= f'
    return Refl
  InR g ?= InR g' = do
    Refl <- g ?= g'
    return Refl
  _ ?= _         = Left "Type error"
