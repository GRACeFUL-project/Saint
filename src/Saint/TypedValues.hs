{-# LANGUAGE GADTs, TypeOperators, UndecidableInstances, FlexibleContexts #-}
module Saint.TypedValues where

import Data.Either

import Saint.Types

data TypedValue f0 f1 where
  (:::) :: a -> Type f0 f1 a -> TypedValue f0 f1

infixr 8 :::

data Type f0 f1 a where
  Base   :: f0 a -> Type f0 f1 a
  Arity1 :: f1 (Type f0 f1) a -> Type f0 f1 a
  (:->)  :: Type f0 f1 a -> Type f0 f1 b -> Type f0 f1 (a -> b)

infixr 9 :->

instance (TypeEquality f0, TypeEquality (f1 (Type f0 f1))) => TypeEquality (Type f0 f1) where
  a ?= b = case (a, b) of
    (Base a, Base b) -> do
      Refl <- a ?= b
      return Refl
    (Arity1 a, Arity1 b) -> do
      Refl <- a ?= b
      return Refl
    (a :-> b,    x :-> y) -> do
      Refl <- a ?= x
      Refl <- b ?= y
      return Refl
    (_,          _)       -> fail "Type error"

instance HasFunctions (Type f0 f1) where
  (-->) = (:->)

instance IsType (Type f0 f1) where
  toSomeType (a :-> b) = SomeFun (toSomeType a) (toSomeType b)
  toSomeType a         = SomeBase a

instance (HasIntegers f0) => HasIntegers (Type f0 f1) where
  int = Base int

unpackAs :: TypeEquality (Type f0 f1) => Type f0 f1 a -> TypedValue f0 f1 -> Either String a
unpackAs t' (a ::: t) = do
  Refl <- t ?= t'
  return a

coerce :: TypeEquality (Type f0 f1) => Type f0 f1 a -> TypedValue f0 f1 -> a
coerce t a = case unpackAs t a of
  Left e  -> error e
  Right v -> v
