{-# LANGUAGE GADTs, TypeOperators, ConstraintKinds, UndecidableInstances, FlexibleContexts #-}
module Saint.Types where

import Data.Either

data a :~: b where
  Refl :: a :~: a

class IsType ty where
  toSomeType :: ty a -> SomeType ty

class TypeEquality ty where
  (?=) :: ty a -> ty b -> Either String (a :~: b)

class HasFunctions ty where
  (-->) :: ty a -> ty b -> ty (a -> b)

class HasIntegers ty where
  int :: ty Int

type FullType ty = (IsType ty, TypeEquality ty, HasFunctions ty, HasIntegers ty)

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
normST (SomeVar v) = fail "Type variable!"
normST t = return t

data Type t a where
  Base   :: t (Type t) a -> Type t a
  (:->)  :: Type t a -> Type t b -> Type t (a -> b)

infixr 9 :->

instance TypeEquality (t (Type t)) => TypeEquality (Type t) where
  a ?= b = case (a, b) of
    (Base a, Base b) -> do
      Refl <- a ?= b
      return Refl

    (a :-> b,    x :-> y) -> do
      Refl <- a ?= x
      Refl <- b ?= y
      return Refl

    (_,          _)       -> fail "Type error"

instance HasFunctions (Type t) where
  (-->) = (:->)

instance IsType (Type t) where
  toSomeType (a :-> b) = SomeFun (toSomeType a) (toSomeType b)
  toSomeType a         = SomeBase a

instance HasIntegers (t (Type t)) => HasIntegers (Type t) where
  int = Base int
