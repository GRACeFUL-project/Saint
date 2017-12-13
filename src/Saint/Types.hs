{-# LANGUAGE GADTs, TypeOperators, ConstraintKinds #-}
module Saint.Types where

import Data.Maybe

data a :~: b where
  Refl :: a :~: a

class IsType ty where
  toSomeType :: ty a -> SomeType ty

class TypeEquality ty where
  (?=) :: ty a -> ty b -> Maybe (a :~: b)

class HasFunctions ty where
  (-->) :: ty a -> ty b -> ty (a -> b)

class HasIntegers ty where
  int :: ty Int

type FullType ty = (IsType ty, TypeEquality ty, HasFunctions ty, HasIntegers ty)

data SomeType ty where
  SomeBase :: ty a -> SomeType ty
  SomeVar  :: Int    -> SomeType ty
  SomeFun  :: SomeType ty -> SomeType ty -> SomeType ty

instance TypeEquality ty => Eq (SomeType ty) where
  SomeBase t == SomeBase t' = isJust $ t ?= t'
  SomeVar  v == SomeVar  v' = v == v'
  SomeFun a b == SomeFun c d = a == c && b == d
  _ == _ = False

normST :: HasFunctions ty => SomeType ty -> Maybe (SomeType ty)
normST (SomeFun a b) = do
  SomeBase a' <- normST a
  SomeBase b' <- normST b
  return $ SomeBase (a' --> b')
normST (SomeVar v) = fail "Type variable!"
normST t = return t
