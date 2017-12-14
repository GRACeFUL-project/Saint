{-# LANGUAGE GADTs, TypeOperators #-}
module Saint.TypedValues where

import Data.Either

import Saint.Types

data TypedValue where
  (:::) :: a -> Type a -> TypedValue

infixr 8 :::

instance Show TypedValue where
  show (a ::: t) = "<<" ++ show t ++ ">>"

data Type a where
  Unit       :: Type ()
  Int        :: Type Int
  Bool       :: Type Bool
  List       :: Type a -> Type [a]
  (:->)      :: Type a -> Type b -> Type (a -> b)

infixr 9 :->

-- TODO: Precedence
instance Show (Type a) where
  show Unit      = "()"
  show Int       = "Int"
  show Bool      = "Bool"
  show (List a)  = "[" ++ show a ++ "]"
  show (a :-> b) = "(" ++ show a ++ ") -> (" ++ show b ++ ")"

instance TypeEquality Type where
  a ?= b = case (a, b) of
    (Int,        Int)     -> return Refl
    (Bool,       Bool)    -> return Refl
    (List a,   List b)    -> do
      Refl <- a ?= b
      return Refl
    (a :-> b,    x :-> y) -> do
      Refl <- a ?= x
      Refl <- b ?= y
      return Refl
    (_,          _)       -> fail "Type error"

instance HasFunctions Type where
  (-->) = (:->)

instance IsType Type where
  toSomeType (a :-> b) = SomeFun (toSomeType a) (toSomeType b)
  toSomeType a         = SomeBase a

instance HasIntegers Type where
  int = Int

unpackAs :: Type a -> TypedValue -> Either String a
unpackAs t' (a ::: t) = do
  Refl <- t ?= t'
  return a

coerce :: Type a -> TypedValue -> a
coerce t a = case unpackAs t a of
  Left e  -> error e
  Right v -> v
