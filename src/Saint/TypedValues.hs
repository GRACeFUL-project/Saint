{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts #-}
module Saint.TypedValues where

import Data.Either

import Saint.Types

data TypedValue t where
  (:::) :: a -> Type t a -> TypedValue t

infixr 0 :::

unpackAs :: TypeEquality (Type t) => Type t a -> TypedValue t -> Either String a
unpackAs t' (a ::: t) = do
  Refl <- t ?= t'
  return a

coerce :: TypeEquality (Type t) => Type t a -> TypedValue t -> a
coerce t a = case unpackAs t a of
  Left e  -> error e
  Right v -> v
