{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts #-}
module Saint.TypedValues where

import Data.Either

import Saint.Types

data TypedValue f0 f1 where
  (:::) :: a -> Type f0 f1 a -> TypedValue f0 f1

infixr 8 :::

unpackAs :: TypeEquality (Type f0 f1) => Type f0 f1 a -> TypedValue f0 f1 -> Either String a
unpackAs t' (a ::: t) = do
  Refl <- t ?= t'
  return a

coerce :: TypeEquality (Type f0 f1) => Type f0 f1 a -> TypedValue f0 f1 -> a
coerce t a = case unpackAs t a of
  Left e  -> error e
  Right v -> v
