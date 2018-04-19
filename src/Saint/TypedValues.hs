{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts #-}
module Saint.TypedValues where

import Data.Either

import Saint.Types

data TypedValue t where
  (:::) :: a -> AnnTypeRep t a -> TypedValue t

infixr 0 :::

unpackAs :: TypeEquality (AnnTypeRep t) => AnnTypeRep t a -> TypedValue t -> Either String a
unpackAs t' (a ::: t) = do
  Refl <- t ?= t'
  return a

coerce :: TypeEquality (AnnTypeRep t) => AnnTypeRep t a -> TypedValue t -> a
coerce t a = case unpackAs t a of
  Left e  -> error e
  Right v -> v
