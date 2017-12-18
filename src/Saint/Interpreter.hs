{-# LANGUAGE GADTs, FlexibleContexts #-}
module Saint.Interpreter where

import Data.Maybe

import Saint.Types
import Saint.TypedValues
import Saint.Expressions

type Env f0 f1 = String -> Maybe (TypedValue f0 f1)

empty :: Env f0 f1
empty _ = Nothing

extend :: Env f0 f1 -> String -> TypedValue f0 f1 -> Env f0 f1
extend e s tv s'
  | s' == s   = Just tv
  | otherwise = e s'

interpret :: FullType (Type f0 f1) => Env f0 f1 -> Expr (Type f0 f1) -> Maybe (TypedValue f0 f1)
interpret env e = case e of
  Var v     -> env v
  Lam s t e t' -> return $ (\x -> coerce t' $ fromJust $ interpret (extend env s (x ::: t)) e) ::: t :-> t'
  ILit i -> return $ i ::: int
  App f a   -> do
    f' <- interpret env f
    a' <- interpret env a
    app f' a'

app :: FullType (Type f0 f1) => TypedValue f0 f1 -> TypedValue f0 f1 -> Maybe (TypedValue f0 f1)
app (f ::: a :-> b) (x ::: a') = do
  Refl <- case a ?= a' of
    Left _  -> Nothing
    Right r -> return r
  return $ f x ::: b
app _ _ = Nothing
