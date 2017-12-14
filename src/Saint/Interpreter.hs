{-# LANGUAGE GADTs #-}
module Saint.Interpreter where

import Data.Maybe

import Saint.Types
import Saint.TypedValues
import Saint.Expressions

type Env = String -> Maybe TypedValue

empty :: Env
empty _ = Nothing

extend :: Env -> String -> TypedValue -> Env
extend e s tv s'
  | s' == s   = Just tv
  | otherwise = e s'

interpret :: Env -> Expr Type -> Maybe TypedValue
interpret env e = case e of
  Var v     -> env v
  Lam s t e t' -> return $ (\x -> coerce t' $ fromJust $ interpret (extend env s (x ::: t)) e) ::: t :-> t'
  ILit i -> return $ i ::: Int
  App f a   -> do
    f' <- interpret env f
    a' <- interpret env a
    app f' a'

app :: TypedValue -> TypedValue -> Maybe TypedValue
app (f ::: a :-> b) (x ::: a') = do
  Refl <- case a ?= a' of
    Left _  -> Nothing
    Right r -> return r
  return $ f x ::: b
app _ _ = Nothing
