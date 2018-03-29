{-# LANGUAGE GADTs, FlexibleContexts #-}
module Saint.Interpreter where

import Data.Maybe
import Data.Either

import Saint.Types
import Saint.TypedValues
import Saint.Expressions

type Env t = String -> Maybe (TypedValue t)

empty :: Env t
empty _ = Nothing

extend :: Env t -> String -> TypedValue t -> Env t
extend e s tv s'
  | s' == s   = Just tv
  | otherwise = e s'

interpret :: FullType (Type t) => Env t -> Expr (Type t) -> Either String (TypedValue t)
interpret env e = case e of
  Var v     -> maybe (Left $ "The impossible happened, unbound variable during evaluation: " ++ show v) return $ env v
  Lam s t e t' -> return $ (\x -> coerce t' $ either (error "The impossible happened") id $ interpret (extend env s (x ::: t)) e) ::: t --> t'
  ILit i -> return $ i ::: int
  App f a   -> do
    f' <- interpret env f
    a' <- interpret env a
    app f' a'

app :: FullType (Type t) => TypedValue t -> TypedValue t -> Either String (TypedValue t)
app (f ::: a :-> b) (x ::: a') = do
  Refl <- a ?= a' 
  return $ f x ::: b
app _ _ = Left "Trying to apply a function"
