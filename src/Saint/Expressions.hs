{-# LANGUAGE GADTs #-}
module Saint.Expressions where

import Data.Maybe
import qualified Lambda.Abs as L
import Lambda.Par
import Lambda.ErrM (Err(..))

import Saint.Types

parse :: String -> Either String UntypedExpr
parse s = case pExpr (myLexer s) of
  Ok e  -> return $ go e
  Bad e -> Left e
  where
    go e = case e of
      L.EVar (L.Ident v)      -> UVar v
      L.ELam (L.Ident v) e    -> ULam v (go e)
      L.ELet (L.Ident v) e e' -> UApp (ULam v (go e')) (go e)
      L.EILit i               -> UILit (fromInteger i)
      L.EApp f x              -> UApp (go f) (go x)

data UntypedExpr where
  UVar  :: String -> UntypedExpr
  ULam  :: String -> UntypedExpr -> UntypedExpr
  UILit :: Int -> UntypedExpr
  UApp  :: UntypedExpr -> UntypedExpr -> UntypedExpr
  deriving (Ord, Eq, Show)

data STypedExpr ty where
  SVar  :: String -> STypedExpr ty
  SLam  :: String -> SType ty -> STypedExpr ty -> SType ty -> STypedExpr ty
  SILit :: Int -> STypedExpr ty
  SApp  :: STypedExpr ty -> STypedExpr ty -> STypedExpr ty

data Expr ty where
  Var  :: String  -> Expr ty
  Lam  :: String  -> ty a    -> Expr ty -> ty b -> Expr ty
  ILit :: Int     -> Expr ty
  App  :: Expr ty -> Expr ty -> Expr ty

someTypedToTyped :: FullType ty => STypedExpr ty -> Either String (Expr ty)
someTypedToTyped s = case s of
  SVar v -> return $ Var v
  SLam x t e t' -> do
    SBase ty  <- normST t
    SBase ty' <- normST t'
    Lam x ty <$> (someTypedToTyped e) <*> return ty'
  SILit i -> return $ ILit i
  SApp f x -> App <$> someTypedToTyped f <*> someTypedToTyped x
