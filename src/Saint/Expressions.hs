{-# LANGUAGE GADTs, FlexibleContexts #-}
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

data STypedExpr tr where
  SVar  :: String -> STypedExpr tr
  SLam  :: String -> SType tr -> STypedExpr tr -> SType tr -> STypedExpr tr
  SILit :: Int -> STypedExpr tr
  SApp  :: STypedExpr tr -> STypedExpr tr -> STypedExpr tr

data Expr tr where
  Var  :: String  -> Expr tr
  Lam  :: String  -> tr a    -> Expr tr -> tr b -> Expr tr
  ILit :: Int     -> Expr tr
  App  :: Expr tr -> Expr tr -> Expr tr

someTypedToTyped :: FullType tr => STypedExpr tr -> Either String (Expr tr)
someTypedToTyped s = case s of
  SVar v -> return $ Var v
  SLam x t e t' -> do
    SBase tr  <- normST t
    SBase tr' <- normST t'
    Lam x tr <$> (someTypedToTyped e) <*> return tr'
  SILit i -> return $ ILit i
  SApp f x -> App <$> someTypedToTyped f <*> someTypedToTyped x
