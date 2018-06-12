{- THIS CODE IS ADAPTED FROM THE IMPLEMENTATION HERE: https://raw.githubusercontent.com/mgrabmueller/AlgorithmW/master/AlgorithmW.lhs -}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Saint.TypeChecker where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import Saint.Types
import Saint.Expressions

data Scheme tr = Scheme [Int] (SType tr)

class Types tr a | a -> tr where
    ftv   :: a -> Set.Set Int
    apply :: Subst tr -> a -> a

instance FullType tr => Types tr (SType tr) where
    ftv (STVar n)     = Set.singleton n
    ftv (SFun t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv _             = Set.empty

    apply s (STVar n) = case Map.lookup n s of
                            Nothing  -> STVar n
                            Just t   -> t
    apply s (SFun t1 t2) = SFun (apply s t1) (apply s t2)
    apply s t               = t

instance FullType tr => Types tr (Scheme tr) where
    ftv (Scheme vars t)     = (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

type Subst tr = Map.Map Int (SType tr)

nullSubst :: FullType tr => Subst tr
nullSubst = Map.empty

composeSubst :: FullType tr => Subst tr -> Subst tr -> Subst tr
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

newtype TypeEnv tr = TypeEnv (Map.Map String (Scheme tr))

remove :: FullType tr => TypeEnv tr -> String -> TypeEnv tr
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance FullType tr => Types tr (TypeEnv tr) where
    ftv (TypeEnv env)     = foldr Set.union Set.empty (map ftv (Map.elems env))
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: FullType tr => TypeEnv tr -> SType tr -> Scheme tr
generalize env t = Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

data TIState tr = TIState { tiSupply :: Int,
                            tiSubst :: Subst tr
                          }

type TI tr a = ExceptT String (State (TIState tr)) a

runTI :: FullType tr => TI tr a -> (Either String a, TIState tr)
runTI t = runState (runExceptT t) initTIState
  where initTIState = TIState{tiSupply = 0, tiSubst = Map.empty}

newTyVar :: FullType tr => TI tr (SType tr)
newTyVar = do
  s <- get
  put s { tiSupply = tiSupply s + 1}
  return (STVar $ tiSupply s)

instantiate :: FullType tr => Scheme tr -> TI tr (SType tr)
instantiate (Scheme vars t) = do nvars <- mapM (\ _ -> newTyVar) vars
                                 let s = Map.fromList (zip vars nvars)
                                 return $ apply s t

mgu :: FullType tr => SType tr -> SType tr -> TI tr (Subst tr)
mgu (SFun l r) (SFun l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s1 `composeSubst` s2)
mgu (STVar u) t = varBind u t
mgu t (STVar u) = varBind u t
mgu t1 t2
  | t1 == t2  = return Map.empty
  | otherwise = throwError $ "types do not unify." -- ++ show t1 ++ "/~" ++ show t2

varBind :: FullType tr => Int -> SType tr -> TI tr (Subst tr)
varBind u t | t == STVar u          = return nullSubst
            | u `Set.member` ftv t  = throwError $ "occurs check fails"
            | otherwise             = return (Map.singleton u t)

applyExp :: FullType tr => Subst tr -> STypedExpr tr -> STypedExpr tr
applyExp s e = case e of
  SVar v -> SVar v
  SLam x t e t' -> SLam x (apply s t) (applyExp s e) (apply s t')
  SILit i -> SILit i
  SApp f x -> SApp (applyExp s f) (applyExp s x)


ti ::  FullType tr =>
       TypeEnv tr -> UntypedExpr -> TI tr (Subst tr, SType tr, STypedExpr tr)
ti (TypeEnv env) (UVar n) =
    case Map.lookup n env of
       Nothing     -> throwError $ "unbound variable: " ++ n
       Just sigma  -> do t <- instantiate sigma
                         return (nullSubst, t, SVar n)

ti env (ULam n e) =
    do  tv <- newTyVar
        let TypeEnv env' = remove env n
            env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
        (s1, t1, e') <- ti env'' e
        return (s1, SFun (apply s1 tv) t1, applyExp s1 (SLam n (apply s1 tv) e' t1))

ti env (UILit i) = return (nullSubst, toSType int, SILit i)

ti env (UApp e1 e2) =
    do  tv <- newTyVar
        (s1, t1, e1') <- ti env e1
        (s2, t2, e2') <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (SFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv, applyExp s3 (SApp e1' e2'))

typeInference ::  FullType tr =>
                  Map.Map String (Scheme tr) -> UntypedExpr -> TI tr (SType tr, STypedExpr tr)
typeInference env e =
    do (s, t, e) <- ti (TypeEnv env) e
       return (apply s t, applyExp s e)
