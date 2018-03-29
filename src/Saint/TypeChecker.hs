{- THIS CODE IS ADAPTED FROM THE IMPLEMENTATION HERE: https://raw.githubusercontent.com/mgrabmueller/AlgorithmW/master/AlgorithmW.lhs -}
{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Saint.TypeChecker where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import Saint.Types
import Saint.Expressions

data Scheme ty = Scheme [Int] (SomeType ty)

class Types ty a | a -> ty where
    ftv   :: a -> Set.Set Int
    apply :: Subst ty -> a -> a

instance FullType ty => Types ty (SomeType ty) where
    ftv (SomeVar n)     = Set.singleton n
    ftv (SomeFun t1 t2) = ftv t1 `Set.union` ftv t2
    ftv _               = Set.empty

    apply s (SomeVar n) = case Map.lookup n s of
                            Nothing  -> SomeVar n
                            Just t   -> t
    apply s (SomeFun t1 t2) = SomeFun (apply s t1) (apply s t2)
    apply s t               = t

instance FullType ty => Types ty (Scheme ty) where
    ftv (Scheme vars t)     = (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

type Subst ty = Map.Map Int (SomeType ty)

nullSubst :: FullType ty => Subst ty
nullSubst = Map.empty

composeSubst :: FullType ty => Subst ty -> Subst ty -> Subst ty
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

newtype TypeEnv ty = TypeEnv (Map.Map String (Scheme ty))

remove :: FullType ty => TypeEnv ty -> String -> TypeEnv ty
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance FullType ty => Types ty (TypeEnv ty) where
    ftv (TypeEnv env)     = foldr Set.union Set.empty (map ftv (Map.elems env))
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: FullType ty => TypeEnv ty -> SomeType ty -> Scheme ty
generalize env t = Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

data TIState ty = TIState { tiSupply :: Int,
                            tiSubst :: Subst ty
                          }

type TI ty a = ExceptT String (State (TIState ty)) a

runTI :: FullType ty => TI ty a -> (Either String a, TIState ty)
runTI t = runState (runExceptT t) initTIState
  where initTIState = TIState{tiSupply = 0, tiSubst = Map.empty}

newTyVar :: FullType ty => TI ty (SomeType ty)
newTyVar = do
  s <- get
  put s { tiSupply = tiSupply s + 1}
  return (SomeVar $ tiSupply s)

instantiate :: FullType ty => Scheme ty -> TI ty (SomeType ty)
instantiate (Scheme vars t) = do nvars <- mapM (\ _ -> newTyVar) vars
                                 let s = Map.fromList (zip vars nvars)
                                 return $ apply s t

mgu :: FullType ty => SomeType ty -> SomeType ty -> TI ty (Subst ty)
mgu (SomeFun l r) (SomeFun l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s1 `composeSubst` s2)
mgu (SomeVar u) t = varBind u t
mgu t (SomeVar u) = varBind u t
mgu t1 t2
  | t1 == t2  = return Map.empty
  | otherwise = throwError $ "types do not unify"

varBind :: FullType ty => Int -> SomeType ty -> TI ty (Subst ty)
varBind u t | t == SomeVar u        = return nullSubst
            | u `Set.member` ftv t  = throwError $ "occurs check fails"
            | otherwise             = return (Map.singleton u t)

applyExp :: FullType ty => Subst ty -> SomeTypedExpr ty -> SomeTypedExpr ty
applyExp s e = case e of
  SVar v -> SVar v
  SLam x t e t' -> SLam x (apply s t) (applyExp s e) (apply s t')
  SILit i -> SILit i
  SApp f x -> SApp (applyExp s f) (applyExp s x)
  

ti :: FullType ty => TypeEnv ty -> UntypedExpr -> TI ty (Subst ty, SomeType ty, SomeTypedExpr ty)
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
        return (s1, SomeFun (apply s1 tv) t1, applyExp s1 (SLam n (apply s1 tv) e' t1))

ti env (UILit i) = return (nullSubst, toSomeType int, SILit i)

ti env (UApp e1 e2) =
    do  tv <- newTyVar
        (s1, t1, e1') <- ti env e1
        (s2, t2, e2') <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (SomeFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv, applyExp s3 (SApp e1' e2'))

typeInference :: FullType ty => Map.Map String (Scheme ty) -> UntypedExpr -> TI ty (SomeType ty, SomeTypedExpr ty)
typeInference env e =
    do (s, t, e) <- ti (TypeEnv env) e
       return (apply s t, applyExp s e)
