{-# LANGUAGE GADTs, FlexibleContexts #-}
module Saint.Library where

import qualified Data.Map as M

import Saint.Types
import Saint.TypedValues
import Saint.TypeChecker
import Saint.Expressions
import Saint.Interpreter

data Item t = Item String (TypedValue t)
data Library t = Library String [Item t]

makeTypingEnv :: Library t -> M.Map String (Scheme (Type t))
makeTypingEnv (Library _ []) = M.empty
makeTypingEnv (Library _ (Item s (_ ::: t) : ls)) = M.insert s (Scheme [] $ toSomeType t) (makeTypingEnv $ Library "" ls)

makeEnv :: Library t -> Env t 
makeEnv (Library _ []) = empty
makeEnv (Library _ (Item s tv : ls)) = extend (makeEnv (Library "" ls)) s tv

interpretIn :: FullType (Type t) => Library t -> UntypedExpr -> Either String (TypedValue t)
interpretIn lib ue = do
  let (eith, _) = runTI $ typeInference (makeTypingEnv lib) ue
  ste <- eith
  e   <- someTypedToTyped (snd ste)
  interpret (makeEnv lib) e

run :: FullType (Type t) => Type t a -> Library t -> String -> Either String a
run t lib s = do
  pexp <- parse s
  (result ::: t') <- interpretIn lib pexp
  Refl <- t ?= t'
  return result
