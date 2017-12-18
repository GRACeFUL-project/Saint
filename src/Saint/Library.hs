{-# LANGUAGE GADTs, FlexibleContexts #-}
module Saint.Library where

import qualified Data.Map as M

import Saint.Types
import Saint.TypedValues
import Saint.TypeChecker
import Saint.Expressions
import Saint.Interpreter

data Item f0 f1 = Item String (TypedValue f0 f1)
data Library f0 f1 = Library String [Item f0 f1]

makeTypingEnv :: Library f0 f1 -> M.Map String (Scheme (Type f0 f1))
makeTypingEnv (Library _ []) = M.empty
makeTypingEnv (Library _ (Item s (_ ::: t) : ls)) = M.insert s (Scheme [] $ toSomeType t) (makeTypingEnv $ Library "" ls)

makeEnv :: Library f0 f1 -> Env f0 f1
makeEnv (Library _ []) = empty
makeEnv (Library _ (Item s tv : ls)) = extend (makeEnv (Library "" ls)) s tv

interpretIn :: FullType (Type f0 f1) => Library f0 f1 -> UntypedExpr -> Either String (TypedValue f0 f1)
interpretIn lib ue = do
  let (eith, _) = runTI $ typeInference (makeTypingEnv lib) ue
  ste <- eith
  e   <- someTypedToTyped (snd ste)
  maybe (fail "Interpreter error") return $ interpret (makeEnv lib) e

run :: FullType (Type f0 f1) => Type f0 f1 a -> Library f0 f1 -> String -> Either String a
run t lib s = do
  pexp <- parse s
  (result ::: t') <- interpretIn lib pexp
  Refl <- t ?= t'
  return result
