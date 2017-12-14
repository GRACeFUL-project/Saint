{-# LANGUAGE GADTs #-}
module Saint.Library where

import qualified Data.Map as M

import Saint.Types
import Saint.TypedValues
import Saint.TypeChecker
import Saint.Expressions
import Saint.Interpreter

data Item = Item String TypedValue
data Library = Library String [Item]

makeTypingEnv :: Library -> M.Map String (Scheme Type)
makeTypingEnv (Library _ []) = M.empty
makeTypingEnv (Library _ (Item s (_ ::: t) : ls)) = M.insert s (Scheme [] $ toSomeType t) (makeTypingEnv $ Library "" ls)

makeEnv :: Library -> Env
makeEnv (Library _ []) = empty
makeEnv (Library _ (Item s tv : ls)) = extend (makeEnv (Library "" ls)) s tv

interpretIn :: Library -> UntypedExpr -> Either String TypedValue
interpretIn lib ue = do
  let (eith, _) = runTI $ typeInference (makeTypingEnv lib) ue
  ste <- eith
  e   <- someTypedToTyped (snd ste)
  maybe (fail "Interpreter error") return $ interpret (makeEnv lib) e

run :: Type a -> Library -> String -> Either String a
run t lib s = do
  pexp <- parse s
  (result ::: t') <- interpretIn lib pexp
  Refl <- t ?= t'
  return result
