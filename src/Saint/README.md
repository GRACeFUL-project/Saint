
-- Codes for types, based on another type of codes for base types
--   implicitly forall-quantified
data SomeType ty where
  SomeBase :: ty a        -> SomeType ty
  SomeVar  :: Int         -> SomeType ty
  SomeFun  :: SomeType ty -> SomeType ty -> SomeType ty

-- For efficiency, cashes the variables used in the type code
data Scheme ty = Scheme [Int] (SomeType ty)

data UntypedExpr where
  UVar  :: String -> UntypedExpr
  ULam  :: String -> UntypedExpr -> UntypedExpr
  UILit :: Int -> UntypedExpr
  UApp  :: UntypedExpr -> UntypedExpr -> UntypedExpr
  deriving (Ord, Eq, Show)

data SomeTypedExpr ty where
  SVar  :: String -> SomeTypedExpr ty
  SLam  :: String -> SomeType ty -> SomeTypedExpr ty -> SomeType ty -> SomeTypedExpr ty
  SILit :: Int -> SomeTypedExpr ty
  SApp  :: SomeTypedExpr ty -> SomeTypedExpr ty -> SomeTypedExpr ty

data Expr ty where
  Var  :: String  -> Expr ty
  ILit :: Int     -> Expr ty
  App  :: Expr ty -> Expr ty -> Expr ty
  Lam  :: String  -> ty a    -> Expr ty -> ty b -> Expr ty
