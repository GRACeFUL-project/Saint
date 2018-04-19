Old: Type t a          SomeType
New: AnnTypeRep t a    SType


-- Codes for types, based on another type of codes for base types
--   implicitly forall-quantified
data SType ty where
  SBase :: ty a        -> SType ty
  SVar  :: Int         -> SType ty
  SFun  :: SType ty -> SType ty -> SType ty

data Type t a where
  Base   :: t (Type t) a -> Type t a
  Tag    :: String -> Type t a -> Type t a
  (:->)  :: Type t a -> Type t b -> Type t (a -> b)


data Expr ty where
  Var  :: String  -> Expr ty
  ILit :: Int     -> Expr ty
  App  :: Expr ty -> Expr ty -> Expr ty
  Lam  :: String  -> ty a    -> Expr ty -> ty b -> Expr ty

-- For efficiency, cashes the variables used in the type code
data Scheme ty = Scheme [Int] (SType ty)

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


----------------------------------------------------------------
