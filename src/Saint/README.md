DONE: Type t a :=  AnnTypeRep t a
DONE: SomeType t :=  SType tr
DONE: IsType := IsTypeRep

Some type variables were renamed from "ty" to "tr" to match "type
representation".

-- Codes for types, based on another type of codes for base types
--   implicitly forall-quantified
data SType tr where
  SBase :: tr a        -> SType tr
  SVar  :: Int         -> SType tr
  SFun  :: SType tr -> SType tr -> SType tr

data AnnTypeRep t a where
  Base   :: t (AnnTypeRep t) a -> AnnTypeRep t a
  Tag    :: String -> AnnTypeRep t a -> AnnTypeRep t a
  (:->)  :: AnnTypeRep t a -> AnnTypeRep t b -> AnnTypeRep t (a -> b)


data Expr tr where
  Var  :: String  -> Expr tr
  ILit :: Int     -> Expr tr
  App  :: Expr tr -> Expr tr -> Expr tr
  Lam  :: String  -> tr a    -> Expr tr -> tr b -> Expr tr

-- For efficiency, cashes the variables used in the type code
data Scheme tr = Scheme [Int] (SType tr)

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


----------------------------------------------------------------
