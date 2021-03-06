{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable,
             FlexibleContexts, FlexibleInstances, OverlappingInstances,
             DeriveGeneric #-}
module Parser
where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.State
import Data.Data
import Data.Generics hiding (Generic)
import Data.Hashable
import Data.List (isSuffixOf, intercalate)
import Data.Maybe
import GHC.Generics (Generic)
import Text.Parsec hiding (label, State, string, space, optional)
import Text.Parsec.Pos (newPos)

import Lexer

import Debug.Trace


idtrace x = traceShow x x


match pred = token (show . snd) (convertPos . fst) (testTok . snd)
  where testTok tt = if pred tt then Just tt else Nothing

convertPos (AlexPn _ line col) = newPos "<input>" line col

word' = match $ \t -> case t of WORD _ -> True; _ -> False
int' = match $ \t -> case t of INT _ -> True; _ -> False

word = do WORD s <- word'; return s
int = do INT s <- int'; return s

anyWord = word <|> show <$> int

exactWord s = match $ \t -> case t of WORD s' | s == s' -> True; _ -> False


data Mutbl = MMut | MImm
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Hashable Mutbl

data Ty =
      TVar Name
    | TAdt Name [Lifetime] [Ty]
    | TTuple [Ty]
    | TRef Lifetime Mutbl Ty
    | TPtr Mutbl Ty
    | TStr
    | TVec Ty
    | TFixedVec Int Ty
    | TInt IntSize
    | TUint IntSize
    | TFloat Int
    | TBool
    | TChar
    | TFn
    | TUnit
    | TBottom
    | TAbstract Name [Lifetime] [Ty]
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Hashable Ty

type Lifetime = Name

type Name = String

data IntSize = BitSize Int | PtrSize
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Hashable IntSize

data Visibility =
    Public
  | Private
  deriving (Eq, Show, Data, Typeable, Generic)

type Abi = Name
type LifetimeParam = Name
type TyParam = Name

data AbstractTypeDef = AbstractTypeDef Name [LifetimeParam] [TyParam]
  deriving (Eq, Show, Data, Typeable, Generic)

data AssociatedTypeDef = AssociatedTypeDef [LifetimeParam] [TyParam] ImplClause Ty
  deriving (Eq, Show, Data, Typeable, Generic)

data StructDef = StructDef Name [LifetimeParam] [TyParam] [FieldDef] (Maybe Name)
  deriving (Eq, Show, Data, Typeable, Generic)

data FieldDef = FieldDef Name Ty
  deriving (Eq, Show, Data, Typeable, Generic)

data EnumDef = EnumDef Name [LifetimeParam] [TyParam] [VariantDef] (Maybe Name)
  deriving (Eq, Show, Data, Typeable, Generic)

data VariantDef = VariantDef Name [Ty]
  deriving (Eq, Show, Data, Typeable, Generic)

data Predicate =
      PImpl Name [Ty]
    | PEq Ty Ty
  deriving (Eq, Show, Data, Typeable, Generic)

data FnDef = FnDef Visibility Name [LifetimeParam] [TyParam] [ArgDecl] Ty (Maybe ImplClause) [Predicate] Expr
  deriving (Eq, Show, Data, Typeable, Generic)

data AbstractFnDef = AbstractFnDef Name [LifetimeParam] [TyParam] [ArgDecl] Ty
  deriving (Eq, Show, Data, Typeable, Generic)

data ExternFnDef = ExternFnDef Abi Name [LifetimeParam] [TyParam] [ArgDecl] Ty
  deriving (Eq, Show, Data, Typeable, Generic)

data ArgDecl = ArgDecl Pattern
  deriving (Eq, Show, Data, Typeable, Generic)

data TraitImpl = TraitImpl [LifetimeParam] [TyParam] ImplClause [Predicate]
  deriving (Eq, Show, Data, Typeable, Generic)

data ImplClause = ImplClause Name [Lifetime] [Ty]
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Expr = Expr Ty Expr_
  deriving (Eq, Show, Data, Typeable, Generic)

data Expr_ =
      EVar Name
    | EConst Name
    | ESimpleLiteral String
    | EStructLiteral [Field]
    | EEnumLiteral Name Int [Expr]
    | ETupleLiteral [Expr]
    | EMatch Expr [MatchArm]
    | EBlock [Stmt] Expr
    | EField Expr Name
    | EDeref Expr
    | EAddrOf Expr
    | EIndex Expr Expr
    | ERange (Maybe Expr) (Maybe Expr)
    | ECast Expr
    | EBinOp String Expr Expr
    | EUnOp String Expr
    | ECall Name [Lifetime] [Ty] [Expr]
    | EUnsafe [Stmt] Expr
    | EAssign Expr Expr
    | EAssignOp String Expr Expr
    | EWhile Expr Expr
    | EReturn Expr
    | EVec [Expr]
    | EFor Pattern Expr Expr_
    | EUnsizeLen Int Expr
    | EBreak
    | EContinue
  deriving (Eq, Show, Data, Typeable, Generic)

data Field = Field Name Expr
  deriving (Eq, Show, Data, Typeable, Generic)

data MatchArm = MatchArm Pattern Expr
  deriving (Eq, Show, Data, Typeable, Generic)

data Pattern = Pattern Ty Pattern_
  deriving (Eq, Show, Data, Typeable, Generic)

data Pattern_ =
      PVar Name
    | PConst Name
    | PEnum Name Int [Pattern]
    | PWild
    | PSimpleLiteral String
    | PTuple [Pattern]
    | PRefVar Name
    | PAddrOf Pattern
  deriving (Eq, Show, Data, Typeable, Generic)

data Stmt = SExpr Expr | SLet Pattern (Maybe Expr)
  deriving (Eq, Show, Data, Typeable, Generic)

data ConstDef = ConstDef Name Ty Expr
  deriving (Eq, Show, Data, Typeable, Generic)

data StaticDef = StaticDef Name Ty Expr
  deriving (Eq, Show, Data, Typeable, Generic)

data UseDefault = UseDefault [LifetimeParam] [TyParam] ImplClause
  deriving (Eq, Show, Data, Typeable, Generic)

data Driver = Driver Expr
  deriving (Eq, Show, Data, Typeable, Generic)

data Item =
      IStruct StructDef
    | IEnum EnumDef
    | IConst ConstDef
    | IFn FnDef
    | IAbstractFn AbstractFnDef
    | IExternFn ExternFnDef
    | IAbstractType AbstractTypeDef
    | IAssociatedType AssociatedTypeDef
    | IStatic StaticDef
    | IUseDefault UseDefault
    | ITraitImpl TraitImpl
    | IDriver Driver
    | IMeta String
  deriving (Eq, Show, Data, Typeable, Generic)

tagged = choice . map (\(x, y) -> exactWord x >> y)

counted p = do
    n <- int
    replicateM n p

optional p = do
    n <- int
    case n of
        0 -> return Nothing
        1 -> Just <$> p
        _ -> fail $ "'optional' expected 0 or 1, not " ++ show n

ty = tagged
    [ ("var", TVar <$> name)
    , ("adt", TAdt <$> name <*> counted lifetime <*> counted ty)
    , ("tuple", TTuple <$> counted ty)
    , ("ref", TRef <$> lifetime <*> return MImm <*> ty)
    , ("ref_mut", TRef <$> lifetime <*> return MMut <*> ty)
    , ("ptr", TPtr <$> return MImm <*> ty)
    , ("ptr_mut", TPtr <$> return MMut <*> ty)
    , ("str", return TStr)
    , ("vec", TVec <$> ty)
    , ("fixed_vec", TFixedVec <$> int <*> ty)
    , ("int", TInt <$> intSize)
    , ("uint", TUint <$> intSize)
    , ("float", TFloat <$> int)
    , ("bool", return TBool)
    , ("char", return TChar)
    , ("fn", return TFn)
    , ("unit", return TUnit)
    , ("bottom", return TBottom)
    , ("abstract", TAbstract <$> name <*> counted lifetime <*> counted ty)
    ]

lifetime = name
name = word

intSize = (exactWord "size" >> return PtrSize) <|> (BitSize <$> int)

abi = name
lifetimeParam = name
tyParam = name

abstractTypeDef = exactWord "abstract_type" >>
    AbstractTypeDef <$> name <*> counted lifetimeParam <*> counted tyParam

associatedTypeDef = exactWord "associated_type" >>
    AssociatedTypeDef <$> counted lifetimeParam <*> counted tyParam <*> implClause <*> ty

structDef = exactWord "struct" >>
    StructDef <$> name <*> counted lifetimeParam <*> counted tyParam <*> counted fieldDef <*> optional name
fieldDef = FieldDef <$> name <*> ty

enumDef = exactWord "enum" >>
    EnumDef <$> name <*> counted lifetimeParam <*> counted tyParam <*> counted variantDef <*> optional name
variantDef = VariantDef <$> name <*> counted ty

visibility = tagged
    [ ("priv", return Private)
    , ("pub", return Public)
    ]

predicate = tagged
    [ ("ty_impl", PImpl <$> name <*> (counted lifetime >> counted ty))
    , ("ty_eq", PEq <$> ty <*> ty)
    , ("ty_eq_proj", PEq <$> ty <*> ty)
    ]

fnDef = do
    exactWord "fn"
    f <- FnDef <$> visibility <*> name <*> counted lifetimeParam <*> counted tyParam
    exactWord "args"
    f <- f <$> counted argDecl
    exactWord "return"
    f <- f <$> ty
    implClause <- optional $ implClause
    f <- f <$> return implClause
    exactWord "preds"
    f <- f <$> counted predicate
    exactWord "body"
    f <$> expr

abstractFnDef = do
    exactWord "abstract_fn"
    f <- AbstractFnDef <$> name <*> counted lifetimeParam <*> counted tyParam
    exactWord "args"
    f <- f <$> counted argDecl
    exactWord "return"
    f <$> ty

externFnDef = do
    exactWord "extern_fn"
    f <- ExternFnDef <$> abi <*> name <*> counted lifetimeParam <*> counted tyParam
    exactWord "args"
    f <- f <$> counted argDecl
    exactWord "return"
    f <$> ty

argDecl = ArgDecl <$> pattern

traitImpl = exactWord "impl" >>
    TraitImpl <$> counted lifetimeParam <*> counted tyParam <*> implClause <*> counted predicate

implClause = ImplClause <$> name <*> counted lifetime <*> counted ty

expr = Expr <$> ty <*> expr_
expr_ = tagged
    [ ("var", EVar <$> name)
    , ("const", EConst <$> name)
    , ("simple_literal", ESimpleLiteral <$> anyWord)
    , ("struct_literal", EStructLiteral <$> counted field)
    , ("enum_literal", EEnumLiteral <$> name <*> int <*> counted expr)
    , ("tuple_literal", ETupleLiteral <$> counted expr)
    , ("match", EMatch <$> expr <*> counted matchArm)
    , ("block", EBlock <$> counted stmt <*> expr)
    , ("field", EField <$> expr <*> name)
    , ("deref", EDeref <$> expr)
    , ("addr_of", EAddrOf <$> expr)
    , ("index", EIndex <$> expr <*> expr)
    , ("range", ERange <$> optional expr <*> optional expr)
    , ("cast", ECast <$> expr)
    , ("binop", EBinOp <$> word <*> expr <*> expr)
    , ("unop", EUnOp <$> word <*> expr)
    , ("call", ECall <$> name <*> counted lifetime <*> counted ty <*> counted expr)
    , ("unsafe", EUnsafe <$> counted stmt <*> expr)
    , ("assign", EAssign <$> expr <*> expr)
    , ("return", EReturn <$> expr)
    , ("while", EWhile <$> expr <*> expr)
    , ("assign_op", EAssignOp <$> word <*> expr <*> expr)
    , ("vec", EVec <$> counted expr)
    , ("for", EFor <$> pattern <*> expr <*> expr_)
    , ("unsize_len", EUnsizeLen <$> int <*> expr)
    , ("break", return EBreak)
    , ("continue", return EContinue)
    ]
field = Field <$> name <*> expr

matchArm = MatchArm <$> pattern <*> expr

pattern = Pattern <$> ty <*> pattern_
pattern_ = tagged
    [ ("var", PVar <$> name)
    , ("const", PConst <$> name)
    , ("enum", PEnum <$> name <*> int <*> counted pattern)
    , ("wild", return PWild)
    , ("simple_literal", PSimpleLiteral <$> anyWord)
    , ("tuple", PTuple <$> counted pattern)
    , ("ref_var", PRefVar <$> name)
    , ("addr_of", PAddrOf <$> pattern)
    ]

stmt = tagged
    [ ("expr", SExpr <$> expr)
    , ("let", SLet <$> pattern <*> optional expr)
    ]

constDef = exactWord "const" >>
    ConstDef <$> name <*> ty <*> expr

staticDef = exactWord "static" >>
    StaticDef <$> name <*> ty <*> expr

useDefault = exactWord "use_default" >>
    UseDefault <$> counted lifetimeParam <*> counted tyParam <*> implClause

driver = exactWord "driver" >> Driver <$> expr

item = choice
    [ IStruct <$> structDef
    , IEnum <$> enumDef
    , IConst <$> constDef
    , IFn <$> fnDef
    , IAbstractFn <$> abstractFnDef
    , IExternFn <$> externFnDef
    , IAbstractType <$> abstractTypeDef
    , IAssociatedType <$> associatedTypeDef
    , IUseDefault <$> useDefault
    , ITraitImpl <$> traitImpl
    , IDriver <$> driver
    , IStatic <$> staticDef
    ]

program = many item


class Pp a where
    pp :: a -> String
    pp x = intercalate " " $ pp' x

    pp' :: a -> [String]

instance Pp Int where
    pp' i = [show i]

instance Pp String where
    pp' s = [s]

instance Pp a => Pp [a] where
    pp' xs = show (length xs) : map pp xs

instance Pp a => Pp (Maybe a) where
    pp' Nothing  = ["0"]
    pp' (Just x) = ["1", pp x]

ppGo x xs = x : xs

instance Pp Ty where
    pp' ty = case ty of
        TVar a ->           ppGo "var"      [pp a]
        TAdt a b c ->       ppGo "adt"      [pp a, pp b, pp c]
        TTuple a ->         ppGo "tuple"    [pp a]
        TRef a MImm b ->    ppGo "ref"      [pp a, pp b]
        TRef a MMut b ->    ppGo "ref_mut"  [pp a, pp b]
        TPtr MImm a ->      ppGo "ptr"      [pp a]
        TPtr MMut a ->      ppGo "ptr_mut"  [pp a]
        TStr ->             ppGo "str"      []
        TVec a ->           ppGo "vec"      [pp a]
        TFixedVec a b ->    ppGo "fixed_vec" [pp a, pp b]
        TInt a ->           ppGo "int"      [pp a]
        TUint a ->          ppGo "uint"     [pp a]
        TFloat a ->         ppGo "float"    [pp a]
        TBool ->            ppGo "bool"     []
        TChar ->            ppGo "char"     []
        TFn ->              ppGo "fn"       []
        TUnit ->            ppGo "unit"     []
        TBottom ->          ppGo "bottom"   []
        TAbstract a b c ->  ppGo "abstract" [pp a, pp b, pp c]
    pp x = "[" ++ intercalate " " (pp' x) ++ "]"

instance Pp IntSize where
    pp' (BitSize b) = [show b]
    pp' (PtrSize) = ["size"]

instance Pp Visibility where
  pp' Private = ["priv"]
  pp' Public = ["pub"]

instance Pp AbstractTypeDef where
    pp' (AbstractTypeDef a b c) = ppGo "abstract_type" [pp a, pp b, pp c]

instance Pp AssociatedTypeDef where
    pp' (AssociatedTypeDef a b c d) = ppGo "associated_type" [pp a, pp b, pp c, pp d]

instance Pp StructDef where
    pp' (StructDef a b c d e) = ppGo "struct" [pp a, pp b, pp c, pp d, pp e]

instance Pp FieldDef where
    pp' (FieldDef a b) = map pp [pp a, pp b]

instance Pp EnumDef where
    pp' (EnumDef a b c d e) = ppGo "enum" [pp a, pp b, pp c, pp d, pp e]

instance Pp VariantDef where
    pp' (VariantDef a b) = map pp [pp a, pp b]

instance Pp Predicate where
    pp' (PImpl a b) = ["ty_impl", pp a, "0" {- lifetimes -}, pp b]
    pp' (PEq a b) = ["ty_eq", pp a, pp b]

instance Pp FnDef where
    pp' (FnDef vis name lifetimeParams tyParams args retTy implClause preds body) =
        ["fn", pp vis, pp name, pp lifetimeParams, pp tyParams,
         "args", pp args,
         "return", pp retTy,
         pp implClause,
         "preds", pp preds,
         "body", pp body]

instance Pp AbstractFnDef where
    pp' (AbstractFnDef name lifetimeParams tyParams args retTy) =
        ["abstract_fn", pp name, pp lifetimeParams, pp tyParams,
         "args", pp args,
         "return", pp retTy]

instance Pp ExternFnDef where
    pp' (ExternFnDef abi name lifetimeParams tyParams args retTy) =
        ["extern_fn", pp abi, pp name, pp lifetimeParams, pp tyParams,
         "args", pp args,
         "return", pp retTy]

instance Pp ArgDecl where
    pp' (ArgDecl a) = map pp [pp a]

instance Pp TraitImpl where
    pp' (TraitImpl a b c d) = ppGo "impl" [pp a, pp b, pp c, pp d]

instance Pp ImplClause where
    pp' (ImplClause absName lifetimes tys) = [pp absName, pp lifetimes, pp tys]

instance Pp Expr where
    pp' (Expr a b) = map pp [pp a, pp b]
    pp x = "(" ++ intercalate " " (pp' x) ++ ")"

instance Pp Expr_ where
    pp' e = case e of
        EVar a ->               ppGo "var"              [pp a]
        EConst a ->             ppGo "const"            [pp a]
        ESimpleLiteral a ->     ppGo "simple_literal"   [pp a]
        EStructLiteral a ->     ppGo "struct_literal"   [pp a]
        EEnumLiteral a b c ->   ppGo "enum_literal"     [pp a, pp b, pp c]
        ETupleLiteral a ->      ppGo "tuple_literal"    [pp a]
        EMatch a b ->           ppGo "match"            [pp a, pp b]
        EBlock a b ->           ppGo "block"            [pp a, pp b]
        EField a b ->           ppGo "field"            [pp a, pp b]
        EDeref a ->             ppGo "deref"            [pp a]
        EAddrOf a ->            ppGo "addr_of"          [pp a]
        EIndex a b ->           ppGo "index"            [pp a, pp b]
        ERange a b ->           ppGo "range"            [pp a, pp b]
        ECast a ->              ppGo "cast"             [pp a]
        EBinOp a b c ->         ppGo "binop"            [pp a, pp b, pp c]
        EUnOp a b ->            ppGo "unop"             [pp a, pp b]
        ECall a b c d ->        ppGo "call"             [pp a, pp b, pp c, pp d]
        EUnsafe a b ->          ppGo "unsafe"           [pp a, pp b]
        EAssign a b ->          ppGo "assign"           [pp a, pp b]
        EReturn a ->            ppGo "return"           [pp a]
        EWhile a b ->           ppGo "while"            [pp a, pp b]
        EAssignOp a b c ->      ppGo "assign_op"        [pp a, pp b, pp c]
        EVec e ->               ppGo "vec"              [pp e]
        EFor a b c ->           ppGo "for"              [pp a, pp b, pp c]
        EUnsizeLen a b ->       ppGo "unsize_len"       [pp a, pp b]
        EBreak ->               ppGo "break"            []
        EContinue ->            ppGo "continue"         []

instance Pp Field where
    pp' (Field a b) = map pp [pp a, pp b]

instance Pp MatchArm where
    pp' (MatchArm a b) = map pp [pp a, pp b]

instance Pp Pattern where
    pp' (Pattern a b) = map pp [pp a, pp b]
    pp x = "(" ++ intercalate " " (pp' x) ++ ")"

instance Pp Pattern_ where
    pp' p = case p of
        PVar a ->               ppGo "var"              [pp a]
        PConst a ->             ppGo "const"            [pp a]
        PEnum a b c ->          ppGo "enum"             [pp a, pp b, pp c]
        PWild ->                ppGo "wild"             []
        PSimpleLiteral a ->     ppGo "simple_literal"   [pp a]
        PTuple a ->             ppGo "tuple"            [pp a]
        PRefVar a ->            ppGo "ref_var"          [pp a]
        PAddrOf a ->            ppGo "addr_of"          [pp a]

instance Pp Stmt where
    pp' s = case s of
        SExpr e -> ppGo "expr" [pp e]
        SLet p e -> ppGo "let" [pp p, pp e]

instance Pp ConstDef where
    pp' (ConstDef n t e) = ppGo "const" [pp n, pp t, pp e]

instance Pp StaticDef where
    pp' (StaticDef n t e) = ppGo "static" [pp n, pp t, pp e]

instance Pp UseDefault where
    pp' (UseDefault a b c) = ppGo "use_default" [pp a, pp b, pp c]

instance Pp Driver where
    pp' (Driver a) = ppGo "driver" [pp a]

instance Pp Item where
    pp' i =
        let a = case i of
                    IStruct a -> pp a
                    IEnum a -> pp a
                    IConst a -> pp a
                    IFn a -> pp a
                    IAbstractFn a -> pp a
                    IExternFn a -> pp a
                    IAbstractType a -> pp a
                    IAssociatedType a -> pp a
                    IUseDefault a -> pp a
                    ITraitImpl a -> pp a
                    IDriver a -> pp a
                    IStatic a -> pp a
                    IMeta s -> s
        in [pp a, "\n"]
