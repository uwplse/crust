{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Pprint
where

import Control.Monad.Reader
import Control.Monad.Writer

import Parser


parens a = tell "(" >> a >> tell ")"
brackets a = tell "[" >> a >> tell "]"
braces a = tell "{" >> a >> tell "}"
angles a = tell "<" >> a >> tell ">"

sepBy punct [] = return ()
sepBy punct [a] = a
sepBy punct (a : as) = a >> punct >> sepBy punct as

commaSep = sepBy (tell ", ")
spaceSep = sepBy (tell " ")

nl = tell "\n"

indentation = do
    ind <- ask
    tell (replicate (4 * ind) ' ')

line :: (MonadReader Int m, MonadWriter String m) => m () -> m ()
line a = do
    indentation
    a
    nl

inline a b c = do
    a >> nl
    indent c
    indentation >> b


indent a = local (+1) a


listNe :: MonadWriter String m => (m () -> m ()) -> [m ()] -> m ()
listNe f [] = return ()
listNe f as = f (commaSep as)

ppMutbl MMut = tell "mut"
ppMutbl MImm = return ()

ppVis Private = tell "priv"
ppVis Public = tell "pub"

ppTy ty = case ty of
    TVar name -> tell name
    TAdt name las tas -> tell name >> listNe angles (map ppLifetime las ++ map ppTy tas)
    TTuple tys -> parens (commaSep $ map ppTy tys)
    TRef life mut ty -> spaceSep $ [tell "&", ppLifetime life, ppMutbl mut, ppTy ty]
    TPtr mut ty -> spaceSep $ [tell "*", ppMutbl mut, ppTy ty]
    TStr -> tell "str"
    TVec ty -> brackets $ ppTy ty
    TFixedVec len ty -> brackets $ ppTy ty >> tell "; " >> tell (show len)
    TInt (BitSize b) -> tell $ "i" ++ show b
    TInt PtrSize -> tell "isize"
    TUint (BitSize b) -> tell $ "u" ++ show b
    TUint PtrSize -> tell "usize"
    TFloat size -> tell $ "f" ++ show size
    TBool -> tell "bool"
    TChar -> tell "char"
    TFn -> tell "fn"
    TUnit -> tell "()"
    TBottom -> tell "!"
    TAbstract name las tas -> angles $ tell name >> listNe angles (map ppLifetime las ++ map ppTy tas)

ppLifetime l = tell "'" >> tell l

ppAbstractTypeDef :: (MonadReader Int m, MonadWriter String m) => AbstractTypeDef -> m ()
ppAbstractTypeDef (AbstractTypeDef name lps tps) =
    line $ tell "/* abstract */ type " >> tell name >> listNe angles (map tell lps ++ map tell tps) >> tell ";"

ppAssociatedTypeDef :: (MonadReader Int m, MonadWriter String m) => AssociatedTypeDef -> m ()
ppAssociatedTypeDef (AssociatedTypeDef lps tps impl ty) =
    line $ tell "/* associated */ type" >> listNe angles (map tell lps ++ map tell tps) >>
        tell " " >> ppImplClause impl >> tell " = " >> ppTy ty

ppStructDef :: (MonadReader Int m, MonadWriter String m) => StructDef -> m ()
ppStructDef (StructDef name lps tps fields mDtor) = do
    line $ tell "struct " >> tell name >> listNe angles (map ppLifetime lps ++ map tell tps) >> tell " {"
    indent $ do
        forM fields $ \(FieldDef name ty) ->
            line $ tell name >> tell ": " >> ppTy ty >> tell ","
        case mDtor of 
            Just dtor -> line $ tell "// destructor: " >> tell dtor
            Nothing -> return ()
    line $ tell "}"

ppEnumDef (EnumDef name lps tps variants mDtor) = do
    line $ tell "enum " >> tell name >> listNe angles (map ppLifetime lps ++ map tell tps) >> tell " {"
    indent $ do
        forM variants $ \(VariantDef name tys) ->
            line $ tell name >> listNe parens (map ppTy tys) >> tell ","
        case mDtor of 
            Just dtor -> line $ tell "// destructor: " >> tell dtor
            Nothing -> return ()
    line $ tell "}"

ppPredicate :: (MonadReader Int m, MonadWriter String m) => Predicate -> m ()
ppPredicate (PImpl name tys) = tell "impl " >> tell name >> listNe angles (map ppTy tys)
ppPredicate (PEq ty1 ty2) = ppTy ty1 >> tell " == " >> ppTy ty2

ppFnDef :: (MonadReader Int m, MonadWriter String m) => FnDef -> m ()
ppFnDef (FnDef vis name lps tps args retTy implClause preds body) = do
    line $ do
        tell "fn " >> ppVis vis >> tell " " >> tell name
        listNe angles (map ppLifetime lps ++ map tell tps)
        parens $ commaSep $ map ppArgDecl args
        tell " -> " >> ppTy retTy
        case implClause of
            Nothing -> tell ""
            Just implClause -> do
                nl
                indent $ indent $ indentation
                tell "/* impl " >> ppImplClause implClause >> tell " */"
        case preds of
            [] -> tell ""
            _ -> do
                forM_ preds $ \pred -> do
                    nl
                    indent $ indent $ indentation
                    tell "where " >> ppPredicate pred
        tell " " >> ppExpr body

ppAbstractFnDef :: (MonadReader Int m, MonadWriter String m) => AbstractFnDef -> m ()
ppAbstractFnDef (AbstractFnDef name lps tps args retTy) = do
    line $ do
        tell "/* abstract */ fn " >> tell name
        listNe angles (map ppLifetime lps ++ map tell tps)
        parens $ commaSep $ map ppArgDecl args
        tell " -> " >> ppTy retTy >> tell ";"

ppExternFnDef :: (MonadReader Int m, MonadWriter String m) => ExternFnDef -> m ()
ppExternFnDef (ExternFnDef abi name lps tps args retTy) = do
    line $ do
        tell "extern \"" >> tell abi >> tell "\" fn " >> tell name
        listNe angles (map ppLifetime lps ++ map tell tps)
        parens $ commaSep $ map ppArgDecl args
        tell " -> " >> ppTy retTy >> tell ";"

ppArgDecl :: (MonadReader Int m, MonadWriter String m) => ArgDecl -> m ()
ppArgDecl (ArgDecl pat@(Pattern ty _)) = ppPat pat >> tell ": " >> ppTy ty

ppImplClause :: (MonadReader Int m, MonadWriter String m) => ImplClause -> m ()
ppImplClause (ImplClause name lifetimes tys) = do
    tell name
    angles $ commaSep (map ppLifetime lifetimes ++ map ppTy tys)

ppExpr :: (MonadReader Int m, MonadWriter String m) => Expr -> m ()
ppExpr (Expr ty e) = case e of
    EVar n -> tell n
    EConst n -> tell n
    ESimpleLiteral n -> parens $ spaceSep [tell n, tell ":", ppTy ty]
    EStructLiteral fs -> do
        let (TAdt structName _ _) = ty
        inline (tell structName >> tell " {") (tell "}") $
            forM fs $ \(Field name val) ->
                line $ tell name >> tell ": " >> ppExpr val >> tell ","
    EEnumLiteral name _ vals -> tell name >> listNe parens (map ppExpr vals)
    ETupleLiteral vals -> parens $ commaSep $ map ppExpr vals
    EMatch expr arms ->
        inline (tell "match " >> ppExpr expr >> tell " {") (tell "}") $
            forM arms $ \(MatchArm pat body) ->
                line $ ppPat pat >> tell " => " >> ppExpr body >> tell ","
    EBlock stmts expr ->
        inline (tell "{") (tell "}") $ mapM ppStmt stmts >> line (ppExpr expr)
    EField expr name -> ppExpr expr >> tell "." >> tell name
    EDeref expr -> tell "*" >> ppExpr expr
    EAddrOf expr ->
        tell "&" >> (case ty of TRef _ MMut _ -> tell "mut "; _ -> return ()) >> ppExpr expr
    EIndex arr idx -> ppExpr arr >> brackets (ppExpr idx)
    ERange low high ->
        maybe (return ()) ppExpr low >>
        tell " .. " >>
        maybe (return ()) ppExpr high
    ECast expr -> ppExpr expr >> tell " as " >> ppTy ty
    EBinOp op a b -> parens $ ppExpr a >> tell " `" >> tell op >> tell "` " >> ppExpr b
    EUnOp op a -> parens $ tell "`" >> tell op >> tell "` " >> ppExpr a
    ECall name las tas args -> do
        tell name
        listNe angles (map ppLifetime las ++ map ppTy tas)
        parens $ commaSep $ map ppExpr args
    EUnsafe stmts expr ->
        inline (tell "unsafe {") (tell "}") $ mapM ppStmt stmts >> line (ppExpr expr)
    EAssign lhs rhs -> ppExpr lhs >> tell " = " >> ppExpr rhs
    EAssignOp op lhs rhs -> ppExpr lhs >> tell (" `" ++ op ++ "`= ") >> ppExpr rhs
    EWhile cond body ->
        tell "while " >> ppExpr cond >> tell " " >> ppExpr body
    EReturn expr -> tell "return " >> ppExpr expr
    EVec exprs -> do
      tell "vec["
      commaSep $ map ppExpr exprs
      tell "]"
    EFor patt expr body ->
        inline (tell "for " >> ppPat patt >> tell " in " >> ppExpr expr >> tell " {") (tell "}") $ ppExpr (Expr TUnit body)
    EUnsizeLen len expr ->
        tell "unsize(" >> tell (show len) >> tell ", " >> ppExpr expr >> tell ")"
    EBreak -> tell "break"
    EContinue -> tell "continue"

ppPat (Pattern ty p) = case p of
    PVar name -> tell name
    PConst name -> tell name
    PEnum name _ pats -> tell name >> listNe parens (map ppPat pats)
    PWild -> tell "_"
    PSimpleLiteral str -> tell str >> tell " : " >> ppTy ty
    PTuple pats -> parens $ commaSep $ map ppPat pats
    PRefVar name -> tell "ref " >> tell name
    PAddrOf pat -> tell "& " >> ppPat pat

ppStmt (SExpr e) = line $ ppExpr e >> tell ";"
ppStmt (SLet pat@(Pattern ty _) Nothing) = line $ do
    spaceSep [tell "let", ppPat pat, tell ":", ppTy ty]
    tell ";"
ppStmt (SLet pat@(Pattern ty _) (Just expr)) = line $ do
    spaceSep [tell "let", ppPat pat, tell ":", ppTy ty, tell "=", ppExpr expr]
    tell ";"

ppConstDef (ConstDef name ty expr) = line $ do
    spaceSep [tell "const", tell name, tell ":", ppTy ty, tell "=", ppExpr expr]
    tell ";"

ppStaticDef (StaticDef name ty expr) = line $ do
    spaceSep [tell "static", tell name, tell ":", ppTy ty, tell "=", ppExpr expr]
    tell ";"

ppUseDefault (UseDefault lps tps impl) = line $
    tell "use_default" >>
    listNe angles (map ppLifetime lps ++ map tell tps) >>
    tell " " >>
    ppImplClause impl

ppDriver (Driver expr) = line $
    tell "driver " >> ppExpr expr

ppItem (IStruct s) = ppStructDef s
ppItem (IEnum e) = ppEnumDef e
ppItem (IConst c) = ppConstDef c
ppItem (IFn f) = ppFnDef f
ppItem (IAbstractFn f) = ppAbstractFnDef f
ppItem (IExternFn f) = ppExternFnDef f
ppItem (IAbstractType t) = ppAbstractTypeDef t
ppItem (IAssociatedType t) = ppAssociatedTypeDef t
ppItem (IStatic t) = ppStaticDef t
ppItem (IUseDefault u) = ppUseDefault u
ppItem (IDriver d) = ppDriver d
ppItem (IMeta m) = line $ tell "// metadata: " >> tell m

runPp :: (ReaderT Int (Writer String) ()) -> String
runPp a = execWriter (runReaderT a 0)
