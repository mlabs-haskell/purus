module Language.Purus.Pipeline.EliminateCases (eliminateCases) where

import Prelude

import Data.Map qualified as M

import Data.Text qualified as T

import Data.Bifunctor (second)
import Data.Foldable (foldl', foldrM)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Traversable (for)

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Constants.Purus qualified as C
import Language.PureScript.CoreFn.Module (
  CtorDecl (..),
  Datatypes,
  cdCtorFields,
  cdCtorName,
  dDataArgs,
  dDataCtors,
  getAllConstructorDecls,
  getConstructorIndexAndDecl,
  lookupCtorType,
  lookupDataDecl,
  tyDict,
 )
import Language.PureScript.CoreFn.TypeLike (
  TypeLike (
    applyType,
    funTy,
    instTy,
    quantify,
    replaceAllTypeVars,
    splitFunTyParts
  ),
  getAllInstantiations,
  getInstantiations,
  safeFunArgTypes,
 )
import Language.PureScript.Names (
  Ident (..),
  ProperName (..),
  ProperNameType (..),
  Qualified (..),
  runIdent,
  showQualified,
 )
import Language.PureScript.Types (
  TypeVarVisibility (TypeVarVisible),
 )

import Language.Purus.Debug (doTrace, doTraceM, prettify)
import Language.Purus.IR (
  Alt (..),
  BVar (BVar),
  BindE (..),
  Exp (..),
  FVar (FVar),
  Lit (..),
  Pat (..),
  Ty (..),
  expTy,
  expTy',
  getPat,
  unsafeAnalyzeApp,
  pattern (:~>),
 )
import Language.Purus.IR qualified as IR
import Language.Purus.IR.Utils (
  Vars,
  WithoutObjects,
  fromExp,
  isConstructor,
  toExp,
 )
import Language.Purus.Pipeline.DesugarCore (
  matchVarLamAbs,
 )
import Language.Purus.Pipeline.GenerateDatatypes.Utils (
  analyzeTyApp,
  foldr1Err,
  freshName,
  funResultTy,
  getDestructorTy,
  prettyQI,
  prettyQPN,
 )
import Language.Purus.Pipeline.Monad (
  MonadCounter (next),
  PlutusContext,
 )
import Language.Purus.Pretty.Common (prettyStr)

import Bound (Var (..))
import Bound.Scope (
  Scope,
  abstract,
  instantiate,
  mapBound,
  toScope,
 )
import Control.Lens (
  at,
  view,
  (^.),
 )
import Control.Lens.Combinators (transform)

import Control.Monad.Except (
  MonadError (throwError),
 )

import PlutusCore.Name.Unique (Unique (Unique))
import PlutusIR (
  Name (Name),
 )

{- @Koz: This module contains the transformations needed to eliminate case expressions.

         Because we are using PIR datatypes (and not, e.g., hand-rolled SOPs), we have to use their
         destructor function machinery to eliminate cases. The bulk of this module consists in functions that
         create (and correctly type) those destructors.

         The order of transformations here is:
           1. Eliminate literal patterns (by transforming them into constructor patterns that match on Boolean)
           2. Eliminate bare irrefutable patterns, since those don't actually require case analysis
           3. Eliminate constructor patterns

         We also run a final Constructor instantiation pass to ensure that all constructors have the
         correct type instantiations and type annotations, and handle nullary constructors separately.

         NOTE on Nullary constructors: The PS type checker annotates constructors with their *Monomorphic* types
                                       (or: with types as monomorphic as they can be). To instantiate nullary
                                       constructors, we just look at the type annotation, figure out the
                                       necessary instantiations by unifying the annotated type with the
                                       type in the declaration, and instantiate.
-}

eliminateCases ::
  Datatypes IR.Kind Ty ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  PlutusContext (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
eliminateCases datatypes _exp = do
  res <- eliminateCaseExpressions datatypes . desugarIrrefutables . desugarLiteralPatterns $ _exp
  doTraceM "eliminateCaseExpressions" ("INPUT:\n" <> prettyStr _exp <> "\n\nOUTPUT:\n" <> prettyStr res)
  pure
    . instantiateNullaryWithAnnotatedType datatypes
    . instantiateCtors datatypes
    $ res

{- NOTE(@Koz): This function runs "desugarConstructorPatterns", but is called "eliminateCaseExpressions"
               because, if we run this *after* desugarIrrefutables and desugarLiteralPatterns,
               we ought to *only* have Constructor Patterns remaining in the AST. If that is the case,
               this will completely eliminate case expressions from the input.

               Honestly, I'm not exactly sure why I had to write it like this. But it *works* written like this!
-}
eliminateCaseExpressions ::
  Datatypes IR.Kind Ty ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  PlutusContext (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
eliminateCaseExpressions datatypes = \case
  V x -> pure $ V x
  LitE t lit -> pure $ LitE t lit
  LamE bv scoped -> do
    let unscoped = toExp scoped
    rescoped <- eliminateCaseExpressions datatypes unscoped
    pure
      . LamE bv
      . toScope
      . fmap F
      $ rescoped
  AppE e1 e2 -> do
    e1' <- eliminateCaseExpressions datatypes e1
    e2' <- eliminateCaseExpressions datatypes e2
    pure $ AppE e1' e2'
  ce@CaseE {} ->
    case ezMonomorphize $ monomorphizePatterns datatypes ce of
      CaseE resTy _scrut _alts -> do
        let retTy = case head _alts of UnguardedAlt _ e -> expTy' id e
            msg =
              prettify
                [ "ANN RES TY:\n " <> prettyStr resTy
                , "SCRUTINEE:\n" <> prettyStr _scrut
                , "ALTS:\n" <> prettyStr _alts
                ]
        doTraceM "eliminateCaseExpressions" msg
        scrut <- eliminateCaseExpressions datatypes _scrut
        alts <- traverse eliminateCasesInAlt _alts
        desugarConstructorPattern datatypes retTy (CaseE resTy scrut alts)
      other -> throwError ("eliminateCaseExpressions: IMPOSSIBLE:\n" <> prettyStr other)
  LetE _bindEs _scoped -> do
    let unscoped = toExp _scoped
    scoped <- toScope . fmap F <$> eliminateCaseExpressions datatypes unscoped
    bindEs <- traverse eliminateCasesInBind _bindEs
    pure $ LetE bindEs scoped
  TyInstE ty inner -> TyInstE ty <$> eliminateCaseExpressions datatypes inner
  TyAbs bv inner -> TyAbs bv <$> eliminateCaseExpressions datatypes inner
  where
    eliminateCasesInAlt ::
      Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)) ->
      PlutusContext (Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)))
    eliminateCasesInAlt (UnguardedAlt pat inner) = do
      let unscoped = toExp inner
      inner' <- toScope . fmap F <$> eliminateCaseExpressions datatypes unscoped
      pure $ UnguardedAlt pat inner'

    eliminateCasesInBind = \case
      NonRecursive i bvix e ->
        let e' = toExp e
         in NonRecursive i bvix . toScope . fmap F
              <$> eliminateCaseExpressions datatypes e'
      Recursive xs ->
        let deScope e = toExp e
            rescope = fmap F . toScope
            xs' = traverse (traverse (eliminateCaseExpressions datatypes) . second deScope) xs
         in Recursive . map (second rescope) <$> xs'

{- Creates (and correctly types) the destructor function

-}
mkDestructorFunTy ::
  Datatypes IR.Kind Ty ->
  Qualified (ProperName 'TypeName) ->
  PlutusContext (Bool, Ty) -- (Is it a nullary TyCon,Destructor fun ty )
mkDestructorFunTy datatypes tn = do
  case datatypes ^. tyDict . at tn of
    Nothing -> throwError $ "mkDestructorFunTy: No type info for " <> prettyQPN tn
    Just dDecl -> do
      let tyArgs = dDecl ^. dDataArgs
          tyAppliedToArgs = foldl' (\acc (t, k) -> TyApp acc (TyVar t k)) (TyCon tn) tyArgs
      let funTyLHS' out = foldr (\(txt, k) acc -> Forall TypeVarVisible txt k acc Nothing) out (dDecl ^. dDataArgs)
      n <- T.pack . show <$> next
      let outVarNm = "out" <> n
          outVar = TyVar outVarNm IR.KindType
      let funTyLHS inner = funTyLHS' $ Forall TypeVarVisible ("out" <> n) IR.KindType inner Nothing
          ctorfs = map snd . view cdCtorFields <$> dDecl ^. dDataCtors
      let funTyRHS = tyAppliedToArgs :~> mkFunTyRHS outVar ctorfs -- foldr funTy outVar funTyCtorArgs
      let result = funTyLHS funTyRHS
      doTraceM
        "mkDestructorFunTy"
        $ prettify
          [ "TYPE NAME:\n" <> prettyStr tn
          , "TY CTOR FIELDS:\n" <> prettyStr ctorfs
          , "TY RHS:\n" <> prettyStr funTyRHS
          , "RESULT:\n" <> prettyStr result
          ]
      pure (null tyArgs, result)
  where
    mkFunTyRHS outVar [] = outVar
    mkFunTyRHS outVar ([] : fss) = outVar :~> mkFunTyRHS outVar fss
    mkFunTyRHS outVar (fs : fss) =
      let fs' = foldr1Err "mkFunTyRHS" (:~>) fs
       in (fs' :~> outVar) :~> mkFunTyRHS outVar fss

desugarConstructorPattern ::
  Datatypes IR.Kind Ty ->
  Ty ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  PlutusContext (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
desugarConstructorPattern datatypes altBodyTy _e =
  let _eTy = expTy id _e
   in case _e of
        CaseE _resTy scrut alts@(UnguardedAlt (ConP tn _ _) _ : _) -> do
          let isConP alt = case getPat alt of ConP {} -> True; _ -> False
              conPatAlts = winnowUnreachable $ takeWhile isConP alts
              scrutTy = expTy id scrut
          indexedBranches <- sortOn fst <$> traverse (mkIndexedBranch scrutTy) conPatAlts
          let branchTy = expTy id . snd . head $ indexedBranches
              branchSplit = splitFunTyParts branchTy
              branchRetTy = last . splitFunTyParts $ branchTy
              allCtors = zip [0 ..] $ getAllConstructorDecls tn datatypes
          (Name dcTor (Unique dctorIx)) <- getDestructorTy tn
          (isNullaryTyCon, dctorTy) <- mkDestructorFunTy datatypes tn
          let destructorRaw = V . B $ BVar dctorIx dctorTy (Ident dcTor)
              instantiateTyCon :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
              instantiateTyCon
                | isNullaryTyCon = id
                | otherwise = mkInstantiateTyCon (expTy id scrut)

              retTy' = mkInstantiateResTy scrutTy altBodyTy
          -- NOTE: We'll need more sophisticated "pattern sorting" with as patterns
          case dropWhile isConP alts of
            [] -> do
              -- In this branch we know we have exhaustive constructor patterns in the alts
              let destructor = TyInstE altBodyTy (AppE (instantiateTyCon destructorRaw) scrut)
                  result = foldl' AppE destructor (snd <$> indexedBranches)
                  msg =
                    prettify
                      [ "INPUT TY:\n" <> prettyStr _eTy
                      , "RESULT TY:\n" <> prettyStr (expTy id result)
                      , "DESTRUCTOR TY:\n" <> prettyStr (expTy id destructor)
                      , "ORIGINAL CASE RES TY:\n" <> prettyStr _resTy
                      , "DEDUCED BRANCH RES TY:\n" <> prettyStr branchRetTy
                      , "SPLIT BRANCH TY:\n" <> prettyStr branchSplit
                      , "FULL BRANCH TY:\n" <> prettyStr branchTy
                      , "SCRUT TY:\n" <> prettyStr scrutTy
                      , "SCRUT EXPR:\n" <> prettyStr scrut
                      , "RESULT:\n" <> prettyStr result
                      , "ALT BODY TY:\n" <> prettyStr altBodyTy
                      , "INSTANTIATED ALT BODY TY:\n" <> prettyStr retTy'
                      ]
              doTraceM "desugarConstructorPattern" msg
              pure result
            irrefutables -> do
              let destructor = TyInstE altBodyTy (AppE (instantiateTyCon destructorRaw) scrut)
                  {- This is confusing and I keep making mistakes, so what's going on with 'irrefutables' is:

                     This only concerns the "catchall" case that we expect when we encounter an
                     incomplete enumeration of constructor patterns in a set of case alternatives.

                     If we have a WildP, we only care about the RHS b/c no variables are bound.

                     If we have a VarP, it binds a variable. But we have to be careful here. If we have (pardon the stupid example)
                        ```
                           case (mb :: Maybe Int) of
                             Nothing -> 0
                             other  -> fromJust other
                        ```

                     `other` is a VarBinder for a value of type `Maybe Int`.

                     The `match` functions we're forced to use don't expect function arguments where the lambda binds
                     a variable of the scrutinee type. E.g. (assuming Nothing is the first ctor)

                     ```
                       match_Maybe :: forall t out. Maybe t -> out -> (t -> out) -> out
                     ```

                     So the translation for the above example is going to look like (pay attention to the types!)

                     match_Maybe @Int mb 0 (\(_n :: Int) -> (\(other :: Maybe Int) -> fromJust other) mb)

                     That's unnecessarily verbose and we can avoid creating a new lambda entirely by substituting the
                     scrutinee into `other` to perform a reduction step, a la:

                     match_Maybe @Int mb 0 (\(_n :: Int) -> fromJust mb)

                     If we had
                     ```
                        case (mb :: Maybe Int) of
                          Nothing -> 0
                          _       -> 1
                     ```

                    The translation would be:

                     ```
                       match_Maybe @Int mb 0 (\(_n :: Int) -> 1)
                     ```

                    Anyway, the idea is that `irrefutable` here is always going to be a self-contained RHS for a lambda that we will attach unused binders to
                    during assembly so as to make the types line up w/ what the destructor fn expects

                    TODO: We should let- bind the scrutinee because it will almost always occur in multiple places
                  -}
                  irrefutable = case head irrefutables of
                    UnguardedAlt WildP irrRHS -> toExp irrRHS
                    UnguardedAlt (VarP bvId bvIx _) irrRHS -> LetE [NonRecursive bvId bvIx (fromExp scrut)] irrRHS
                    other -> error $ "Expected an irrefutable alt but got: " <> prettyStr other
              result <- assemblePartialCtorCase (CtorCase irrefutable (M.fromList indexedBranches) destructor scrutTy) allCtors
              let msg =
                    prettify
                      [ "INPUT TY:\n" <> prettyStr _eTy
                      , "INPUT:\n" <> prettyStr _e
                      , "RESULT TY:\n" <> prettyStr (expTy id result)
                      , "DESTRUCTOR TY:\n" <> prettyStr (expTy id destructor)
                      , "ORIGINAL CASE RES TY:\n" <> prettyStr _resTy
                      , "DEDUCED BRANCH RES TY:\n" <> prettyStr branchRetTy
                      , "SPLIT BRANCH TY:\n" <> prettyStr branchSplit
                      , "FULL BRANCH TY:\n" <> prettyStr branchTy
                      , "SCRUT TY:\n" <> prettyStr scrutTy
                      , "SCRUT EXPR:\n" <> prettyStr scrut
                      , "RESULT:\n" <> prettyStr result
                      , "ALT BODY TY:\n" <> prettyStr altBodyTy
                      , "INSTANTIATED ALT BODY TY:\n" <> prettyStr retTy'
                      ]
              doTraceM "desugarConstructorPattern" msg
              pure result
        other -> pure other
  where
    -- FIXME: This might not be the only place we need to do this. We may need to winnow unreachable literals too, or
    --        unreachable-constructors-with-literal-args
    -- these will all be constructors, this is only ever called after `takeWhile isConP`
    -- REVIEW @Koz I'm not sure about this. Please look it over closely for me.
    winnowUnreachable :: [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))]
                      -> [Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))]
    winnowUnreachable [] = []
    winnowUnreachable (alt:alts) = case getPat alt of
      ConP tn cn ps | null ps || all irrefutable ps ->
        let cleaned = flip filter alts $ \case
                        (getPat -> ConP tn' cn' _) | tn' == tn && cn' == cn -> False
                        _ -> True
            winnowedRest = winnowUnreachable cleaned
        in alt:winnowedRest
      ConP tn cn ps ->
        let cleaned = flip filter alts $ \case
                         (getPat -> ConP tn' cn' ps') | tn' == tn && cn' == cn && and (zipWith covers ps ps') -> False
                         _ -> True
            winnowedRest = winnowUnreachable cleaned
        in alt:winnowedRest
      _ -> alt : winnowUnreachable alts
     where
       irrefutable = \case
         VarP{} -> True
         WildP  -> True
         _ -> False

       {- Whether the second argument is covered by the first.
           A literal covers any literal equal to it.
           A variable or wildcard covers everything.
           A constructor pattern covers constructors of the same type and index iff all of its arguments cover all of the
           second one's arguments. ** Not 100% on this one
       -}
       covers :: Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)
              -> Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Vars Ty)
              -> Bool
       covers VarP{} _ = True
       covers WildP _  = True
       covers (LitP l1) (LitP l2) = l1 == l2
       covers (ConP tn1 cn1 ps1) (ConP tn2 cn2 ps2)
         = tn1 == tn2 && cn1 == cn2 && and (zipWith covers ps1 ps2)
       covers _ _ = False 

    
    assemblePartialCtorCase :: CtorCase -> [(Int, CtorDecl Ty)] -> PlutusContext (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
    assemblePartialCtorCase CtorCase {..} [] = pure acc
    assemblePartialCtorCase CtorCase {..} ((ctorIx, ctorDecl) : rest) = case M.lookup ctorIx indexedMatchArgs of
      Nothing -> do
        let cn = ctorDecl ^. cdCtorName
            tn = fromJust $ lookupCtorType cn datatypes
            cnProper = ProperName . runIdent <$> cn
            monoFieldTypes = snd $ monoCtorFields tn cnProper scrutType datatypes

        lhsVars <- for monoFieldTypes $ \fldTy -> do
          Name nm (Unique u) <- freshName
          pure $ BVar u fldTy (Ident nm)

        let irrefutableLam = mkLHSBinder lhsVars irrefutableRHS
            acc' = AppE acc irrefutableLam
        assemblePartialCtorCase (CtorCase irrefutableRHS indexedMatchArgs acc' scrutType) rest
      Just iFun -> do
        let acc' = AppE acc iFun
        assemblePartialCtorCase (CtorCase irrefutableRHS indexedMatchArgs acc' scrutType) rest
      where
        defAbstr = abstract $ \case
          B bv -> Just bv
          _ -> Nothing
        mkLHSBinder ::
          [BVar Ty] ->
          Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
          Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
        mkLHSBinder [] = id
        mkLHSBinder (bv : bvs) = \inner ->
          let mkRHS = mkLHSBinder bvs
              rhs = mkRHS inner
           in LamE bv (defAbstr rhs)

    mkInstantiateTyCon ::
      Ty ->
      Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
      Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
    mkInstantiateTyCon t e = doTrace "instantiateTyCon" msg result
      where
        result = case analyzeTyApp t of
          Just (_, tyArgs) -> foldr TyInstE e (reverse tyArgs)
          Nothing -> e

        resTy = expTy id result

        msg =
          prettify
            [ "INPUT TY:\n" <> prettyStr t
            , "INPUT EXPR:\n" <> prettyStr e
            , "INPUT EXPR TY:\n" <> prettyStr (expTy id e)
            , "OUTPUT TY:\n" <> prettyStr resTy
            , "OUTPUT:\n" <> prettyStr result
            ]
    {- This is a bit weird. If the alt body type is already quantified then we don't want to
       do any instantiations. TODO: Explain why (kind of complicated)
    -}
    mkInstantiateResTy :: Ty -> Ty -> Ty
    mkInstantiateResTy _ altT@(Forall {}) = doTrace "instantiateResTy" ("UNCHANGED:\n" <> prettyStr altT) altT
    mkInstantiateResTy scrutT altT = doTrace "instantiateResTy" msg result
      where
        result = case analyzeTyApp scrutT of
          Just (_, tyArgs) -> foldr instTy (quantify altT) (reverse tyArgs)
          Nothing -> altT
        msg =
          prettify
            [ "INPUT SCRUT TY:\n" <> prettyStr scrutT
            , "INPUT TARG TY:\n" <> prettyStr altT
            , "OUTPUT TY:\n" <> prettyStr result
            ]

    mkIndexedBranch ::
      Ty ->
      Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)) ->
      PlutusContext (Int, Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
    mkIndexedBranch scrutTy alte@(UnguardedAlt (ConP tn cn binders) rhs) = do
      doTraceM "mkIndexedBranch" ("INPUT:\n" <> prettyStr alte)
      let go (x, t) acc = case x of
            VarP bvId bvIx bvTy' -> do
              let lamBV = BVar bvIx bvTy' bvId
              pure $
                LamE lamBV
                  . abstract (\case F fv -> matchVarLamAbs bvId bvIx fv; B _ -> Nothing)
                  . acc
            WildP -> do
              freshIx <- next
              let newName = "_t" <> T.pack (show freshIx)
                  lamBv = BVar freshIx t (Ident newName)
              pure $ LamE lamBv . toScope . fmap F . acc
            other -> error $ "Unexpected pattern in alternative: Expected a VarP but got " <> show other
          monoFields = snd $ monoCtorFields tn cn scrutTy datatypes
      doTraceM "mkIndexedBranch" ("MONO FIELDS:\n" <> prettyStr monoFields)
      lambdaLHS <- foldrM go id (zip binders monoFields)
      let indx = case fst <$> getConstructorIndexAndDecl cn datatypes of
            Left _ -> error $ "No constructor data for ctor " <> show cn
            Right i -> i
          rhsUnscoped = toExp rhs
          result = lambdaLHS rhsUnscoped
      doTraceM "mkIndexedBranch" ("RESULT:\n" <> prettyStr result)
      pure (indx, result)
    mkIndexedBranch _ (UnguardedAlt otherP _) = error $ "mkIndexedBranch: Expected constructor pattern but got " <> prettyStr otherP

instantiateCtors :: Datatypes IR.Kind Ty -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) -> Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
instantiateCtors dt = transform (instantiateCtor dt)

instantiateCtor ::
  Datatypes IR.Kind Ty ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
instantiateCtor datatypes expr = case expr of
  AppE fe ae -> case unsafeAnalyzeApp (AppE fe ae) of
    (fvar@(V (F (FVar t n))), args)
      | isConstructor n ->
          let ctorNm :: Qualified (ProperName 'ConstructorName)
              ctorNm = ProperName . runIdent <$> n

              tyNm = case lookupCtorType n datatypes of
                Nothing ->
                  error $
                    "instantiateCtor: No type information for constructor: "
                      <> prettyQI n
                Just tn -> tn
              monoFields = monoCtorInst tyNm ctorNm (funResultTy t) datatypes
              fe' = foldr TyInstE fvar monoFields
              result = foldl' AppE fe' args
              msg =
                prettify
                  [ "NAME:" <> T.unpack (showQualified runIdent n)
                  , "MONO TYPE:\n" <> prettyStr t
                  , "INPUT:\n" <> prettyStr expr
                  , "RESULT:\n" <> prettyStr result
                  , "MONO FIELDS:\n" <> prettyStr monoFields
                  , "INSTANTIATED FUN:\n" <> prettyStr fe'
                  ]
           in doTrace "instantiateCtor" msg result
    _ -> expr
  _ -> expr

instantiateNullaryWithAnnotatedType ::
  forall x.
  Datatypes IR.Kind Ty ->
  Exp x Ty (Vars Ty) ->
  Exp x Ty (Vars Ty)
instantiateNullaryWithAnnotatedType datatypes _e = doTrace "instantiateNullaryWithAnnotatedType" msg result
  where
    msg =
      prettify
        [ "INPUT:\n" <> prettyStr _e
        , "OUTPUT:\n" <> prettyStr result
        ]
    result = transform go _e
    go ::
      Exp x Ty (Vars Ty) ->
      Exp x Ty (Vars Ty)
    go expr = case expr of
      V (F (FVar ty nm))
        | isConstructor nm ->
            let cnm = ProperName . runIdent <$> nm
                ctorDecl = either error snd $ getConstructorIndexAndDecl cnm datatypes
             in case ctorDecl ^. cdCtorFields of
                  [] -> case analyzeTyApp ty of
                    Just (_, xs@(_ : _)) -> foldr TyInstE expr (reverse xs)
                    _ -> expr
                  _ -> expr
      _ -> expr

monoCtorInst ::
  Qualified (ProperName 'TypeName) ->
  Qualified (ProperName 'ConstructorName) ->
  Ty -> -- the type of the scrutinee
  Datatypes IR.Kind Ty ->
  [Ty] -- Constructor index & list of field types
monoCtorInst tn cn t datatypes = doTrace "monoCtorInst" msg $ snd <$> reverse instantiations
  where
    msg =
      "TYPE NAME:"
        <> T.unpack (showQualified runProperName tn)
        <> "CTOR NAME:\n"
        <> T.unpack (showQualified runProperName cn)
        <> "MONO IN TYPE:\n"
        <> prettyStr t
        <> "CTOR DECL ARGS:\n"
        <> prettyStr ctorArgs
        <> "POLY TY:\n"
        <> prettyStr polyTy
        <> "INSTANTIATIONS:\n"
        <> prettyStr instantiations
    (_, thisCtorDecl) = either error id $ getConstructorIndexAndDecl cn datatypes
    ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
    thisDataDecl = fromJust $ lookupDataDecl tn datatypes
    declArgVars = uncurry IR.TyVar <$> thisDataDecl ^. dDataArgs
    dataTyCon = TyCon tn
    polyTy = foldl' applyType dataTyCon declArgVars

    instantiations = getInstantiations t polyTy

{- Given a scrutinee type and enough information to uniquely identify a constructor (so as to
   retrieve its data declaration), return the index of the constructor and its instantiated arguments.

   This is used, primarily, to ensure that the annotations attached to Var patterns are correctly instantiated to the
   type of the scrutinee they serve as matchers for.
-}
monoCtorFields ::
  Qualified (ProperName 'TypeName) ->
  Qualified (ProperName 'ConstructorName) ->
  Ty -> -- the type of the scrutinee
  Datatypes IR.Kind Ty ->
  (Int, [Ty]) -- Constructor index & list of field types
monoCtorFields tn cn t datatypes = doTrace "monoCtorFields" msg (thisCtorIx, monoCtorArgs)
  where
    msg =
      prettify
        [ "TYPE NAME:" <> T.unpack (showQualified runProperName tn)
        , "CTOR NAME:\n" <> T.unpack (showQualified runProperName cn)
        , "MONO IN TYPE:\n" <> prettyStr t
        , "CTOR DECL ARGS:\n" <> prettyStr ctorArgs
        , "POLY TY:\n" <> prettyStr polyTy
        , "RESULT TYS:\n" <> prettyStr monoCtorArgs
        , "INSTANTIATIONS:\n" <> prettyStr instantiations
        ]
    (thisCtorIx, thisCtorDecl) = either error id $ getConstructorIndexAndDecl cn datatypes
    ctorArgs = snd <$> thisCtorDecl ^. cdCtorFields
    thisDataDecl = fromJust $ lookupDataDecl tn datatypes
    declArgVars = uncurry IR.TyVar <$> thisDataDecl ^. dDataArgs
    dataTyCon = TyCon tn
    polyTy = foldl' applyType dataTyCon declArgVars
    polyCtorTy = foldr funTy polyTy ctorArgs
    instantiations = getInstantiations t polyTy
    -- This might not be exactly correct, it assumes that NO tyvars remain in the input type.
    -- If any remain, they might not line up with the var names in the declaration (probably won't
    -- since I think I generated the decl vars). Might require tweaking "instantiates" & co
    monoCtorTy = replaceAllTypeVars instantiations polyCtorTy
    -- if it's a unary constructor, we want to skip the error from funArgTypes being partial
    monoCtorArgs = safeFunArgTypes monoCtorTy

{-

Variables bound in patterns contain type annotations in those patterns. We generally ignore those annotations
in previous compiler passes, as they do not matter there. Here, we need to ensure that the annotations are correct
so that the constructor pattern desugaring works as intended & the lambdas introduced there have the correct types.

An example might be helpful. If we have

```
case (x :: Maybe Int) of
  Just y -> f y
  Nothing -> (...)
```

The `y` in pattern position there there may have a (hidden/internal) annotation like
`Maybe a`. We need to force that `a` to `Int`, which is simple enough with some unification,
provided that we have access to the data declaration for `Maybe`.
-}
monomorphizePatterns ::
  Datatypes IR.Kind Ty ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
monomorphizePatterns datatypes _e' = case _e' of
  CaseE resTy scrut alts ->
    let scrutTy = expTy id scrut
        alts' = goAlt scrutTy <$> alts -- REVIEW: Why do we do this twice? Was there a reason or is this just a mistake?
        res = CaseE resTy scrut $ goAlt scrutTy <$> alts'
     in doTrace "monomorphizePatterns" ("INPUT:\n" <> prettyStr _e' <> "RESULT:\n" <> prettyStr res) res
  other -> other
  where
    monomorphPat ::
      Ty ->
      Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)) ->
      Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
    monomorphPat t = \case
      VarP idnt indx _ -> VarP idnt indx t
      ConP tn cn ps ->
        let monoFields = snd $ monoCtorFields tn cn t datatypes
            ps' = zipWith monomorphPat monoFields ps
         in ConP tn cn ps'
      other -> other

    rebindPat ::
      Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)) ->
      Scope (BVar Ty) (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)) ->
      Scope (BVar Ty) (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
    rebindPat p e =
      let upd idnt indx t =
            mapBound (\bv@(BVar bvix _ bvidnt) -> if idnt == bvidnt && indx == bvix then BVar bvix t bvidnt else bv)
              . fmap
                ( \case
                    bv@(B (BVar bvix _ bvidnt)) -> if idnt == bvidnt && indx == bvix then B (BVar bvix t bvidnt) else bv
                    other -> other
                )
       in case p of
            VarP vpId vpIx vpTy -> upd vpId vpIx vpTy e
            ConP _ _ ps' -> foldl' (flip rebindPat) e ps'
            _ -> e

    goAlt ::
      Ty ->
      Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty)) ->
      Alt WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
    goAlt scrutTy (UnguardedAlt p e) =
      let p' = monomorphPat scrutTy p
          e' = rebindPat p' e
       in UnguardedAlt p' e'

desugarLiteralPatterns ::
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
desugarLiteralPatterns = transform desugarLiteralPattern

desugarLiteralPattern ::
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
desugarLiteralPattern = \case
  CaseE resTy scrut (UnguardedAlt (LitP patLit) rhs : alts) ->
    let eqTest = mkEqTestFun scrut patLit
        trueP = ConP C.Boolean C.C_True []
        falseP = ConP C.Boolean C.C_False []
        rest = fromExp $ desugarLiteralPattern (CaseE resTy scrut alts)
     in CaseE
          resTy
          eqTest
          [ UnguardedAlt trueP rhs
          , UnguardedAlt falseP rest
          ]
  CaseE _ _ (UnguardedAlt WildP rhs : _) -> toExp rhs -- FIXME: Wrong! Need to do the same
  -- catchall stuff we do in the ctor
  -- case eliminator
  -- NOTE (8/28): I'm not sure if the previous FIXME still matters?
  CaseE _ scrut (UnguardedAlt (VarP bvId bvIx _) rhs : _) -> LetE [NonRecursive bvId bvIx (fromExp scrut)] rhs
  other -> other
  where
    eqInt =
      V . F $
        FVar
          (TyCon C.Int :~> TyCon C.Int :~> TyCon C.Boolean)
          C.I_equalsInteger

    eqChar = eqInt
    eqString =
      V . F $
        FVar
          (TyCon C.Int :~> TyCon C.Int :~> TyCon C.Boolean)
          C.I_equalsString

    mkEqTestFun ::
      Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
      Lit WithoutObjects (Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))) ->
      Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
    mkEqTestFun scrut = \case
      IntL i -> eqInt `AppE` LitE (TyCon C.Int) (IntL i) `AppE` scrut
      CharL c -> eqChar `AppE` LitE (TyCon C.Char) (CharL c) `AppE` scrut
      StringL s -> eqString `AppE` LitE (TyCon C.String) (StringL s) `AppE` scrut

-- This is for case expressions where the first alternative contains an irrefutable pattern (WildP, VarP)
-- (we need this b/c the other two won't catch and eliminate those expressions)
desugarIrrefutables ::
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
desugarIrrefutables = transform $ \case
  CaseE _ _ (UnguardedAlt WildP rhs : _) -> toExp rhs
  CaseE _ scrut (UnguardedAlt (VarP bvId bvIx _) rhs : _) ->
    LetE [NonRecursive bvId bvIx (fromExp scrut)] rhs
  other -> other

data CtorCase = CtorCase
  { irrefutableRHS :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
  , indexedMatchArgs :: M.Map Int (Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)))
  , -- The destructor fun initially, then the application of that fun to its args
    acc :: Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
  , scrutType :: Ty
  }

-- I think we can replace this with Language.Purus.Pipeline.Instantiate.instantiateTypes but I'm terrified to
-- change anything now that the compiler seems to work....
-- FIXME/REVIEW: I don't think we'll have to do this anymore if we re-instantiate after object desugaring. I should test this.
ezMonomorphize ::
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty)) ->
  Exp WithoutObjects Ty (Var (BVar Ty) (FVar Ty))
ezMonomorphize = transform go
  where
    go expr = case expr of
      AppE fe ae -> case unsafeAnalyzeApp (AppE fe ae) of
        (f, args) -> case expTy id f of
          ft@Forall {} -> case getAllInstantiations ft (expTy id <$> args) of
            [] -> expr
            instantiations' ->
              let instantiations = reverse (snd <$> instantiations')
                  f' = foldr TyInstE f instantiations
                  result = foldl' AppE f' args
                  msg =
                    prettify
                      [ "INPUT:\n" <> prettyStr expr
                      , "OUTPUT:\n"
                      , prettyStr result
                      ]
               in doTrace "ezMonomorphize" msg result
          ft ->
            let msg =
                  prettify
                    [ "NO CHANGE (NOT A FORALL):"
                    , "FUN TY:\n" <> prettyStr ft
                    , "ARG TYPES:\n" <> prettyStr (expTy id <$> args)
                    , "ORIGINAL EXPR:\n" <> prettyStr expr
                    ]
             in doTrace "ezMonomorphize" msg expr
      _ -> expr -- doTrace "ezMonomorphize" ("" <> prettyStr expr) expr
