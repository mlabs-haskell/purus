{-# OPTIONS_GHC -Wno-orphans #-}

module Language.PureScript.CoreFn.Utils where

import Control.Lens (Indexable (indexed), (^?))
import Control.Lens.IndexedPlated (IndexedPlated (..), itransform)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Natural (Natural)
import Language.PureScript.CoreFn.Desugar.Utils (showIdent', traverseLit)
import Language.PureScript.CoreFn.Expr (
  Bind (..),
  CaseAlternative (CaseAlternative),
  Expr (..),
  Guard,
  PurusType,
  _Var,
 )
import Language.PureScript.Names (Ident, Qualified (..), QualifiedBy (..))
import Language.PureScript.Types
import Prelude hiding (error)
import Prelude qualified

import Control.Lens.Plated
import Control.Lens.Type (Lens')
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.Environment (function, pattern (:->))
import Language.PureScript.Pretty.Types (prettyPrintType)

foldl1x :: (Foldable t) => String -> (a -> a -> a) -> t a -> a
foldl1x msg f xs
  | null xs = Prelude.error msg
  | otherwise = foldl1 f xs

type Context = Map Ident SourceType

{-
prettyContext :: Context -> String
prettyContext cxt = concatMap go (M.toList cxt)
  where
    go :: (Ident,SourceType) -> String
    go (ident,ty) = showIdent' ident <> " := " <> prettyTypeStr ty <> "\n"
-}
instance (Show a) => IndexedPlated Context (Expr a) where
  iplate d f = \case
    Literal ann ty lit -> Literal ann ty <$> traverseLit (indexed f d) lit
    Accessor ann ty field e -> Accessor ann ty field <$> indexed f d e
    ObjectUpdate ann ty orig copyFields updateFields ->
      (\orig' updateFields' -> ObjectUpdate ann ty orig' copyFields updateFields')
        <$> indexed f d orig
        <*> traverse (sequenceA . second (indexed f d)) updateFields
    Abs ann ty ident body -> Abs ann ty ident <$> indexed f (M.insert ident (arg ty) d) body
    App ann fE argE -> App ann <$> indexed f d fE <*> indexed f d argE
    Case a ty scrutinees alternatives ->
      Case a ty <$> traverse (indexed f d) scrutinees <*> traverseAltE d f alternatives
    Let a binds e ->
      Let a <$> traverseBinds d f binds <*> indexed f d e
    other -> pure other -- ctors and vars don't contain any sub-expressions
    where
      arg = \case
        ForAll _ _ _ _ inner _ -> arg inner
        a :-> _ -> a
        other -> other

      traverseBinds :: forall p f. (Indexable Context p, Applicative f) => Context -> p (Expr a) (f (Expr a)) -> [Bind a] -> f [Bind a]
      traverseBinds cxt g binds = traverse (go cxt) binds
        where
          go :: Context -> Bind a -> f (Bind a)
          go gCxt = \case
            NonRec ann ident e ->
              let cxt' = M.insert ident (exprType e) gCxt
               in NonRec ann ident <$> indexed g cxt' e
            Rec es -> Rec <$> goRecursive gCxt es
          goRecursive :: Context -> [((a, Ident), Expr a)] -> f [((a, Ident), Expr a)]
          goRecursive _ [] = pure []
          goRecursive gCxt (((ann, nm), e) : rest) =
            let gCxt' = M.insert nm (exprType e) gCxt
             in (\x xs -> ((ann, nm), x) : xs) <$> indexed g gCxt' e <*> goRecursive gCxt' rest
      traverseAltE :: forall p f. (Indexable Context p, Applicative f) => Context -> p (Expr a) (f (Expr a)) -> [CaseAlternative a] -> f [CaseAlternative a]
      traverseAltE cxt g alts = traverse (go cxt) alts
        where
          go :: Context -> CaseAlternative a -> f (CaseAlternative a)
          go gCxt (CaseAlternative binders result) =
            CaseAlternative binders
              <$> helper gCxt result -- hellishly complex
          helper :: Context -> Either [(Guard a, Expr a)] (Expr a) -> f (Either [(Guard a, Expr a)] (Expr a))
          helper gCxt = \case
            Right e -> Right <$> indexed g gCxt e
            Left es ->
              let g' = indexed g gCxt
               in Left <$> traverse (bitraverse g' g') es

-- Bound tyVars and kinds. Idk if we'll use it but it takes like 10 seconds
type TyContext = Map Text SourceType

-- Might be able to do something useful with a non-natural index (think about later)
instance IndexedPlated TyContext SourceType where
  iplate d f = \case
    TypeApp ann t1 t2 -> TypeApp ann <$> indexed f d t1 <*> indexed f d t2
    KindApp ann k1 t1 -> KindApp ann <$> indexed f d k1 <*> indexed f d t1
    ForAll a vis var mbK inner skol ->
      let cxt = M.insert var mbK d
       in (\mbK' inner' -> ForAll a vis var mbK' inner' skol)
            <$> indexed f d mbK
            <*> indexed f cxt inner
    -- \/ Just for completeness, they shouldn't exist
    ConstrainedType ann c t1 -> ConstrainedType ann c <$> indexed f d t1
    RCons ann lbl t1 t2 -> RCons ann lbl <$> indexed f d t1 <*> indexed f d t2
    KindedType a t1 t2 -> KindedType a <$> indexed f d t1 <*> indexed f d t2
    BinaryNoParensType ann t1 t2 t3 ->
      BinaryNoParensType ann
        <$> indexed f d t1
        <*> indexed f d t2
        <*> indexed f d t3
    ParensInType ann t -> ParensInType ann <$> indexed f d t
    -- nothing else has child types
    other -> pure other

{- TODO: REMOVE EVERYTHING BELOW (it should all be subsumed by the TypeLike class)

-}

-- TODO: Explain what this is / how it works
-- TODO: Type Constructors
instantiates ::
  Text -> -- Name of the TyVar we're checking
  SourceType -> -- Monomorphic type (or "more monomorphic" type)
  SourceType -> -- Polymorphic type (or "more polymoprhic" type)
  Maybe SourceType
instantiates var x (TypeVar _ y _) | y == var = Just x
instantiates var (TypeApp _ t1 t2) (TypeApp _ t1' t2') = case instantiates var t1 t1' of
  Just x -> Just x
  Nothing -> instantiates var t2 t2'
instantiates _ _ _ = Nothing

appFunArgs :: Expr a -> Expr a -> (Expr a, [Expr a])
appFunArgs f args = (appFun f, appArgs f args)
  where
    appArgs :: Expr a -> Expr a -> [Expr a]
    appArgs (App _ t1 t2) t3 = appArgs t1 t2 <> [t3]
    appArgs _ t3 = [t3]

    appFun :: Expr a -> Expr a
    appFun (App _ t1 _) = appFun t1
    appFun res = res

updateVarTy :: Ident -> PurusType -> Expr Ann -> Expr Ann
updateVarTy ident ty = itransform goVar (M.empty :: Context)
  where
    goVar :: forall x. x -> Expr Ann -> Expr Ann
    goVar _ expr = case expr ^? _Var of
      Just (ann, _, Qualified q@(BySourcePos _) varId) | varId == ident -> Var ann ty (Qualified q ident)
      _ -> expr

appType :: (Show a) => Expr a -> Expr a -> SourceType
appType fe ae = case stripQuantifiers funTy of
  ([], ft) ->
    let numArgs = length argTypes
     in foldl1x "appType first branch (CoreFn.Utils)" function . drop numArgs . splitFunTyParts $ ft
  (xs, ft) ->
    let funArgs = splitFunTyParts ft -- funArgTypes ft
        dict = mkInstanceMap M.empty xs argTypes funArgs
        numArgs = length argTypes
        msg =
          "FUNTY: "
            <> prettyPrintType 100 funTy
            <> "\nARGTY: "
            <> prettyPrintType 100 (exprType ae)
            <> "\nNUM ARGS:"
            <> show numArgs
     in quantify
          . foldl1x msg function
          . drop numArgs
          . splitFunTyParts
          . replaceAllTypeVars (M.toList dict)
          $ ft
  where
    (f, args) = appFunArgs fe ae
    funTy = exprType f
    argTypes = exprType <$> args

    mkInstanceMap :: Map Text SourceType -> [Text] -> [SourceType] -> [SourceType] -> Map Text SourceType
    mkInstanceMap acc [] _ _ = acc
    mkInstanceMap acc _ [] _ = acc
    mkInstanceMap acc _ _ [] = acc
    mkInstanceMap acc (var : vars) (mt : mts) (pt : pts) = case instantiates var mt pt of
      Nothing ->
        mkInstanceMap acc [var] mts pts
          <> mkInstanceMap M.empty vars (mt : mts) (pt : pts)
      Just t -> mkInstanceMap (M.insert var t acc) vars (mt : mts) (pt : pts)

stripQuantifiers :: SourceType -> ([Text], SourceType)
stripQuantifiers = first reverse . go []
  where
    go :: [Text] -> SourceType -> ([Text], SourceType)
    go acc (ForAll _ _ var _ inner _) = go (var : acc) inner
    go acc other = (acc, other)

-- | (a -> b -> c) -> [a,b,c]
splitFunTyParts :: Type a -> [Type a]
splitFunTyParts = \case
  (a :-> b) -> a : splitFunTyParts b
  t -> [t]

{- | (a -> b -> c) -> [a,b]

  NOTE: Unsafe/partial
-}
funArgTypes :: Type a -> [Type a]
funArgTypes = init . splitFunTyParts

exprType :: (Show a) => Expr a -> PurusType
exprType = \case
  Literal _ ty _ -> ty
  Accessor _ ty _ _ -> ty
  ObjectUpdate _ ty _ _ _ -> ty
  Abs _ ty _ _ -> ty
  App _ t1 t2 -> appType t1 t2
  Var _ ty __ -> ty
  Case _ ty _ _ -> ty
  Let _ _ e -> exprType e

instance Plated (Expr a) where
  plate f = \case
    Literal a t lit -> Literal a t <$> traverseLit f lit
    Accessor a t s e -> Accessor a t s <$> f e
    ObjectUpdate a t e cf fs ->
      (\e' fs' -> ObjectUpdate a t e' cf fs')
        <$> f e
        <*> traverse (traverse f) fs
    Abs a t bv e -> Abs a t bv <$> f e
    App a e1 e2 -> App a <$> f e1 <*> f e2
    Var a t qi -> pure $ Var a t qi
    Case a t scruts alts ->
      Case a t
        <$> traverse f scruts
        <*> traverse goAlt alts
    Let a decls body ->
      Let a <$> traverse goDecl decls <*> f body
    where
      goAlt (CaseAlternative caBinders caResult) =
        CaseAlternative caBinders
          <$> bitraverse (traverse (traverse f)) f caResult

      goDecl = \case
        NonRec a nm body -> NonRec a nm <$> f body
        Rec xs -> Rec <$> traverse (traverse f) xs
