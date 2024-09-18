module Language.Purus.Pipeline.Lift.Types where

import Prelude

import Data.Map qualified as M
import Data.Set (Set)
import Data.Text (Text)

import Data.Set qualified as S

import Language.PureScript.CoreFn.Expr (PurusType)
import Language.PureScript.CoreFn.TypeLike
import Language.PureScript.Names (
  Ident (GenIdent, Ident),
  ModuleName (ModuleName),
  Qualified (Qualified),
  QualifiedBy (ByModuleName),
  runIdent,
 )

import Language.Purus.Debug (prettify)
import Language.Purus.IR
import Language.Purus.IR.Utils
import Language.Purus.Pretty.Common

import Bound (Scope, Var (..))
import Prettyprinter
import Data.Map (Map)

-- sorry Koz i really want to be able to fit type sigs on one line
type MonoExp = Exp WithObjects PurusType (Vars PurusType)
type MonoScoped = Scope (BVar PurusType) (Exp WithObjects PurusType) (Vars PurusType)
type MonoBind = BindE PurusType (Exp WithObjects PurusType) (Vars PurusType)
type MonoAlt = Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)

{- The thing that our Lift function gives us.

   This is essentially equivalent to a LetE expression,
   but it's useful to keep the lifted declarations separate from
   the body expression, since we'll be inlining and monomorphizing
   with those declarations.
-}
data LiftResult = LiftResult
  { liftedDecls :: [MonoBind]
  , trimmedExp :: MonoExp
  }

{- When we lift expressions, we have to abstract any types mentioned in the
   expressions which are well-scoped in the original context we are lifting from,
   but are no longer in scope in the lifted expression.

   This will usually turn a non-quantified expression into a quantified expression.

   This is bad. It can (e.g.) cause case expressions that return functions to have
   branches with *different* types. E.g.

   ```haskell
   -- Assume there's a (y :: a) for some a in scope
   let f :: Int -> a -> a
       f x t = (...)
   in case (z :: Maybe Int) of
         Just w -> f w
         Nothing -> \_ -> y

   -- f gets lifted to:
   f :: forall (t1 :: Type). Int -> t1 -> t1
   f = /\(t1 :: Type) -> (...)
   ```

   In a situation like that, we lack sufficient information to "force" the type to its "original"
   form. In this particular example, it is not even clear how we could possibly know what
   `t1` needs to be instantiated to (well ok *we*, i.e., competent human beings, can know it
   but it's not clear that a general procedure for recovering the information from the context exists)

   The easiest solution here is to pack that information into variables. Either bound or free variables
   would work here, but we can use the ModuleName field in the Qualified Ident inside of an FVar to
   very clearly indicate that a "hole" representing an inline target exists.

   This pattern synonym (it's bidirectional btw) allows us to construct and deconstruct a
   representation of those "holes".

   NOTE: This will break a bunch of stuff, as up to this point we haven't been using FVars for anything
         meaningful. But that should be OK, since all of these FVars will be eliminated during inlining
         (and it's very easy to throw a *useful* error if we made a mistake and missed one)
-}
pattern LiftedHole :: Text -> Integer -> t -> FVar t
pattern LiftedHole nm indx ty =
  FVar
    ty
    ( Qualified
        (ByModuleName (ModuleName "$LIFTED")) -- just has to be an arbitrary illegal module name
        (GenIdent (Just nm) indx)
      )

pattern LiftedHoleTerm :: Text -> Integer -> t -> Exp x t (Vars t)
pattern LiftedHoleTerm nm indx ty = V (F (LiftedHole nm indx ty))

-- concrete representation, avoid manual Text<->Ident & Integer<->Int conversion
data Hole t = Hole Ident Int t

toHole :: Exp x t (Vars t) -> Maybe (Hole t)
toHole (LiftedHoleTerm nm indx ty) = Just $ Hole (Ident nm) (fromIntegral indx) ty
toHole _ = Nothing

fromHole :: Hole t -> Exp x t (Vars t)
fromHole (Hole hId hIx hTy) = LiftedHoleTerm (runIdent hId) (fromIntegral hIx) hTy

-- dunno if we should ignore the type?
fillsHole :: BVar t -> Exp x t (Vars t) -> Bool
fillsHole (BVar bvIx _ bvIdent) = \case
  LiftedHoleTerm (Ident -> i) (fromIntegral -> indx) _ -> bvIx == indx && bvIdent == i
  _ -> False

unHole :: Hole t -> (Ident, Int)
unHole (Hole hId hIx _) = (hId, hIx)

instance Pretty LiftResult where
  pretty (LiftResult decls expr) =
    let mkPrettyDeclWithTySig acc (i, u) scoped =
          let unscoped = toExp scoped
              ty = expTy id unscoped
              prettyBody = pretty (NonRecursive i u scoped)
              prettySig = pretty i <::> pretty ty
              prettyWithSig = align $ vcat [prettySig, prettyBody, hardline]
           in prettyWithSig : acc

        prettyDecls = foldBinds mkPrettyDeclWithTySig [] decls
     in align $
          vcat
            [ "let"
            , indent 2 . align . vcat $ prettyDecls
            , "in" <+> align (pretty expr)
            ]

{- Intermediate data type for recording the scope at the
   place where a group of declarations occurs in the AST.

   Without this scope information, lifting is impossible.
-}
data ToLift = ToLift
  { varScopeAtDecl :: Set (BVar PurusType)
  , tyVarScopeAtDecl :: Set (BVar (KindOf PurusType))
  , declarations :: Set MonoBind
  }
  deriving (Show, Eq, Ord)

instance Pretty ToLift where
  pretty ToLift {..} =
    pretty $
      prettify
        [ "------ToLift-------"
        , "Var Scope:\n" <> prettyStr (S.toList varScopeAtDecl)
        , "Ty Var Scope:\n " <> prettyStr (S.toList tyVarScopeAtDecl)
        , "Decls:\n" <> prettyStr (S.toList declarations)
        , "-------------------"
        ]

oosInLiftResult :: LiftResult -> Map Ident (Set (BVar PurusType))
oosInLiftResult (LiftResult lifted body) =
  let liftedScope = foldBinds (\acc (nm,indx) e -> S.insert (BVar indx (expTy' id e) nm) acc) S.empty lifted
      oosInLifted = foldBinds (\acc (nm,_) e -> M.insert nm (asExp e $ findOutOfScopeVars' liftedScope ) acc) M.empty lifted
      oosInMain   = M.singleton (Ident "$MAIN") (findOutOfScopeVars' liftedScope body)
  in oosInMain <> oosInLifted

findOutOfScopeVars :: MonoExp -> S.Set (BVar PurusType)
findOutOfScopeVars = findOutOfScopeVars' S.empty

findOutOfScopeVars' :: S.Set (BVar PurusType) -> MonoExp -> S.Set (BVar PurusType)
findOutOfScopeVars' inScopeVars = \case
  V (B bv) | bv `S.notMember` inScopeVars -> S.singleton bv
  V _ -> S.empty
  LitE _ (ObjectL _ fs ) -> S.unions (findOutOfScopeVars' inScopeVars . snd <$> fs)
  LitE _ _ -> S.empty
  AppE e1 e2 -> findOutOfScopeVars' inScopeVars e1 <> findOutOfScopeVars' inScopeVars e2
  LamE bv body -> asExp body $ findOutOfScopeVars' (S.insert bv inScopeVars)
  LetE bs body ->
    let newScope = foldBinds (\acc (nm,indx) e -> S.insert (BVar indx (expTy' id e) nm) acc) inScopeVars bs
        oosInBs  = S.unions $ foldBinds (\acc _ e -> S.insert (asExp e (findOutOfScopeVars' newScope)) acc) S.empty bs
    in oosInBs <> asExp body (findOutOfScopeVars' newScope)
  AccessorE _ _ _ e -> findOutOfScopeVars' inScopeVars e
  ObjectUpdateE _ _ e _ fs -> findOutOfScopeVars' inScopeVars e <> S.unions (findOutOfScopeVars' inScopeVars . snd <$> fs)
  CaseE _ scrut alts -> findOutOfScopeVars' inScopeVars scrut <> S.unions (goAlt inScopeVars <$> alts)
  TyInstE _ e -> findOutOfScopeVars' inScopeVars e
  TyAbs _ e -> findOutOfScopeVars' inScopeVars e
 where
  goAlt :: S.Set (BVar PurusType)
        -> Alt WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)
        -> S.Set (BVar PurusType)
  goAlt inScope (UnguardedAlt pat body) =
    let newScope = getPatBinders inScope pat <> inScope
    in asExp body $ findOutOfScopeVars' newScope
  getPatBinders :: S.Set (BVar PurusType)
                -> Pat WithObjects PurusType (Exp WithObjects PurusType) (Vars PurusType)
                -> S.Set (BVar PurusType)
  getPatBinders scop = \case
    VarP ident indx ty -> S.singleton (BVar indx ty ident)
    LitP (ObjectL _ fs) -> S.unions (getPatBinders scop . snd <$> fs)
    LitP _ -> S.empty
    ConP _ _ ps -> S.unions (getPatBinders scop <$> ps)
    WildP -> S.empty
