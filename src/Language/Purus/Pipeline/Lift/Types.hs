module Language.Purus.Pipeline.Lift.Types where

import Prelude

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

import Bound (Scope, Var (F))
import Prettyprinter

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
