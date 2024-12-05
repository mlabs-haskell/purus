{-# LANGUAGE TemplateHaskell #-}

module Language.Purus.Pipeline.EliminateCases.Types where

import Prelude

import Language.PureScript.Names (
    Ident (..),
    ProperName (..),
    ProperNameType (..),
    Qualified (..),
 )
import Language.Purus.IR (
    BVar (..),
    Exp (..),
    FVar (..),
    Lit (..),
    Pat (..),
    Ty (..),
 )
import Language.Purus.IR.Utils (
    Vars,
    WithoutObjects,
 )
import Bound (Var (..))
import Control.Lens (
    makeLenses,
 )
import Language.Purus.Pipeline.GenerateDatatypes.Utils () -- FIXME: Move the damn pretty instance for the Qualified stuff out of here -_-
import Prettyprinter
    ( Doc,
      Pretty(pretty),
      (<+>),
      align,
      group,
      hardline,
      hsep,
      indent,
      punctuate,
      brackets,
      parens, vsep )


{- Honestly I think keeping this is fine. At this point in the compilation pipeline, these are *obviously*
   the only parameters of Exp/Pat/Lit that we have access to. We shouldn't export them but for local module purposes
   they are fine.
-}

type Pattern = Pat WithoutObjects Ty (Exp WithoutObjects Ty) (Var (BVar Ty) (FVar Ty))
type Expression = Exp WithoutObjects Ty (Vars Ty)
-- If you look at the types, this can only represent "true literals", i.e., is un-nested
type Literal = Lit WithoutObjects Pattern

{- Newtype for constructor argument position. By convention should start at 0, such that (e.g.)
   the argument position of `x` in `Just x` is == 0.
-}
newtype CtorArgPos = CtorArgPos Int deriving (Show, Eq, Ord)

instance Pretty CtorArgPos where
  pretty (CtorArgPos i) = "ARG#" <> pretty i

{- Newtype for constructor indices. Only really exists so we don't mix up ArgPos and Index
   (which is very easy to do). Start at 0 as you'd expect from Plutus.
-}
newtype CtorIx = CtorIx Int deriving (Show, Eq, Ord)

newtype ResultRowIx = ResultRowIx {getResultRow :: Int} deriving (Show, Eq, Ord)

getPosition :: PatternConstraint -> Position
getPosition (pos :@ _) = pos

getContent :: PatternConstraint -> ConstraintContent
getContent (_ :@ content) = content

getCix :: CtorIx -> Int
getCix (CtorIx i) = i

getArgPos :: CtorArgPos -> Int
getArgPos (CtorArgPos i) = i

instance Pretty CtorIx where
  pretty (CtorIx i) = "IX#" <> pretty i

{- An indexing scheme that relates a given constraint to a (possible) location in the structure of the scrutinees.

   A position is either a ScrutineeRef, which points to a "top level" scrutinee, or a constructor argument position.

   When eliminating nested cases with only one scrutinee, there will only be one ScrutineeRef, and the index will always be 0. For multiple scrutinee cases,
   there can be an arbitrary number (up to the compiler's maxTupleSize limit).

   Constructor argument positions point to the field of some constructor, and can be arbitrarily nested, but must contain
   a ScrutineeRef "root position". This invariant is enforced by the type.

-}
data Position
    = -- The index of the scrutinee in the list of scrutinees
      ScrutineeRef Int
    | -- A constructor argument local-position (index, argument position) indexed to a position in the matrix
      -- (can nest these but the structure of the type should ensure that we always end up with a matrix position root)
      ConstructorArgPos (Qualified (ProperName 'TypeName)) Position CtorIx CtorArgPos
    deriving (Show, Eq)

instance Pretty Position where
  pretty = \case
    ScrutineeRef i -> "M" <> parens (pretty i)
    ConstructorArgPos _ pos ix argpos -> "CTORARG" <> brackets (hsep [pretty pos, pretty ix, pretty argpos])

instance Ord Position where
    compare (ScrutineeRef p1) (ScrutineeRef p2) = compare p1 p2
    compare (ScrutineeRef _) _ = LT
    compare (ConstructorArgPos tn1 p1 cix1 argpos1) (ConstructorArgPos tn2 p2 cix2 argpos2)
      = case compare tn1 tn2 of
        EQ -> case compare p1 p2 of
         EQ -> case compare cix1 cix2 of
             EQ -> compare argpos1 argpos2
             other -> other
         other -> other
        other -> other
    compare ConstructorArgPos {} ScrutineeRef {} = GT

{- An Identifer is either a PureScript (Ident,Int) pair or a placeholder identifier that will be replaced with a
   fresh variable name/index in the cleanup phase. This isn't *necessary* but it lets us keep the functions here
   self-contains and "pure" (in the sense that we don't need to do all of this in one of the pipeline monads)

   The Int in LocalIdentifiers does not have any particular significance (i.e. it's not a Unique index, but should be unique
   relative to other identifiers generated in the transformations here).

   We need to keep these distinct - Local variables always originate from the expansion of nested cases and will *always* be
   further refined by deeper children in the constraint tree.
-}
data Identifier
    = PSVarData Ident Int Ty -- Something that came from a variable in the input AST
    | LocalIdentifier Int Ty -- Something we had to generate during expansion
    deriving (Show, Eq, Ord)

instance Pretty Identifier where
  pretty = \case
    PSVarData idnt indx ty -> parens $ pretty idnt <> "#" <> pretty indx <+> "::" <+> pretty ty -- (BVar indx ty idnt)
    LocalIdentifier i ty -> parens $ "LOCAL#" <> pretty i <+> "::" <+> pretty ty

{- This is just `Pat` from the IR, but with constructor indices instead of names (which is mostly stylistic, it's just easier to think about indices here)
   and with two sorts of variable. Sometimes we treat Local synthetic vars and "real" AST vars as equivalent, but sometimes we have to distinguish them.
-}
data ConstraintContent
    = VarC Identifier
    | WildC
    | LitC Literal
    | Constructor (Qualified (ProperName 'TypeName)) CtorIx [PatternConstraint]
    deriving (Show, Eq)

instance Pretty ConstraintContent where
  pretty = \case
    VarC i -> pretty i
    WildC  -> "_"
    LitC lit -> pretty lit
    Constructor qtn ctorIx inner -> parens $ pretty qtn <+> pretty ctorIx <+> brackets (hsep . punctuate "," $ pretty . getContent <$> inner)

{- A constraint paired with a position. Yeah it's just a tuple, but we use it so often that
   it's worth having a data type for it (if nothing else it keeps signatures readable)
-}
data PatternConstraint = Position :@ ConstraintContent
    deriving (Show, Eq)

instance Pretty PatternConstraint where
  pretty (pos :@ c) = pretty pos <+> ":=" <+> pretty c

{- Data types for the case expression skeleton.

   At this point it still makes more sense to work with an abstract notion of
   position instead of explicit named variables. All of the variables which occur in the
   outer scrutinee(s) can easily be replaced at the end, and all of the "new" variables
   must have their types deduced - best to keep that task separate.

   A skeleton is either a prototype case expression (with a Position and a list of (Constraint,Skeleton) pairs)
   or a result conjoined with the necessary variable re-bindings. (I think? Hope I got this right)
-}

data Result = Result Expression [(Position, Identifier, Identifier)]

instance Pretty Result where
  pretty (Result e rebinds) = pretty e <+> pretty rebinds

data CaseOf a = CaseOf Position [(ConstraintContent, Either (CaseOf a) a)]

instance Pretty a => Pretty (CaseOf a) where
  pretty (CaseOf pos inner) = "case" <+> pretty pos <+> "of" <> hardline <> indent 2 (align . group $ vsep (goInner <$> inner))
   where
     goInner :: forall x. (ConstraintContent, Either (CaseOf a)  a) -> Doc x
     goInner (ccontent, rest) = pretty ccontent <+> "->" <+> case rest of
       Left caseOf -> pretty caseOf
       Right a -> pretty a
{- Data type for binding context. Needs to track variables bound in each position.
-}

data BindingContext = BindingContext
    { _currentPath :: [PatternConstraint]
    }

makeLenses ''BindingContext
