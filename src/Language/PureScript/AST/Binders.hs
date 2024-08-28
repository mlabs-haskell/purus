-- |
-- Case binders
--
module Language.PureScript.AST.Binders where

import           Prelude

import           Language.PureScript.AST.Literals  (Literal (..))
import           Language.PureScript.AST.SourcePos (SourceSpan)
import           Language.PureScript.Comments      (Comment)
import           Language.PureScript.Names         (Ident, OpName,
                                                    OpNameType (..), ProperName,
                                                    ProperNameType (..),
                                                    Qualified)
import           Language.PureScript.Types         (SourceType)

data BinderAtom
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder SourceSpan Ident
  deriving (Show)

instance Eq BinderAtom where
  NullBinder == NullBinder = True
  VarBinder _ a == VarBinder _ b = a == b
  _ == _ = False

instance Ord BinderAtom where
  compare NullBinder NullBinder = EQ
  compare (VarBinder _ ident) (VarBinder _ ident') =
    compare ident ident'
  compare binder binder' =
    compare (orderOf binder) (orderOf binder')
      where
        orderOf :: Binder -> Int
        orderOf NullBinder             = 0
        orderOf VarBinder{}            = 1

-- |
-- Data type for binders
--
data Binder
  -- |
  -- A binder which matches a data constructor
  --
  = ConstructorBinder SourceSpan (Qualified (ProperName 'ConstructorName)) [BinderAtom]
  -- |
  -- A binder that contains a list of variables
  | BinderAtoms [BinderAtom]
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder BinderAtom (Qualified (OpName 'ValueOpName)) BinderAtom
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder SourceSpan Ident Binder
  -- |
  -- A binder with source position information
  --
  | PositionedBinder SourceSpan [Comment] Binder
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder SourceType Binder
  deriving (Show)

-- Manual Eq and Ord instances for `Binder` were added on 2018-03-05. Comparing
-- the `SourceSpan` values embedded in some of the data constructors of `Binder`
-- was expensive. This made exhaustiveness checking observably slow for code
-- such as the `explode` function in `test/purs/passing/LargeSumTypes.purs`.
-- Custom instances were written to skip comparing the `SourceSpan` values. Only
-- the `Ord` instance was needed for the speed-up, but I did not want the `Eq`
-- to have mismatched behavior.
instance Eq Binder where
  (ConstructorBinder _ qpc bs) == (ConstructorBinder _ qpc' bs') =
    qpc == qpc' && bs == bs'
  BinderAtoms a == BinderAtoms b = a == b
  (BinaryNoParensBinder b1 b2 b3) == (BinaryNoParensBinder b1' b2' b3') =
    b1 == b1' && b2 == b2' && b3 == b3'
  (ParensInBinder b) == (ParensInBinder b') =
    b == b'
  (NamedBinder _ ident b) == (NamedBinder _ ident' b') =
    ident == ident' && b == b'
  (PositionedBinder _ comments b) == (PositionedBinder _ comments' b') =
    comments == comments' && b == b'
  (TypedBinder ty b) == (TypedBinder ty' b') =
    ty == ty' && b == b'
  _ == _ = False

instance Ord Binder where
  compare (ConstructorBinder _ qpc bs) (ConstructorBinder _ qpc' bs') =
    compare qpc qpc' <> compare bs bs'
  compare (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    compare b1 b1' <> compare b2 b2' <> compare b3 b3'
  compare (ParensInBinder b) (ParensInBinder b') =
    compare b b'
  compare (NamedBinder _ ident b) (NamedBinder _ ident' b') =
    compare ident ident' <> compare b b'
  compare (PositionedBinder _ comments b) (PositionedBinder _ comments' b') =
    compare comments comments' <> compare b b'
  compare (TypedBinder ty b) (TypedBinder ty' b') =
    compare ty ty' <> compare b b'
  compare binder binder' =
    compare (orderOf binder) (orderOf binder')
      where
        orderOf :: Binder -> Int
        orderOf ConstructorBinder{}    = 0
        orderOf BinderAtoms{}          = 1
        orderOf BinaryNoParensBinder{} = 3
        orderOf ParensInBinder{}       = 4
        orderOf NamedBinder{}          = 5
        orderOf PositionedBinder{}     = 6
        orderOf TypedBinder{}          = 7

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder -> [Ident]
binderNames = map snd . binderNamesWithSpans

binderNamesWithSpans :: Binder -> [(SourceSpan, Ident)]
binderNamesWithSpans = go []
  where
  go ns (ConstructorBinder _ _ bs)      = foldl go ns $ uncurry VarBinder <$> bs
  go ns (BinderAtoms atoms)             = ns ++ concatMap binderAtomNamesWithSpans atoms
  go ns (BinaryNoParensBinder b1 b2 b3) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder b)              = go ns b
  go ns (NamedBinder ss name b)         = go ((ss, name) : ns) b
  go ns (PositionedBinder _ _ b)        = go ns b
  go ns (TypedBinder _ b)               = go ns b
  go ns _                               = ns

binderAtomNamesWithSpans :: BinderAtom -> [(SourceSpan, Ident)]
binderAtomNamesWithSpans NullBinder = []
binderAtomNamesWithSpans (VarBinder span ident) = [(span, ident)]

isIrrefutable :: Binder -> Bool
isIrrefutable NullBinder               = True
isIrrefutable (VarBinder _ _)          = True
isIrrefutable (PositionedBinder _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ b)        = isIrrefutable b
isIrrefutable _                        = False
