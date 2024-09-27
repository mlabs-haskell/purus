-- TODO: Remove this module
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.PureScript.CodeGen.UPLC where

import Protolude (
  Maybe,
  Monad,
  MonadError,
  MonadIO (..),
  MonadReader,
  MonadState,
  print,
  undefined,
  ($),
  (.),
  (<$>),
 )
import Protolude.Error (error)
import Prelude (($), (.))

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.AST qualified as AST
import Language.PureScript.CoreFn (Ann, Bind, Expr (..), Literal (..), Meta, Module (..))
import Language.PureScript.Errors (MultipleErrors (..))
import Language.PureScript.Options (Options (..))

import Control.Monad.Writer.Class (MonadWriter)
import Data.String (IsString (fromString))
import Language.PureScript.Comments (Comment)
import Language.PureScript.Names (Ident (..))
import Language.PureScript.TypeChecker (CheckState)
import Language.PureScript.TypeChecker.Types (infer)
import Language.PureScript.Types (SourceType)
import Language.PureScript.Types qualified as T
import PlutusCore (someValue)
import PlutusCore qualified as PLC
import PlutusCore.Pretty (prettyPlcReadableSimple)
import PlutusIR qualified as PIR

-- Stolen from Ply, not 100% sure if this is what we want, i.e. maybe there should be an annotation?
type PIRProgram = PIR.Program PLC.TyName PLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()

type PIRTerm ann = PIR.Term PLC.TyName PLC.Name PLC.DefaultUni PLC.DefaultFun ann

sourceSpan :: Ann -> AST.SourceSpan
sourceSpan (x, _, _) = x

comments :: Ann -> [Comment]
comments (_, x, _) = x

meta :: Ann -> Maybe Meta
meta (_, _, x) = x

moduleToUPLC ::
  forall m.
  (MonadReader Options m, MonadSupply m, MonadError MultipleErrors m) =>
  Module (Bind Ann) SourceType SourceType Ann ->
  m PIRProgram
moduleToUPLC = error "Error: UPLC Backend not yet implemented!"

type M m = (Monad m, MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)

{-
transformExpr :: forall m b
              . M m
              => Expr Ann
              -> m (Expr (SourceType,Ann))
transformExpr = \case
  Literal ann cfnLit ->  case cfnLit of
   NumericLiteral x -> do
     TypedValue'  <- infer $  AST.Literal (sourceSpan ann) $ NumericLiteral x
     pure $ Literal
   StringLiteral psString -> f ann $ AST.Literal (sourceSpan ann) $ StringLiteral psString
   CharLiteral c -> f ann $ AST.Literal (sourceSpan ann) $ CharLiteral c
   BooleanLiteral b -> f ann $ AST.Literal (sourceSpan ann) $ BooleanLiteral b
   ArrayLiteral xs -> Literal $ ArrayLiteral $  foldExpr f <$> xs

  Constructor ann tyName ctorName fields -> undefined
  Accessor ann l t -> undefined
  ObjectUpdate a orig copyFields updateFields -> undefined
  Abs ann identifier body -> undefined
  App ann e1 e2 -> undefined
  Var ann qualIdent -> undefined
  Case ann es alts -> undefined
  Let ann binds expr -> undefined
-}

inferExprTypes ::
  forall m a.
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Expr a ->
  m (Expr (T.Type a))
inferExprTypes = \case
  _ -> undefined

{- | nil = constr 0 []
    cons x xs = constr 1 x xs

sopList :: forall name ann. ann -> [PIRTerm ann] -> PIRTerm ann
sopList ann = \case -- ann is the default annotation for an empty list
  [] -> PIR.Constr ann 0 []
  (x:xs) -> PIR.Constr ann 1 [x,sopList ann xs]



exprToTerm :: forall m  ann
            . (MonadReader Options m,
               MonadSupply m,
               MonadError MultipleErrors m,
               Monoid ann
            ) => Expr ann ->  m (PIRTerm  ann)
exprToTerm = \case
  Literal ann lit -> litToTerm ann lit
  Constructor ann tyName ctorName identifiers -> undefined
  Accessor ann label expr -> undefined
  ObjectUpdate ann expr copyFields updateFields -> undefined
  Abs ann identifier expr -> do
    name <- identifierToName identifier
    body <- exprToTerm expr
    pure $PIR.LamAbs ann name body
  App ann e1 e2 -> undefined
  Var ann qIdentifier -> undefined
  Case ann es cas -> undefined
  Let ann binds expr -> undefined
 where
   identifierToName :: Ident -> m PIR.Name
   identifierToName = \case
     GenIdent (Just nm) i -> pure $ PIR.Name nm (PLC.Unique $ fromIntegral i)
     _ -> error "WIP"

   litToTerm :: ann -> Literal (Expr ann) -> m (PIRTerm  ann)
   litToTerm a = \case
     NumericLiteral (Left integer) -> pure $ PIR.Constant a (someValue integer)
     NumericLiteral (Right _double) -> error "Figure out what to do w/ Doubles"
     StringLiteral psString -> do
       let bs :: ByteString = fromString (show psString)
       pure $ PIR.Constant a (someValue bs)
     CharLiteral _char -> error "Figure out what to do with Chars"
     BooleanLiteral boolean -> pure $ PIR.Constant a (someValue boolean)
     ArrayLiteral array -> sopList mempty <$> traverse exprToTerm array
     {\- ObjectLiterals, aka Record literals, get represented onchain as products with field order determined by lexicographic sorting of the labels.
     -\}
     ObjectLiteral fields -> do
       let sorted = map snd . sortOn fst $ fields -- these are probably already sorted somewhere, but not 100% sure
       terms <- traverse exprToTerm sorted
       pure $ PIR.Constr a 0 terms -- the evaluator should use 0 based indices? i hope?
-}
printUPLC :: forall m. (MonadIO m) => PIRProgram -> m ()
printUPLC program = liftIO . print $ prettyPlcReadableSimple program
