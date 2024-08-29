module Language.Purus.Pipeline.Inline.Types where

import Prelude

import Language.PureScript.Names ( Ident )

import Language.Purus.Pipeline.Lift.Types ( MonoScoped )

import Prettyprinter ( Pretty )

newtype LoopBreakerScore = LoopBreakerScore {getScore :: ((Ident, Int), Maybe Int)} deriving (Show, Eq, Ord)

newtype LoopBreaker = LoopBreaker {getLoopBreaker :: (Ident, Int)}
  deriving newtype (Show, Eq, Ord, Pretty)

data InlineBodyData
  = NotALoopBreaker MonoScoped
  | IsALoopBreaker MonoScoped
  deriving (Show, Eq)

-- idk this is ugly. also isn't there some category theory magic for this?
unBodyData :: InlineBodyData -> (MonoScoped -> InlineBodyData, MonoScoped)
unBodyData = \case
  NotALoopBreaker ms -> (NotALoopBreaker, ms)
  IsALoopBreaker ms -> (IsALoopBreaker, ms)

getInlineBody :: InlineBodyData -> MonoScoped
getInlineBody = \case
  NotALoopBreaker s -> s
  IsALoopBreaker s -> s

traverseBodyData :: (Applicative f) => (MonoScoped -> f MonoScoped) -> InlineBodyData -> f InlineBodyData
traverseBodyData f = \case
  NotALoopBreaker b -> NotALoopBreaker <$> f b
  IsALoopBreaker b -> IsALoopBreaker <$> f b

isALoopBreaker :: InlineBodyData -> Bool
isALoopBreaker = \case IsALoopBreaker {} -> True; _ -> False

notALoopBreaker :: InlineBodyData -> Bool
notALoopBreaker = \case NotALoopBreaker {} -> True; _ -> False
