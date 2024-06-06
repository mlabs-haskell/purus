module Language.PureScript.CoreFn.Ann where

import Prelude

import Language.PureScript.AST.SourcePos (SourceSpan, pattern NullSourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Meta (Meta)

-- |
-- Type alias for basic annotations
--
type Ann = (SourceSpan, [Comment], Maybe Meta)

-- |
-- An annotation empty of metadata aside from a source span.
--
ssAnn :: SourceSpan -> Ann
ssAnn ss = (ss, [], Nothing)

annSS :: Ann -> SourceSpan
annSS (ss,_,_) = ss

nullAnn :: Ann
nullAnn = (NullSourceSpan, [] , Nothing)

-- |
-- Remove the comments from an annotation
--
removeComments :: Ann -> Ann
removeComments (ss, _, meta) = (ss, [], meta)
