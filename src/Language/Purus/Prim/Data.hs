{- Contains primitive Purus datatypes.

   Unlike PureScript, where (e.g.) `true/false` are
   morally "foreign imports" and `Array` is an opaque primitive type,
   our `Boolean` and `Array` (TODO: CHANGE THE NAME TO LIST) are
   real algebraic datatypes with constructors & so on, and therefore
   we need to provide definitions for their constructors.

   We also create tuples here. They're directly exposed to users
   (albeit in the somewhat ugly Tuple1, Tuple2, ... form), but, more importantly,
   we need tuples (qua anonymous products) to eliminate Records (which
   Plutus has no notion of). 

-}

module Language.Purus.Prim.Data where

import Prelude

import Data.Map (Map)
import Data.Map qualified as M

import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.AST.SourcePos (pattern NullSourceAnn, SourceAnn)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Desugar.Utils (properToIdent)
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module (
  CtorDecl (CtorDecl),
  DataDecl (DataDecl),
  Datatypes (Datatypes),
  dDataTyName,
 )
import Language.PureScript.Environment (
  DataDeclType (Data),
  kindType,
  mkTupleTyName,
 )
import Language.PureScript.Names (
  Ident (Ident, UnusedIdent),
  ProperName,
  ProperNameType (TypeName),
  Qualified (..),
  QualifiedBy (ByModuleName),
 )
import Language.PureScript.Types (Type (..), SourceType)

import Language.Purus.IR
    ( Kind(KindType), Ty(TyVar, TyApp, TyCon) )
import Language.Purus.Config ( maxTupleSize )

import Control.Lens ((<&>), (^.))

pattern ArrayCons :: Qualified Ident
pattern ArrayCons = Qualified (ByModuleName C.M_Prim) (Ident "Cons")

pattern ArrayNil :: Qualified Ident
pattern ArrayNil = Qualified (ByModuleName C.M_Prim) (Ident "Nil")


mkProdFields :: [t] -> [(Ident, t)]
mkProdFields = map (UnusedIdent,)

na :: SourceAnn
na = NullSourceAnn

primData :: Datatypes Kind Ty
primData = tupleDatatypes <> Datatypes tDict cDict
  where
    tDict :: Map (Qualified (ProperName 'TypeName)) (DataDecl Kind Ty)
    tDict =
      M.fromList $
        map
          (\x -> (x ^. dDataTyName, x))
          [ DataDecl
              Data
              C.Array
              [("a", KindType)]
              [ CtorDecl ArrayNil []
              , CtorDecl ArrayCons $ mkProdFields [TyVar "a" KindType, TyApp (TyCon C.Array) (TyVar "a" KindType)]
              ]
          , DataDecl
              Data
              C.Boolean
              []
              [ CtorDecl (properToIdent <$> C.C_False) []
              , CtorDecl (properToIdent <$> C.C_True) []
              ]
          ]

    cDict :: Map (Qualified Ident) (Qualified (ProperName 'TypeName))
    cDict =
      M.fromList
        [ (ArrayCons, C.Array)
        , (ArrayNil, C.Array)
        , (properToIdent <$> C.C_True, C.Boolean)
        , (properToIdent <$> C.C_False, C.Boolean)
        ]

tupleDatatypes :: Datatypes Kind Ty
tupleDatatypes = Datatypes (M.fromList tupleTypes) (M.fromList tupleCtors)
  where
    tupleTypes :: [(Qualified (ProperName 'TypeName), DataDecl Kind Ty)]
    tupleTypes = flip map [0 .. maxTupleSize] $ \(n :: Int) ->
      let tyNm = mkTupleTyName n
          ctorNm = mkTupleCtorIdent n
          argKinds = mkTupleArgKinds n
          ctorTvArgs = mkTupleCtorTvArgs n
       in (tyNm, DataDecl Data tyNm argKinds [CtorDecl ctorNm ctorTvArgs])

    tupleCtors :: [(Qualified Ident, Qualified (ProperName 'TypeName))]
    tupleCtors = [0 .. maxTupleSize] <&> \x -> (mkTupleCtorIdent x, mkTupleTyName x)

    mkTupleCtorIdent :: Int -> Qualified Ident
    mkTupleCtorIdent n = properToIdent <$> mkTupleTyName n

    vars :: Int -> [Text]
    vars n = map (\x -> "t" <> T.pack (show x)) [1 .. n]

    mkTupleArgKinds :: Int -> [(Text, Kind)]
    mkTupleArgKinds = fmap (,KindType) . vars

    mkTupleCtorTvArgs :: Int -> [(Ident, Ty)]
    mkTupleCtorTvArgs = mkProdFields . map (flip TyVar KindType) . vars

primDataPS :: Datatypes PurusType PurusType
primDataPS = tupleDatatypesPS <> Datatypes tDict cDict
  where
    tDict :: Map (Qualified (ProperName 'TypeName)) (DataDecl PurusType PurusType)
    tDict =
      M.fromList $
        map
          (\x -> (x ^. dDataTyName, x))
          [ DataDecl
              Data
              C.Array
              [("a", kindType)]
              [ CtorDecl ArrayNil []
              , CtorDecl ArrayCons $ mkProdFields [TypeVar na "a" kindType, TypeApp na (TypeConstructor na C.Array) (TypeVar na "a" kindType)]
              ]
          , DataDecl
              Data
              C.Boolean
              []
              [ CtorDecl (properToIdent <$> C.C_False) []
              , CtorDecl (properToIdent <$> C.C_True) []
              ]
          ]

    cDict :: Map (Qualified Ident) (Qualified (ProperName 'TypeName))
    cDict =
      M.fromList
        [ (ArrayCons, C.Array)
        , (ArrayNil, C.Array)
        , (properToIdent <$> C.C_True, C.Boolean)
        , (properToIdent <$> C.C_False, C.Boolean)
        ]

tupleDatatypesPS :: Datatypes PurusType PurusType
tupleDatatypesPS = Datatypes (M.fromList tupleTypes) (M.fromList tupleCtors)
  where
    tupleTypes :: [(Qualified (ProperName 'TypeName), DataDecl PurusType PurusType)]
    tupleTypes = flip map [0 .. maxTupleSize] $ \(n :: Int) ->
      let tyNm = mkTupleTyName n
          ctorNm = mkTupleCtorIdent n
          argKinds = mkTupleArgKinds n
          ctorTvArgs = mkTupleCtorTvArgs n
       in (tyNm, DataDecl Data tyNm argKinds [CtorDecl ctorNm ctorTvArgs])

    tupleCtors :: [(Qualified Ident, Qualified (ProperName 'TypeName))]
    tupleCtors = [0 .. maxTupleSize] <&> \x -> (mkTupleCtorIdent x, mkTupleTyName x)

    mkTupleCtorIdent :: Int -> Qualified Ident
    mkTupleCtorIdent n = properToIdent <$> mkTupleTyName n

    vars :: Int -> [Text]
    vars n = map (\x -> "t" <> T.pack (show x)) [1 .. n]

    mkTupleArgKinds :: Int -> [(Text, SourceType)]
    mkTupleArgKinds = fmap (,kindType) . vars

    mkTupleCtorTvArgs :: Int -> [(Ident, Type SourceAnn)]
    mkTupleCtorTvArgs = mkProdFields . map (\v -> TypeVar na v kindType) . vars
