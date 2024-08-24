module Language.Purus.Make where

{-
decodeModuleIR :: FilePath -> IO (Module IR_Decl SourceType SourceType Ann, (Int, M.Map Ident Int))
decodeModuleIR path = do
  myMod <- decodeModuleIO path
  case desugarCoreModule myMod of
    Left err -> throwIO $ userError err
    Right myModIR -> pure myModIR

testDesugarObjects :: FilePath -> Text -> IO (Exp WithoutObjects Ty (Vars Ty))
testDesugarObjects path decl = do
  (myMod, ds) <- decodeModuleIR path
  Just myDecl <- pure . fmap snd $ findDeclBody decl myMod
  case runMonomorphize myMod [] (toExp myDecl) of
    Left (MonoError msg) -> throwIO $ userError $ "Couldn't monomorphize " <> T.unpack decl <> "\nReason:\n" <> msg
    Right body -> case evalStateT (tryConvertExpr body) ds of
      Left convertErr -> throwIO $ userError convertErr
      Right e -> do
        putStrLn (ppExp e)
        pure e

prepPIR ::
  FilePath ->
  Text ->
  IO (Exp WithoutObjects Ty (Vars Ty), Datatypes Kind Ty)
prepPIR path decl = do
  (myMod@Module {..}, ds) <- decodeModuleIR path

  desugaredExpr <- case snd <$> findDeclBody decl myMod of
    Nothing -> throwIO $ userError "findDeclBody"
    Just expr -> pure expr
  case runMonomorphize myMod [] (toExp desugaredExpr) of
    Left (MonoError msg) ->
      throwIO $
        userError $
          "Couldn't monomorphize "
            <> T.unpack (runModuleName moduleName <> ".main")
            <> "\nReason:\n"
            <> msg
    Right body -> do
      putStrLn (ppExp body)
      case evalStateT (tryConvertExpr body) ds of
        Left convertErr -> throwIO $ userError convertErr
        Right e -> do
          moduleDataTypes' <-
            either (throwIO . userError) pure $
              bitraverseDatatypes
                tryConvertKind
                tryConvertType
                moduleDataTypes
          putStrLn $ "tryConvertExpr result:\n" <> ppExp e <> "\n" <> replicate 20 '-'
          pure (e, moduleDataTypes')
-}
