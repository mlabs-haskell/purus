-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Hardy Jones 2014
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Hardy Jones <jones3.hardy@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Generate Directed Graphs of PureScript TypeClasses
--
-----------------------------------------------------------------------------

module Command.Hierarchy (command) where

import Prelude
import Protolude (catMaybes)

import Control.Applicative (optional)
import Data.Foldable (for_)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (Parser)
import Options.Applicative qualified as Opts
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, stderr)
import System.IO.UTF8 (readUTF8FilesT)
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Hierarchy (Graph(..), _unDigraph, _unGraphName, typeClasses)

data HierarchyOptions = HierarchyOptions
  { _hierarchyInput   :: FilePath
  , _hierarchyOutput :: Maybe FilePath
  }

parseInput :: [FilePath] -> IO (Either P.MultipleErrors [P.Module])
parseInput paths = do
  content <- readUTF8FilesT paths
  return $ map (snd . snd) <$> CST.parseFromFiles id content

compile :: HierarchyOptions -> IO ()
compile (HierarchyOptions inputGlob mOutput) = do
  input <- glob inputGlob
  modules <- parseInput input
  case modules of
    Left errs -> hPutStr stderr (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> exitFailure
    Right ms -> do
      for_ (catMaybes $ typeClasses ms) $ \(Graph name graph) ->
        case mOutput of
          Just output -> do
            createDirectoryIfMissing True output
            T.writeFile (output </> T.unpack (_unGraphName name)) (_unDigraph graph)
          Nothing -> T.putStrLn (_unDigraph graph)
      exitSuccess

inputFile :: Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.value "main.purs"
  <> Opts.showDefault
  <> Opts.help "The input file to generate a hierarchy from"

outputFile :: Parser (Maybe FilePath)
outputFile = optional . Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.help "The output directory"

pscOptions :: Parser HierarchyOptions
pscOptions = HierarchyOptions <$> inputFile
                              <*> outputFile

command :: Opts.Parser (IO ())
command = compile <$> (Opts.helper <*> pscOptions)
