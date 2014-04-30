module Main where

import qualified Language.Haskell.Exts.Annotated as HSE
import qualified Language.Haskell.Exts as UnAnn
import Language.Haskell.Exts (defaultParseMode, ParseMode(..))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Parser (fromParseResult)
import Control.Monad
import Control.Exception
import Data.Version
import Data.Typeable
import Data.Proxy
import Data.Maybe
import qualified Data.Foldable as F
import System.FilePath
import Text.Printf

import Distribution.HaskellSuite
import qualified Distribution.HaskellSuite.Compiler as Compiler

import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Verbosity

import Language.Haskell.Exts.Annotated.CPP
import Language.Haskell.Names.SyntaxUtils

main :: IO ()
main =
  Compiler.main theTool

version :: Data.Version.Version
version = Data.Version.Version [7,6,3] []

suffix :: String
suffix = "declarations"

data DeclarationsDB = DeclarationsDB

instance IsDBName DeclarationsDB where
    getDBName = undefined

nameFilesExtension :: FilePath
nameFilesExtension = "declarations"

theTool :: Compiler.Simple (StandardDB DeclarationsDB)
theTool =
  Compiler.simple
    "haskell-names"
    version
    knownLanguages
    knownExtensions
    compile
    [nameFilesExtension]

fixCppOpts :: CpphsOptions -> CpphsOptions
fixCppOpts opts =
  opts {
    defines = ("__GLASGOW_HASKELL__", "706") : defines opts, -- FIXME
    preInclude = "cabal_macros.h" : preInclude opts
  }

parse :: Language -> [Extension] -> CpphsOptions -> FilePath -> IO (HSE.Module HSE.SrcSpan)
parse lang exts cppOpts file = do
    parseresult <- parseFileWithCommentsAndCPP (fixCppOpts cppOpts) mode file
    return (fmap srcInfoSpan (fst (fromParseResult parseresult)))
  where
    mode = defaultParseMode
             { UnAnn.parseFilename   = file
             , baseLanguage          = lang
             , extensions            = exts
             , ignoreLanguagePragmas = False
             , ignoreLinePragmas     = False
             }

compile :: Compiler.CompileFn
compile builddirectory maybelanguage extensions cppoptions packagename packagedbs dependencies files = do
  let language = fromMaybe Haskell98 maybelanguage

  modules <- mapM (parse language extensions cppoptions) files

  forM_ modules (\modul -> do

    let HSE.ModuleName _ modulename = getModuleName modul
        declarationsfilename = builddirectory </> toFilePath (fromString modulename) <.> nameFilesExtension

    createDirectoryIfMissingVerbose silent True (dropFileName declarationsfilename)

    print ("working on: " ++ show packagename))
