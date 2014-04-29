module Main where

import qualified Language.Haskell.Exts.Annotated as HSE
import qualified Language.Haskell.Exts as UnAnn
import Language.Haskell.Exts (defaultParseMode, ParseMode(..))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
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

theTool :: Compiler.Simple NamesDB
theTool =
  Compiler.simple
    "haskell-names"
    version
    knownLanguages
    knownExtensions
    compile
    ["declarations"]

compile :: Compiler.CompileFn
compile buildDir mbLang exts cppOpts pkgName pkgdbs deps files = do
    print "hello world"
