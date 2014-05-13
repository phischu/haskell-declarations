{-# LANGUAGE OverloadedStrings #-}
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
import Data.Tagged
import Data.Maybe
import qualified Data.Foldable as F
import System.FilePath
import Text.Printf
import Data.Aeson (encode,ToJSON(toJSON))
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import qualified Language.Haskell.Exts.Annotated as HSE (Module,Decl,SrcSpan,ModuleName)
import Language.Haskell.Exts.Annotated (Decl(..))
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Set as Set (fromList)

import Distribution.HaskellSuite
import qualified Distribution.HaskellSuite.Compiler as Compiler

import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Verbosity

import Language.Haskell.Names (
    Symbols(Symbols),annotateModule,Scoped(Scoped),SymValueInfo,SymTypeInfo,OrigName,
    NameInfo(GlobalValue,GlobalType))
import Language.Haskell.Names.SyntaxUtils (getModuleDecls,getModuleName)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)

import Distribution.ModuleName (ModuleName)
import Distribution.Text (display)

import Data.Aeson (encode,ToJSON(toJSON),object,(.=))

import qualified Data.ByteString.Lazy as ByteString (writeFile)
import Control.Monad (forM_,when)
import Data.Either (partitionEithers)
import qualified Data.Set as Set (fromList)
import Data.Foldable (foldMap)

import Language.Haskell.Exts.Annotated.CPP
import Language.Haskell.Names.SyntaxUtils

main :: IO ()
main =
  Compiler.main theTool

version :: Data.Version.Version
version = Data.Version.Version [0,1] []

suffix :: String
suffix = "declarations"

data DeclarationsDB = DeclarationsDB

instance IsDBName DeclarationsDB where
    getDBName = Tagged "haskell-declarations"

nameFilesExtension :: FilePath
nameFilesExtension = "declarations"

theTool :: Compiler.Simple (StandardDB DeclarationsDB)
theTool =
  Compiler.simple
    "haskell-delcarations"
    version
    knownLanguages
    knownExtensions
    compile
    [nameFilesExtension]

fixCppOpts :: CpphsOptions -> CpphsOptions
fixCppOpts opts =
  opts {
    defines = ("__GLASGOW_HASKELL__", "706") : defines opts, -- FIXME
    preInclude = "cabal_macros.h" : preInclude opts,
    includes = "/usr/lib/ghc/include/": includes opts
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

  print packagedbs

  globalpackages <- getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB
  localpackages <- getInstalledPackages (Proxy :: Proxy NamesDB) UserPackageDB

  let packages = globalpackages ++ localpackages      

  forM_ modules (\moduleast -> do

    let HSE.ModuleName _ modulename = getModuleName moduleast
        declarationsfilename = builddirectory </> toFilePath (fromString modulename) <.> nameFilesExtension

    createDirectoryIfMissingVerbose silent True (dropFileName declarationsfilename)

    annotatedmoduleast <- evalNamesModuleT (annotateModule Haskell2010 [] moduleast) packages

    let declarations = extractDeclarations (getModuleName annotatedmoduleast) annotatedmoduleast

    ByteString.writeFile declarationsfilename (encode declarations))

extractDeclarations :: HSE.ModuleName (Scoped HSE.SrcSpan) -> HSE.Module (Scoped HSE.SrcSpan) -> [Declaration]
extractDeclarations modulenameast annotatedmoduleast = map (declToDeclaration modulenameast) (getModuleDecls annotatedmoduleast)

declToDeclaration :: HSE.ModuleName (Scoped HSE.SrcSpan) -> HSE.Decl (Scoped HSE.SrcSpan) -> Declaration
declToDeclaration modulenameast annotatedmoduleast = Declaration
    (declGenre annotatedmoduleast)
    (prettyPrint annotatedmoduleast)
    (declaredSymbols modulenameast annotatedmoduleast)
    (usedSymbols annotatedmoduleast)

declGenre :: HSE.Decl (Scoped HSE.SrcSpan) -> Genre
declGenre (TypeDecl _ _ _) = Type
declGenre (TypeFamDecl _ _ _) = Type
declGenre (DataDecl _ _ _ _ _ _) = Type
declGenre (GDataDecl _ _ _ _ _ _ _) = Type
declGenre (DataFamDecl _ _ _ _) = Type
declGenre (TypeInsDecl _ _ _) = Type
declGenre (DataInsDecl _ _ _ _ _) = Type
declGenre (GDataInsDecl _ _ _ _ _ _) = Type
declGenre (ClassDecl _ _ _ _ _) = TypeClass
declGenre (InstDecl _ _ _ _) = ClassInstance
declGenre (DerivDecl _ _ _) = ClassInstance
declGenre (TypeSig _ _ _) = TypeSignature
declGenre (FunBind _ _) = Value
declGenre (PatBind _ _ _ _ _) = Value
declGenre (ForImp _ _ _ _ _ _) = Value
declGenre _ = Other

declaredSymbols :: HSE.ModuleName (Scoped HSE.SrcSpan) -> HSE.Decl (Scoped HSE.SrcSpan) -> Symbols
declaredSymbols modulenameast annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulenameast annotatedmoduleast)

usedSymbols :: HSE.Decl (Scoped HSE.SrcSpan) -> Symbols
usedSymbols annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (foldMap externalSymbol annotatedmoduleast)

externalSymbol :: Scoped HSE.SrcSpan -> [Either (SymValueInfo OrigName) (SymTypeInfo OrigName)]
externalSymbol (Scoped (GlobalValue symvalueinfo) _) = [Left symvalueinfo]
externalSymbol (Scoped (GlobalType symtypeinfo) _) = [Right symtypeinfo]
externalSymbol _ = []

data Declaration = Declaration Genre DeclarationAST DeclaredSymbols UsedSymbols deriving (Show,Eq)
data Genre = Value | TypeSignature | Type | TypeClass | ClassInstance | Other deriving (Show,Eq)
type DeclarationAST = String
type DeclaredSymbols = Symbols
type UsedSymbols = Symbols

instance ToJSON Declaration where
  toJSON (Declaration genre declarationast declaredsymbols usedsymbols) = object [
        "genre" .= show genre,
        "declarationast" .= declarationast,
        "declaredsymbols" .= declaredsymbols,
        "usedsymbols" .= usedsymbols]
