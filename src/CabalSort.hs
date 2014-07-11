module Main where

import qualified Distribution.PackageDescription as P
import Distribution.PackageDescription
         (GenericPackageDescription, package, packageDescription, )
import Distribution.PackageDescription.Parse (readPackageDescription, )
import Distribution.Package
         (Dependency(Dependency), PackageName(PackageName), pkgName, )

import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ReadE as ReadE

import System.Console.GetOpt
          (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Exit (exitSuccess, exitFailure, )
import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

import Data.Graph.Inductive.Query.DFS (topsort', scc, components, )
import Data.Graph.Inductive.Tree (Gr, )
import qualified Data.Graph.Inductive.Graph as Graph

import Control.Arrow ((***))
import qualified Control.Monad.Exception.Synchronous as Exc
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.Set as Set
import Control.Monad (guard, when, )
import Data.Maybe (fromMaybe, mapMaybe, )


main :: IO ()
main =
   Exc.resolveT handleException $ do
      argv <- Trans.lift Env.getArgs
      let (opts, cabalPaths, errors) =
             getOpt RequireOrder options argv
      when (not (null errors)) $ Exc.throwT $ concat $ errors
      flags <-
         Exc.ExceptionalT $ return $
            foldr (flip (>>=))
               (return $
                Flags {optHelp = False,
                       optVerbosity = Verbosity.silent,
                       optInfo = location,
                       optParallel = False,
                       optMakefile = False,
                       optBuilddir = ".",
                       optInstall = "cabal install"})
               opts
      when (optHelp flags)
         (Trans.lift $
          Env.getProgName >>= \programName ->
          putStrLn
             (usageInfo ("Usage: " ++ programName ++
                         " [OPTIONS] CABAL-FILES ...") options) >>
          exitSuccess)

      sortCabalFiles flags cabalPaths

handleException :: String -> IO ()
handleException msg = do
   putStrLn $ "Aborted: " ++ msg
   exitFailure


data Flags =
   Flags {
      optHelp :: Bool,
      optVerbosity :: Verbosity.Verbosity,
      optInfo :: SourcePackage -> String,
      optParallel :: Bool,
      optMakefile :: Bool,
      optBuilddir :: FilePath,
      optInstall  :: String
   }

options :: [OptDescr (Flags -> Exc.Exceptional String Flags)]
options =
   Option ['h'] ["help"]
      (NoArg (\flags -> return $ flags{optHelp = True}))
      "show options" :
   Option ['v'] ["verbose"]
      (ReqArg
         (\str flags ->
            fmap (\n -> flags{optVerbosity = n}) $
            Exc.fromEither $
            ReadE.runReadE Verbosity.flagToVerbosity str)
         "N")
      "verbosity level: 0..3" :
   Option [] ["info"]
      (ReqArg
         (\str flags ->
            fmap (\select -> flags{optInfo = select}) $
            case str of
               "name" -> Exc.Success
                            (getPkgName . pkgName . package .
                             packageDescription . description)
               "path" -> Exc.Success location
               "dir"  -> Exc.Success (FilePath.takeDirectory . location)
               _ ->
                  Exc.Exception $
                  "unknown info type " ++ str)
         "KIND")
      "kind of output: name, path, dir" :
   Option ['p'] ["parallel"]
      (NoArg (\flags -> return $ flags{optParallel = True}))
      "Display independently buildable groups of packages" :
   Option ['m'] ["makefile"]
      (NoArg (\flags -> return $ flags{optMakefile = True}))
      "Generate a makefile of package dependencies" :
   Option [] ["builddir"]
      (ReqArg
         (\str flags ->
            fmap (\dir -> flags{optBuilddir = dir}) (Exc.Success str))
         "PATH")
      "Specify the build dir to use for generated makefile" :
   Option [] ["install-cmd"]
      (ReqArg
         (\str flags ->
            fmap (\cmd -> flags{optInstall = cmd}) (Exc.Success str))
         "CMD")
      "Specify the install command to use in generated makefile" :
   []



data SourcePackage =
   SourcePackage {
      location :: FilePath,
      description :: GenericPackageDescription
   }
   deriving (Show, Eq)

sortCabalFiles :: Flags -> [FilePath] -> Exc.ExceptionalT String IO ()
sortCabalFiles flags cabalPaths =
   do pkgDescs <-
         Trans.lift $
         mapM (readPackageDescription (optVerbosity flags)) cabalPaths
      when (optVerbosity flags >= Verbosity.verbose) $
         Trans.lift $
         flip mapM_ pkgDescs $ \pkgDesc -> do
            putStrLn
               ((getPkgName . pkgName . package . packageDescription $ pkgDesc) ++ ":")
            let deps =
                   Set.toAscList $ Set.fromList $
                   map (getPkgName . depName) $
                   allDependencies pkgDesc
            flip mapM_ deps $ \dep ->
               putStrLn $ "  " ++ dep
      let pkgs = zipWith SourcePackage cabalPaths pkgDescs
          graph = getBuildGraph pkgs
      checkForCycles graph
      Trans.lift $
         if optMakefile flags
           then printMakefile flags $ getDeps graph
           else if optParallel flags
              then
                 mapM_ (putStrLn . unwords . map (optInfo flags)) $
                 map (topsort' . subgraph graph) $
                 components graph
              else
                 mapM_ (putStrLn . optInfo flags) $ topsort' graph


printMakefile :: Flags -> [(SourcePackage, [SourcePackage])] -> IO ()
printMakefile flags deps = do
    let printDep (l, ls) = putStrLn (l ++ ": " ++ unwords ls)
        stamp =
           (optBuilddir flags </>) .
           flip FilePath.replaceExtension "cstamp" . location
        allDeps = unwords (map (stamp . fst) deps)
    putStrLn (optBuilddir flags </> "%.cstamp:")
    putStrLn ("\t" ++ optInstall flags ++ " `dirname $*`")
    putStrLn "\tmkdir -p `dirname $@`"
    putStrLn "\ttouch $@"
    putStrLn ""
    putStrLn ("all: " ++ allDeps)
    putStrLn ""
    putStrLn "clean:"
    putStrLn ("\t$(RM) " ++ allDeps)
    putStrLn ""
    mapM_ (printDep . (stamp *** map stamp)) deps

getDeps :: Gr SourcePackage () -> [(SourcePackage, [SourcePackage])]
getDeps gr =
    let c2dep :: Graph.Context SourcePackage () -> (SourcePackage, [SourcePackage])
        c2dep ctx =
           (Graph.lab' ctx,
            map (Graph.lab' . Graph.context gr) (Graph.pre gr . Graph.node' $ ctx))
    in  Graph.ufold (\ctx ds -> c2dep ctx : ds) [] gr

getBuildGraph ::
   [SourcePackage] ->
   Gr SourcePackage ()
getBuildGraph srcPkgs =
   let nodes = zip [0..] srcPkgs
       nodeDict =
          zip
             (map (pkgName . package . packageDescription . description)
                  srcPkgs)
             [0..]
       edges = do
          (srcNode,desc) <- nodes
          dstNode <-
             mapMaybe
                (flip lookup nodeDict . depName)
                (allDependencies $ description desc)
          guard (dstNode /= srcNode)
          return (dstNode, srcNode, ())
   in  Graph.mkGraph nodes edges


checkForCycles ::
   Monad m =>
   Gr SourcePackage () ->
   Exc.ExceptionalT String m ()
checkForCycles graph =
   case getCycles graph of
      [] -> return ()
      cycles ->
         Exc.throwT $ unlines $
         "Cycles in dependencies:" :
         map (unwords . map location . nodeLabels graph) cycles

nodeLabels :: Gr a b -> [Graph.Node] -> [a]
nodeLabels graph =
   map (fromMaybe (error "node not found in graph") .
        Graph.lab graph)

subgraph :: Gr a b -> [Graph.Node] -> Gr a b
subgraph graph nodes =
   let nodeSet = Set.fromList nodes
       edges = do
           from <- nodes
           (to, lab) <- Graph.lsuc graph from
           guard $ Set.member from nodeSet && Set.member to nodeSet
           return (from,to,lab)
   in  Graph.mkGraph (zip nodes $ nodeLabels graph nodes) edges

getCycles :: Gr a b -> [[Graph.Node]]
getCycles =
   filter (\component -> case component of _:_:_ -> True; _ -> False) .
   scc


allDependencies :: GenericPackageDescription -> [Dependency]
allDependencies pkg =
   P.buildDepends (packageDescription pkg) ++
   maybe [] (concatMap snd . flattenCondTree) (P.condLibrary pkg) ++
   concatMap (concatMap snd . flattenCondTree . snd) (P.condExecutables pkg)

flattenCondTree :: P.CondTree v c a -> [(a,c)]
flattenCondTree tree =
   (P.condTreeData tree, P.condTreeConstraints tree) :
   concatMap
      (\(_, thenBranch, elseBranch) ->
         flattenCondTree thenBranch ++
         maybe [] flattenCondTree elseBranch)
      (P.condTreeComponents tree)

depName :: Dependency -> PackageName
depName (Dependency name _) = name

getPkgName :: PackageName -> String
getPkgName (PackageName name) = name
