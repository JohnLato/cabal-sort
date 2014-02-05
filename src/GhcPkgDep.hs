module Main where

import qualified System.Process as Proc
import qualified System.IO as IO

import System.Console.GetOpt
          (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Exit (exitWith, ExitCode(..), )
import qualified System.Environment as Env

import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ReadE as ReadE
import qualified Distribution.Package as Pkg
import qualified Distribution.Text as DistText

import qualified Control.Monad.Exception.Synchronous as Exc
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Class as Trans
-- import Control.Monad.IO.Class (liftIO, )

import qualified Data.Set as Set

import qualified Data.ByteString.Char8 as B

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Char as Char
import Control.Monad (when, guard, )
import Data.Maybe (fromMaybe, )


main :: IO ()
main =
   Exc.resolveT (\e -> IO.hPutStr IO.stderr $ "Aborted: " ++ e ++ "\n") $ do
      argv <- Trans.lift Env.getArgs
      let (opts, pgkNames, errors) =
             getOpt RequireOrder options argv
      when (not (null errors)) $ Exc.throwT $ concat $ errors
      flags <-
         Exc.ExceptionalT $ return $
            foldr (flip (>>=)) (return defltFlags) opts
      when (optHelp flags)
         (Trans.lift $
          Env.getProgName >>= \programName ->
          putStrLn
             (usageInfo ("Usage: " ++ programName ++
                         " [OPTIONS] PKG-NAMES ...") options) >>
          exitWith ExitSuccess)

      mapM_ (Trans.lift . putStrLn) =<<
         (Exc.mapExceptionalT (flip State.evalStateT Set.empty) $
          fmap concat $ mapM (getAllDependencies flags True) pgkNames)


data Flags =
   Flags {
      optHelp :: Bool,
      optVerbosity :: Verbosity.Verbosity,
      optUser, optGlobal :: Bool,
      optShowVersions :: Bool,
      optPkgCmd :: String
   }

defltFlags :: Flags
defltFlags =
   Flags {
      optHelp = False,
      optVerbosity = Verbosity.silent,
      optUser = False,
      optGlobal = False,
      optShowVersions = False,
      optPkgCmd = "ghc-pkg"
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
   Option [] ["user"]
      (NoArg (\flags -> return $ flags{optUser = True}))
      "query GHC's local user package database" :
   Option [] ["global"]
      (NoArg (\flags -> return $ flags{optGlobal = True}))
      "query GHC's global package database" :
   Option [] ["show-versions"]
      (NoArg (\flags -> return $ flags{optShowVersions = True}))
      "show package version numbers in the output" :
   Option [] ["pkg-cmd"]
      (ReqArg
         (\str flags -> return flags{optPkgCmd = str})
         "COMMAND")
      ("command for querying the package database (default: " ++
       optPkgCmd defltFlags ++ ")") :
   []


type PkgName = String


getAllDependencies ::
   Flags -> Bool -> PkgName ->
   Exc.ExceptionalT String (State.StateT (Set.Set PkgName) IO) [PkgName]
getAllDependencies flags userSuppliedName name = do
   when (optVerbosity flags >= Verbosity.deafening)
      (Trans.lift $
         Trans.lift . print =<< State.get)
   b <- Trans.lift $ State.gets (Set.member name)
   if b
     then return []
     else
       Exc.catchT
          (do {- register this name _before_ calling ghc-pkg,
                 because ghc-pkg may not find the package,
                 and then we do not need to query again -}
              Trans.lift $ State.modify (Set.insert name)
              deps <-
                 Exc.mapExceptionalT Trans.lift $
                 getDirectDependencies flags name
              allDeps <- mapM (getAllDependencies flags False) deps
              let strippedName = fromMaybe name $ do
                     guard $ not $ optShowVersions flags
                     Pkg.PackageName pname <-
                        fmap Pkg.pkgName $ DistText.simpleParse name
                     return pname
              return $ concat allDeps ++ [strippedName])
          (\errTxt ->
             if not userSuppliedName &&
                B.isInfixOf (B.pack "cannot find") (B.pack errTxt)
               then return []
               else Exc.throwT errTxt)


getDirectDependencies ::
   Flags -> PkgName ->
   Exc.ExceptionalT String IO [PkgName]
getDirectDependencies flags name = do
   let cmd = optPkgCmd flags
       args =
          (if optUser   flags then ("--user"   :) else id) $
          (if optGlobal flags then ("--global" :) else id) $
          ["field", name, "depends"]
   when (optVerbosity flags >= Verbosity.verbose)
      (Trans.lift $ putStrLn $ unwords $ cmd : args)
   (inp,out,err,pid) <-
      Trans.lift (Proc.runInteractiveProcess cmd args Nothing Nothing)
   txt    <- Trans.lift $ B.hGetContents out
   errTxt <- Trans.lift $ B.hGetContents err
   when (optVerbosity flags >= Verbosity.normal) $
      Trans.lift $ putStr $ B.unpack errTxt
   Exc.mapExceptionT (\n ->
      "ghc-pkg exited with code " ++ show n ++ "\n" ++ B.unpack errTxt) $
         Exc.fromExitCodeT $
         Proc.waitForProcess pid
   Trans.lift (mapM_ IO.hClose [inp,out,err])
   case words (B.unpack txt) of
      "depends:":names ->
         {-
         If multiple packages are installed,
         then the dependencies of each are preceded by a 'depends:' prefix.

         ghc-pkg lists 'builtin_rts' as dependency of 'base',
         but does not accept this identifier as package name,
         since it contains an underscore.
         -}
         return $
            map stripHashId $
            filter (not . flip elem ["depends:", "builtin_rts"]) names
      _ -> Exc.throwT $
              "unexpected output of ghc-pkg - " ++
              "it should start with 'depends:'" ++
              if optVerbosity flags >= Verbosity.verbose
                then "\n" ++ B.unpack txt
                else ""

stripHashId :: PkgName -> PkgName
stripHashId name =
   (\parts ->
      let lastPart = last parts
      in  if (not $ null $ drop 15 lastPart) &&
             all Char.isHexDigit lastPart
            then List.intercalate "-" $ init parts
            else name)
   .
   ListHT.chop ('-'==)
   $
   name
