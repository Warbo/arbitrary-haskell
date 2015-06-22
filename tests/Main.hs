module Main where

import Control.Exception (try, SomeException)
import Data.List
import System.Directory
import System.Exit
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.Arbitrary.Haskell
import Test.Arbitrary.Cabal
import Test.Arbitrary.Internal.Cabal
import Text.Regex.Posix

-- Haskell tests

moduleIsMain (H s) = s =~ "^module Main\\s" :: Bool

hsTests = testGroup "Haskell tests" [
            testProperty "Only Main modules are generated" moduleIsMain
          ]

-- Cabal tests

cabalDirExists p = monadicIO $ cleanup $ do
  dir <- run (makeProject tmpDir p)
  exists <- run (doesDirectoryExist dir)
  assert exists

cabalFileExists p = monadicIO $ cleanup $ do
  dir <- run (makeProject tmpDir p)
  exists <- run (doesFileExist (dir ++ "/" ++ name p ++ ".cabal"))
  assert exists

cabalProjectValid p = monadicIO $ cleanup $ do
  dir <- run (makeProject tmpDir p)
  result <- run (cabal dir ["check"])
  assert (result == ExitSuccess)

requiredSectionsUnique rs = length unique == length rs'
  where (rs', _) = dropDupes (rs++rs) []
        unique   = nub (map getName (rs++rs))

optionalSectionsUnique os = length unique == length os'
  where (_, os') = dropDupes [] (os++os)
        unique   = nub (map getName (os++os))

dedupeDecreasesLength rs os = length rs' + length os' <= length rs + length os
  where (rs', os') = dropDupes (rs++rs) (os++os)

dedupedSectionsUnique rs os = nameCount == sectionCount
  where (rs', os')   = dropDupes (rs++rs) (os++os)
        nameCount    = length (nub (map getName rs' ++ map getName os'))
        sectionCount = length rs' + length os'

cabalSectionsUnique p = length (sections p) == length unique
  where unique = nub (map getName (sections p))

cabalFilesAreWritten p = monadicIO $ cleanup $ do
  dir <- run (makeProject tmpDir p)
  found <- run $ filesIn dir
  run (print found)
  assert (files p `allIn` found)
  where allIn []                ys = True
        allIn (((ds, f), _):xs) ys = intercalate "/" ([""] ++ ds ++ [f]) `elem` ys &&
                                     xs `allIn` ys
        filesIn dir = do out <- readProcess "find" [dir] ""
                         return . map (drop (length dir)) . lines $ out

cabalTests = testGroup "Cabal tests" [
    testProperty "Cabal dir exists"  cabalDirExists
  , testProperty "Cabal file exists" cabalFileExists
  , testProperty "Cabal project is valid" cabalProjectValid
  , testProperty "Deduping decreases length" dedupeDecreasesLength
  , testProperty "Deduped Required sections are unique" requiredSectionsUnique
  , testProperty "Deduped Optional sections are unique" optionalSectionsUnique
  , testProperty "Section names are unique" dedupedSectionsUnique
  , testProperty "Cabal section names are unique" cabalSectionsUnique
  , testProperty "Files are written" cabalFilesAreWritten
  ]

-- Helpers

tmpDir = "/tmp/ArbitraryHaskellTest"

rmTmpDir :: IO (Either SomeException ())
rmTmpDir = try (removeDirectoryRecursive tmpDir)

cleanup :: PropertyM IO a -> PropertyM IO a
cleanup x = do run $ rmTmpDir
               result <- x
               run $ rmTmpDir
               return result

sanitise :: FilePath -> FilePath
sanitise = filter (`elem` alpha)

cabal :: FilePath -> [String] -> IO ExitCode
cabal d args = do (sin, sout, serr, p) <- createProcess (proc "cabal" args) {
                                            cwd = Just d
                                          }
                  waitForProcess p

cleanupInvariant = monadicIO $ do
  cleanup (return ())
  exists <- run (doesDirectoryExist tmpDir)
  assert (not exists)

cleanupCleansUp x = monadicIO $ do
  run $ createDirectoryIfMissing True (tmpDir ++ "/" ++ sanitise x)
  existsPre <- run (doesDirectoryExist tmpDir)
  assert existsPre
  cleanup (return ())
  existsPost <- run (doesDirectoryExist tmpDir)
  assert (not existsPost)

helperTests = testGroup "Helper tests" [
    testProperty (tmpDir ++ " wiped by cleanup") cleanupInvariant
  , testProperty "Cleanup cleans up" cleanupCleansUp
  ]

main = defaultMain $ testGroup "All tests" [
         hsTests
       , cabalTests
       , helperTests
       ]
