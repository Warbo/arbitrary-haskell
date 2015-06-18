{-# LANGUAGE FlexibleInstances #-}
module Test.Arbitrary.Cabal (Project(..)
                            , Section(..)
                            , alpha
                            , makeProject
                            ) where

import Data.List
import Data.Maybe
import System.Directory
import Test.QuickCheck

-- | Alphabetic strings
newtype AlphaName = AN String deriving (Show)

upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"
alpha = upper ++ lower

instance Arbitrary AlphaName where
  arbitrary = fmap AN (listOf1 (elements alpha))

-- | A Cabal project
data Project = P { name :: String
                 , version  :: [Int]
                 , headers  :: Section ()
                 , sections :: [Section String]
                 }

instance Show Project where
  show = mkCabalFile

instance Arbitrary Project where
  arbitrary = do
    AN name' <- arbitrary
    version' <- listOf1 arbitrary
    library  <-         arbitrary :: Gen (Maybe (Section  LibraryType))
    required <- listOf1 arbitrary :: Gen        [Section RequiredType]
    optional <- listOf  arbitrary :: Gen        [Section OptionalType]
    AN user   <- arbitrary
    AN desc   <- arbitrary
    AN syn    <- arbitrary
    let (required', optional') = dropDupes required optional
        haveTest = not (null optional')
        sections' = renderSections (maybeToList library) ++
                    renderSections required'             ++
                    renderSections optional'
    return $ P { name     = name'
               , version  = map abs version'
               , sections = sections'
               , headers  = S () [
                   ("cabal-version", if haveTest then ">= 1.8"
                                                 else ">= 1.2")
                 , ("build-type", "Simple")
                 , ("category", "Language")
                 , ("maintainer", user ++ "@example.com")
                 , ("description", desc)
                 , ("synopsis", take 79 syn)
                 , ("license", "GPL")
                 , ("license-file", "LICENSE")
                 ]
               }

data Section t = S t [(String, String)]

instance Show (Section String) where
  show (S t l) = unlines $ t : map (indent . keyVal) l

instance Arbitrary (Section RequiredType) where
  arbitrary = do
    t <- arbitrary
    case t of
     Exec n -> do AN m <- arbitrary
                  return (S t [("main-is", m ++ ".hs")])

instance Arbitrary (Section LibraryType) where
  arbitrary = do t <- arbitrary
                 return (S t [])

instance Arbitrary (Section OptionalType) where
  arbitrary = do t <- arbitrary
                 b <- arbitrary
                 (typ, kvs) <- if b then do AN main <- arbitrary
                                            return ("exitcode-stdio-1.0", [
                                                ("main-is", main ++ ".hs")
                                              ])
                                    else do AN mod  <- arbitrary
                                            initial <- elements upper
                                            return ("detailed-0.9", [
                                                ("test-module", initial:mod)
                                              ])
                 return (S t (("type", typ) : kvs))

data RequiredType = Exec String

instance Show RequiredType where
  show (Exec e) = "executable " ++ e

instance Arbitrary RequiredType where
  arbitrary = do AN n <- arbitrary
                 oneof $ map return [Exec n]

data LibraryType = Lib

instance Show LibraryType where
  show Lib = "library"

instance Arbitrary LibraryType where
  arbitrary = return Lib

data OptionalType = Test String

instance Show OptionalType where
  show (Test t) = "test-suite " ++ t

instance Arbitrary OptionalType where
  arbitrary = do AN n <- arbitrary
                 oneof $ map return [Test n]

keyVal (k, v) = k ++ ": " ++ v

indent = ("  " ++)

-- | Create a Cabal project inside the given directory, returning the path to
--   the project
makeProject :: FilePath -> Project -> IO FilePath
makeProject d p = let path = d ++ "/" ++ name p in do
  createDirectoryIfMissing True path
  writeFile (path ++ ("/" ++ name p ++ ".cabal")) (mkCabalFile p)
  writeFile (path ++ "/LICENSE")  ""
  writeFile (path ++ "/Setup.hs") ""
  return path

mkCabalFile :: Project -> String
mkCabalFile p = unlines $ concat [
    ["name: "    ++ name p,
     "version: " ++ intercalate "." (map show (version p))]
  , case headers p of
         S _ kv -> map keyVal kv
  , map show (sections p)
  ]

-- | Remove any sections with duplicate names (preferring Required, of course!)
dropDupes :: [Section RequiredType] ->
             [Section OptionalType] ->
             ([Section RequiredType], [Section OptionalType])
dropDupes rs os = let (ns, rs') = dropDupesRs [] rs
                  in (rs, dropDupesOs ns os)

dropDupesRs ::  [String] -> [Section RequiredType] ->
               ([String],   [Section RequiredType])
dropDupesRs names [] = (names, [])
dropDupesRs names (r@(S (Exec n) _):rs) = let (names', rs') = dropDupesRs (n:names) rs
                                          in (names', if n `elem` names
                                                         then rs'
                                                         else r:rs')

dropDupesOs :: [String] -> [Section OptionalType] -> [Section OptionalType]
dropDupesOs names [] = []
dropDupesOs names (o@(S (Test n) _):os) = if n `elem` names
                                             then     dropDupesOs    names  os
                                             else o : dropDupesOs (n:names) os

renderSection :: Show a => Section a -> Section String
renderSection (S x kvs) = S (show x) kvs

renderSections :: Show a => [Section a] -> [Section String]
renderSections = map renderSection
