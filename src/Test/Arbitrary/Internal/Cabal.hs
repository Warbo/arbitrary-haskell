{-# LANGUAGE FlexibleInstances #-}
module Test.Arbitrary.Internal.Cabal where

-- Implementation details that we don't want to export to users, but we do want
-- to use in our tests

import Data.List
import Data.Maybe
import System.Directory
import Test.Arbitrary.Haskell
import Test.QuickCheck

-- | Alphabetic strings
newtype AlphaName = AN String deriving (Show)

instance Arbitrary AlphaName where
  arbitrary = fmap AN (listOf1 (elements alpha))

upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"
alpha = upper ++ lower

capitalised :: Gen String
capitalised = do init <- elements upper
                 rest <- listOf (elements alpha)
                 return (init : take 10 rest)

-- | A Cabal project
data Project = P { name :: String
                 , version  :: [Int]
                 , headers  :: Section ()
                 , sections :: [Section String]
                 , files :: [(File, Haskell)]
                 }

instance Show Project where
  show p = mkCabalFile p ++ "\n" ++ show (files p)

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
    files'    <- listOf genHsFile
    let (required', optional') = dropDupes required optional
        -- Cabal rejects tests with the same name as the package
        optional'' = filter (\(S (Test n) _) -> n /= name') optional'
        haveTest   = not (null optional'')
        sections'  = renderSections (maybeToList library) ++
                     renderSections required'             ++
                     renderSections optional''
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
               , files = files'
               }

type File = ([FilePath], FilePath) -- Directories and filename

genFile :: Gen File
genFile = do dirs <- listOf capitalised
             name <- capitalised
             return (dirs, name)

genHsFile :: Gen (File, Haskell)
genHsFile = do code <- arbitrary
               file <- genFile
               return (file, code)

data Section t = S t [(String, String)]

-- If we use Show a => Show (Section a) we'd get extra quote marks for String
instance Show (Section String) where
  show (S t l) = unlines $ t : map (indent . keyVal) l

-- Convert other types to String, then use the above instance
instance Show (Section RequiredType) where
  show (S t l) = show (S (show t) l)

instance Show (Section OptionalType) where
  show (S t l) = show (S (show t) l)

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
  mapM_ (mkFiles path) (files p)
  return path

mkFiles path ((dirs, file), H src) = do
  let dir = intercalate "/" (path:dirs)
  createDirectoryIfMissing True dir
  writeFile (dir ++ "/" ++ file) src

mkCabalFile :: Project -> String
mkCabalFile p = unlines $ concat [
    ["name: "    ++ name p,
     "version: " ++ intercalate "." (map show (version p))]
  , case headers p of
         S _ kv -> map keyVal kv
  , map show (sections p)
  ]

renderSection :: Show a => Section a -> Section String
renderSection (S x kvs) = S (show x) kvs

renderSections :: Show a => [Section a] -> [Section String]
renderSections = map renderSection

-- | Remove any sections with duplicate names (preferring Required, of course!)
dropDupes :: [Section RequiredType] ->
             [Section OptionalType] ->
             ([Section RequiredType], [Section OptionalType])
dropDupes = dropDupes' []

dropDupes' :: [String] -> [Section RequiredType] -> [Section OptionalType] ->
                         ([Section RequiredType],   [Section OptionalType])
dropDupes' names (r:rs) os = let n          = getName r
                                 (rs', os') = dropDupes' (n:names) rs os
                             in  if n `elem` names
                                    then (rs',   os')
                                    else (r:rs', os')
dropDupes' names [] (o:os) = let n        = getName o
                                 (_, os') = dropDupes' (n:names) [] os
                             in  if n `elem` names
                                    then ([], os')
                                    else ([], o:os')
dropDupes' names [] []     = ([], [])

sectionName (S (Exec n) _) = show n

class Named a where
  getName :: a -> String

instance Named String where
  getName = id

instance Named OptionalType where
  getName (Test n) = n

instance Named RequiredType where
  getName (Exec n) = n

instance Named a => Named (Section a) where
  getName (S x _) = getName x
