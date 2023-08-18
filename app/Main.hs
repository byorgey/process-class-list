{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad (forM, forM_, mzero)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, toLower)
import Data.Csv
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (isExtensionOf, takeBaseName, (-<.>), (</>))
import System.Process.Typed (proc, runProcess, shell)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> processClassList dir
    _ -> putStrLn "Usage: process-class-list <DIR>"

processClassList :: FilePath -> IO ()
processClassList classListDir = do
  dirFiles <- listDirectory classListDir
  let classListFiles = filter ((||) <$> isExtensionOf "xls" <*> isExtensionOf "xlsx") dirFiles
      classPrefix = takeWhile (/= '-') (takeBaseName (head classListFiles))
  sections <- forM classListFiles $ \classListFileEnd -> do
    let classListFile = classListDir </> classListFileEnd
        section = drop 1 . dropWhile (/= '-') . takeBaseName $ classListFile
    -- Fix "Biochemistry &" formatting
    _ <- runProcess (shell $ "sed -ie 's/Biochemistry \\& /Biochemistry \\&amp; /g' " ++ classListFile)
    -- Convert .xls to .csv
    let csvFile = classListFile -<.> "csv"
    _ <- runProcess (proc "ssconvert" [classListFile, csvFile])
    -- Chop off first line, which just has sheet title information
    _ <- runProcess (shell $ "tail -n +2 " ++ csvFile ++ " | sponge " ++ csvFile)
    -- Read CSV
    csvData <- BL.readFile csvFile
    case decodeByName csvData of
      Left err -> putStrLn err >> exitFailure
      Right (_, students) -> return (section, V.toList students)

  -- Also create a combined section for the entire class, if there's
  -- more than one section
  let allSections = case sections of
        [sect] -> [sect]
        _ -> combine sections : sections

  -- Output each section in all formats
  forM_ allSections $ \(section, students) ->
    forM_ formats $ \fmt ->
      uncurry writeFile (fmt (classListDir </> withSection section classPrefix, students))

withSection :: String -> String -> String
withSection "" prefix = prefix
withSection s prefix = prefix ++ "-" ++ s

-- Combine all sections into the entire class
combine :: [(String, [Student])] -> (String, [Student])
combine ss = ("", map head . group . sort . concatMap snd $ ss)

------------------------------------------------------------
-- Formats

type Format = (FilePath, [Student]) -> (FilePath, String)

formats :: [Format]
formats = [raw, checklist, textNames, alias]

mkFmt :: String -> ([Student] -> String) -> Format
mkFmt ext f (fn, students) = (fn -<.> ext, f students)

raw :: Format
raw = mkFmt "raw" show

checklist :: Format
checklist =
  mkFmt "checklist"
    $ unlines
    . map checklistItem
    . sortBy (comparing fname <> comparing lname)
 where
  checklistItem s = "{{[[TODO]]}} [[person/" ++ fname s ++ " " ++ lname s ++ "]]"

textNames :: Format
textNames =
  mkFmt "txt"
    $ unlines
    . map name
    . sortBy (comparing fname <> comparing lname)
 where
  name s = fname s ++ " " ++ lname s

alias :: Format
alias (fn, students) = (fn -<.> "alias", unlines (aliases ++ [classAlias]))
 where
  handle s = map toLower (fname s) ++ "." ++ [toLower (head (lname s))]
  mkEmail s = unwords [fname s, lname s, "<" ++ email s ++ ">"]
  mkAlias s = unwords ["alias", handle s, "\"" ++ mkEmail s ++ "\""]
  aliases = map mkAlias students
  classAlias = unwords $ ["alias", "class." ++ takeBaseName fn] ++ map handle students

------------------------------------------------------------
-- Data types

data Year = Freshman | Sophomore | Junior | Senior
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance FromField Year where
  parseField = \case
    "Freshman" -> pure Freshman
    "Sophomore" -> pure Sophomore
    "Junior" -> pure Junior
    "Senior" -> pure Senior
    _ -> mzero

data Student = Student
  { ferpa :: !Bool
  , studentID :: !String
  , fname :: !String
  , lname :: !String
  , email :: !String
  , major :: !String
  , year :: !Year
  }
  deriving (Eq, Ord, Show)

getFName, getLName :: String -> String
getFName = takeWhile (not . isSpace) . drop 2 . dropWhile (/= ',')
getLName = takeWhile (/= ',')

instance FromNamedRecord Student where
  parseNamedRecord r =
    Student
      <$> r
      .: "FERPA Restrict"
      <*> r
      .: "Student ID"
      <*> (getFName <$> (r .: "Student"))
      <*> (getLName <$> (r .: "Student"))
      <*> r
      .: "Email"
      <*> r
      .: "Major"
      <*> r
      .: "Class"

instance FromField Bool where
  parseField = \case
    "Y" -> pure True
    "N" -> pure False
    _ -> mzero
