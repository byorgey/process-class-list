{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad (forM_, mzero)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, toLower)
import Data.Csv
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.FilePath (takeBaseName, (-<.>))
import System.Process.Typed (proc, runProcess, shell)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [classList] -> processClassList classList
    _ -> putStrLn "Usage: process-class-list <classlist.xls>"

processClassList :: FilePath -> IO ()
processClassList classListFile = do
  -- Convert .xls to .csv
  let csvFile = classListFile -<.> "csv"
  _ <- runProcess (proc "ssconvert" [classListFile, csvFile])
  -- Chop off first line, which just has sheet title information
  _ <- runProcess (shell $ "tail -n +2 " ++ csvFile ++ " | sponge " ++ csvFile)
  -- Read CSV
  csvData <- BL.readFile csvFile
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, students) -> forM_ formats $ \fmt ->
      uncurry writeFile (fmt (csvFile, students))
  return ()

------------------------------------------------------------
-- Formats

type Format = (FilePath, V.Vector Student) -> (FilePath, String)

formats :: [Format]
formats = [raw, checklist, textNames, alias]

mkFmt :: String -> (V.Vector Student -> String) -> Format
mkFmt ext f (fn, students) = (fn -<.> ext, f students)

raw :: Format
raw = mkFmt "raw" show

checklist :: Format
checklist =
  mkFmt "checklist" $
    unlines . map checklistItem . sortBy (comparing fname <> comparing lname) . V.toList
 where
  checklistItem s = "{{[[TODO]]}} [[person/" ++ fname s ++ " " ++ lname s ++ "]]"

textNames :: Format
textNames =
  mkFmt "txt" $
    unlines . map name . sortBy (comparing fname <> comparing lname) . V.toList
 where
  name s = fname s ++ " " ++ lname s

alias :: Format
alias (fn, V.toList -> students) = (fn -<.> "alias", unlines (aliases ++ [classAlias]))
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
