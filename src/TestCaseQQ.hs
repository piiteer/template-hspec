{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TestCaseQQ(testCases) where
import Language.Haskell.TH
import Language.Haskell.Meta.Parse(parseExp)
import Language.Haskell.TH.Quote
import TemplateHspecTest
import qualified Data.Text as T

getLines :: String -> [T.Text]
getLines = (filter (not . T.null)) . (map T.strip) . T.lines . T.pack

splitCols :: T.Text -> [T.Text]
splitCols = (filter (not . T.null)) . (map T.strip) . (T.splitOn "|")

getArgs :: String -> ExpQ
getArgs table = case getLines table of
  [] -> [|([], 0, ())|]
  names:vals -> do
    let nameList = map (stringE . T.unpack) (splitCols names)
    let argList = map getArg (map splitCols vals)
    let argCount = litE . integerL . toInteger $ length argList
    tupE [listE nameList, argCount, tupE argList]

getArg :: [T.Text] -> ExpQ
getArg line = do
    let args = map parseArg line
    tupE args

parseArg :: T.Text -> ExpQ
parseArg arg = case parseExp $ T.unpack arg of
    Left err  -> fail $ show err
    Right e   -> return e

testCases :: QuasiQuoter
testCases = QuasiQuoter getArgs undefined undefined undefined
