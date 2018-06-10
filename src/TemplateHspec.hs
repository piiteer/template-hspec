{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module TemplateHspec (they, testNames) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax(lift)
import Test.Hspec
import Text.Parsec.String(Parser)
import Text.Parsec
import Control.Applicative((<$>), (<*>))


they :: String -> ExpQ -> [String] -> ExpQ
they desc test vars = [| mapM_ $(singleTest desc test vars) |]

singleTest :: String -> ExpQ -> [String] -> ExpQ
singleTest desc test vars = lamE [tupP (map (varP . mkName) vars)] [| it $(prepareDesc vars desc) $(test) |]

testNames :: [String] -> [Name]
testNames = map mkName


pDesc :: [String] -> Parser ExpQ
pDesc vars = do
    res <- fmap stringE normal
    next <- getVar <|> (fmap stringE $ (++) <$> (string "#") <*> normal)
    return [| $(res) ++ $(next)|]
    where
        getVar = choice $ map try $ map (\s -> string ('#':s) >> notFollowedBy alphaNum >> return [|show $(varE $ mkName s)|]) vars
        normal = try $ many (noneOf "#")

parseDesc :: [String] -> Parser ExpQ
parseDesc vars = do
    res <- manyTill (pDesc vars) eof
    return [| concat $(listE res) |]

prepareDesc :: [String] -> String -> ExpQ
prepareDesc vars desc = case runParser (parseDesc vars) () "" desc of
      Left err  -> fail $ show err
      Right e   -> e
