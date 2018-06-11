{-# LANGUAGE TemplateHaskell #-}
module TemplateHspecTest(multiTestCase) where
import Language.Haskell.TH
import Text.Parsec.String(Parser)
import Text.Parsec
import Control.Applicative((<$>), (<*>))

multiTestCase :: Int -> String -> ExpQ -> [String] -> ExpQ
multiTestCase n desc test vars = let names = map (\x -> mkName $ 'a':(show x)) [1..n] in
    let pat = tupP (map varP names) in
    let tests = map (\a -> noBindS [| $(singleTest desc test vars) $(varE a) |]) names in
    lamE [pat] (doE (tests ++ [noBindS [|return ()|]]))

singleTest :: String -> ExpQ -> [String] -> ExpQ
singleTest desc test vars = lamE [tupP (map (varP . mkName) vars)] [| it $(prepareDesc vars desc) $(test) |]

pDesc :: [String] -> Parser [ExpQ]
pDesc vars = do
    res <- fmap stringE normal
    next <- getVar <|> (fmap stringE $ (++) <$> (string "#") <*> normal) <|> fmap stringE normal
    return [res, next]
    where
        getVar = choice $ map try $ map (\s -> string ('#':s) >> notFollowedBy alphaNum >> return [|show $(varE $ mkName s)|]) vars
        normal = try $ many (noneOf "#")

parseDesc :: [String] -> Parser ExpQ
parseDesc vars = do
    res <- manyTill (pDesc vars) eof
    return [| concat $(listE (concat res)) |]

prepareDesc :: [String] -> String -> ExpQ
prepareDesc vars desc = case runParser (parseDesc vars) () "" desc of
      Left err  -> fail $ show err
      Right e   -> e