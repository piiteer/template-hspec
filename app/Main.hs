{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Hspec
import TemplateHspec
import Language.Haskell.TH

myMin :: Int -> Int -> Int
myMin a b = if a < b then a else b

main :: IO ()
main = hspec $
  describe "Testing min function" $ do
    it "should return minimum" $
      myMin 1 5 `shouldBe` 1
    it "should return minimum 2" $
      myMin 5 1 `shouldBe` 1

    $(they "test plusa #aa + #bb = #cc"
      [| aa + bb `shouldBe` cc |]
      ["aa", "bb", "cc"]) $
        [(1, 2, 3), (2, 3, 5)]

