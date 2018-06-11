{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Hspec
import TemplateHspec

absolute :: Int -> Int
absolute n = if n < 0 then negate n else n 

myMin :: Int -> Int -> Int
myMin a b = if a < b then a else b


main :: IO ()
main = hspec $ do
  describe "absolute" $ do
    $(they "returns #a when given #b" [|
      absolute a `shouldBe` b
      |] [testCases|  a  |  b  |
                   |  1::Int  |  1::Int  |
                   |  -1::Int |  1::Int  |
                   |  0::Int  |  0::Int  |])
  describe "Testing different functions" $ do
    it "should return minimum" $
      myMin 1 5 `shouldBe` 1
    it "should return minimum 2" $
      myMin 5 1 `shouldBe` 1

    $(they "plus: #aa + #bb = #cc" [|
      aa + bb `shouldBe` cc
      |] [testCases|   aa   |   bb   |   cc         |
                   | 1::Int | 2::Int | 3::Int	      |
                   | 2::Int | 3::Int | (2 + 3)::Int |])

    $(they "read #a reads #b" [|
      read a `shouldBe` b
      |] [testCases|    a      |  b                  |
                   |   "1"     |  1::Int             |
                   | "(2, 3)"  |  (2, 3)::(Int, Int) |])

    $(they "show #a shows #b" [|
      show a `shouldBe` b
      |] [testCases|   a                |         b              |
                   | 42::Int            |         "42"           |
                   | ('4', '2')         |       "('4','2')"      |
                   | ["four", "two"]    |  "[\"four\",\"two\"]"  |])

    $(they "testing single case works too" [|
      a ++ b `shouldBe` c
      |] [testCases| a   | b   | c    |
                   | "a" | "b" | "ab" |])

    $(they "testing empty case does nothing" [|
      1 + 2 `shouldBe` "3"
      |] [testCases||])

    $(they "testing empty case does nothing" [|
      1 + 2 `shouldBe` "3"
      |] [testCases| even | with | vars |])

