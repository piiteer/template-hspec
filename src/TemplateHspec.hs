{-# LANGUAGE TemplateHaskell #-}
module TemplateHspec (they, testCases) where

import Language.Haskell.TH
import TestCaseQQ
import TemplateHspecTest

they desc test (vars, n, c) = [| $(multiTestCase n desc test vars) c |]


