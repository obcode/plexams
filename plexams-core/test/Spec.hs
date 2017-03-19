module Main where

import qualified ImportSpec (spec)
import           Test.Hspec
import qualified TypesSpec  (spec)

main = do
    hspec TypesSpec.spec
    hspec ImportSpec.spec
