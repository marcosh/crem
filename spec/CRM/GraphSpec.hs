module CRM.GraphSpec where

import "crm" CRM.Graph
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Graph" $ do
    describe "productGraph" $ do
      it "computes correctly the product of two graphs" $
        do
          productGraph
            (Graph [(1 :: Int, 1), (1, 2)])
            (Graph [('a', 'b'), ('c', 'd')])
          `shouldBe` Graph
            [ ((1, 'a'), (1, 'b'))
            , ((1, 'c'), (1, 'd'))
            , ((1, 'a'), (2, 'b'))
            , ((1, 'c'), (2, 'd'))
            ]
