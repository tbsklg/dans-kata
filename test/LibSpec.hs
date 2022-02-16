module LibSpec where

import Lib (Tone (..), costs, costs', dist, extract, order, order')
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract tones from raw data" $ do
    extract [] `shouldBe` []
    extract ["EADGHE", "DADF#AD", "DADGAD"] `shouldBe` [[E, A, D, G, H, E], [D, A, D, F', A, D], [D, A, D, G, A, D]]
    extract ["EC#ADF#A", "DADG#AD"] `shouldBe` [[E, C', A, D, F', A], [D, A, D, G', A, D]]

  it "should calculate distance" $ do
    dist C C' `shouldBe` 1
    dist C H `shouldBe` 1
    dist H C `shouldBe` 1
    dist E C `shouldBe` 4
    dist C E `shouldBe` 4
    dist E D `shouldBe` 2
    dist D E `shouldBe` 2
    dist C D `shouldBe` 2
    dist D C `shouldBe` 2

  it "should calculate costs to switch a tuning" $ do
    costs [E, A, D, G, H, E] [E, A, D, G, H, E] `shouldBe` 0
    costs [E, A, D, G, H, E] [D, A, D, G, H, E] `shouldBe` 2
    costs [E, A, D, G, H, E] [D, A, D, G, C, E] `shouldBe` 3
    costs [D, A, D, G, C, E] [E, A, D, G, H, E] `shouldBe` 3

  it "should calculate order based on first tune" $ do
    order [[E, A, D, G, H, E], [D, A, D, F', A, D], [D, A, D, G, A, D]] `shouldBe` [[E, A, D, G, H, E], [D, A, D, G, A, D], [D, A, D, F', A, D]]
    order [[D, A, D, F', A, D], [E, A, D, G, H, E], [C, G, D, G, A, D], [D', B, C, G, B, D], [D, G, D, G, A, D], [F', A, D, G, H, E], [H, G, D, G, A, D], [D, A, C', E, H, E], [C, G, C, G, A, D]]
      `shouldBe` [[D, A, D, F', A, D], [D, G, D, G, A, D], [C, G, D, G, A, D], [H, G, D, G, A, D], [C, G, C, G, A, D], [D', B, C, G, B, D], [E, A, D, G, H, E], [F', A, D, G, H, E], [D, A, C', E, H, E]]

  it "should calculate best order for different first tune" $ do
    order' [[D, A, D, F', A, D], [E, A, D, G, H, E], [C, G, D, G, A, D], [D', B, C, G, B, D], [D, G, D, G, A, D], [F', A, D, G, H, E], [H, G, D, G, A, D], [D, A, C', E, H, E], [C, G, C, G, A, D]]
      `shouldBe` [[F', A, D, G, H, E], [E, A, D, G, H, E], [D, A, C', E, H, E], [D, A, D, F', A, D], [D, G, D, G, A, D], [C, G, D, G, A, D], [H, G, D, G, A, D], [C, G, C, G, A, D], [D', B, C, G, B, D]]

  it "should calculate costs" $ do
    costs' [[F', A, D, G, H, E], [F', A, D, G, H, E]] `shouldBe` 0
    costs' [[D, A, D, F', A, D], [H, G, D, G, A, D], [C, G, D, G, A, D], [D, G, D, G, A, D], [C, G, C, G, A, D], [D', B, C, G, B, D], [F', A, D, G, H, E], [D, A, C', E, H, E], [E, A, D, G, H, E]] `shouldBe` 43
    costs' [[D, A, D, F', A, D], [D, G, D, G, A, D], [C, G, D, G, A, D], [H, G, D, G, A, D], [C, G, C, G, A, D], [D', B, C, G, B, D], [E, A, D, G, H, E], [F', A, D, G, H, E], [D, A, C', E, H, E]] `shouldBe` 33
    costs' [[F', A, D, G, H, E], [E, A, D, G, H, E], [D, A, C', E, H, E], [D, A, D, F', A, D], [D, G, D, G, A, D], [C, G, D, G, A, D], [H, G, D, G, A, D], [C, G, C, G, A, D], [D', B, C, G, B, D]] `shouldBe` 31
