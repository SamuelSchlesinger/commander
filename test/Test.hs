{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Test.Hspec
import Control.Monad.IO.Class
import Control.Monad.Commander
import Control.Applicative

main :: IO ()
main = hspec $ do
  describe "Control.Monad.Commander" do
    describe "runCommanderT" do
      it "runs Victory to Just" do
        runCommanderT (Victory ()) () `shouldReturn` Just ()
      it "runs Defeat to Nothing" do
        runCommanderT Defeat () `shouldReturn` Nothing @()
      it "runs pure to Just" do
        runCommanderT (pure ()) () `shouldReturn` Just ()
      it "runs a stateful action to completion" do
        let action = Action \n -> pure (if n > 0 then action else pure (), n - 1)
        runCommanderT action 100 `shouldReturn` Just ()
      it "backtracks successfully" do
        runCommanderT (Defeat <|> pure 10) () `shouldReturn` Just 10
        runCommanderT (Defeat <|> (Action \() -> pure (Defeat, ()))) () `shouldReturn` Nothing @()
        runCommanderT (Defeat <|> (Action \() -> pure (Defeat, ())) <|> pure 10) () `shouldReturn` Just 10
    it "has a reasonable functor instance" do
      runCommanderT (fmap (+ 1) (pure 10)) () `shouldReturn` Just 11
      x <- runCommanderT (fmap (+ 1) $ fmap (+ 2) (pure 10)) ()
      y <- runCommanderT (fmap ((+ 1) . (+ 2)) (pure 10)) ()
      x `shouldBe` y
    it "has a reasonable applicative instance" do
      runCommanderT (pure (+ 10) <*> pure 10) () `shouldReturn` Just 20
    it "has a reasonable monad instance" do
      runCommanderT (do { x <- pure 10; y <- pure 11; pure (x + y) }) () `shouldReturn` Just 21
