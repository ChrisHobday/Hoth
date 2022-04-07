{-# LANGUAGE TemplateHaskell #-}
module Main where

import Executor
import User

import Language.Haskell.TH
import System.Mem.StableName

main :: IO ()
main = do
  print 'name
  stableName <- makeStableName name
  print $ hashStableName stableName
