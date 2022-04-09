{-# LANGUAGE TemplateHaskell #-}
module User
  where

import Executor

import Language.Haskell.TH

data User = User
  { name        :: String 
  , permissions :: [String]
  } deriving ( Show )

instance Executor User where
  allowed user f = "bob" `elem` permissions user
  as user f a    = if allowed user f
                     then f a
                     else a

rename :: String -> User -> User
rename newName user = user { name = newName }

-- functionName f = 'f