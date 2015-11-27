module Qy.Random where

import System.Random
import Data.Char (isAlphaNum)
import qualified Data.Text as T

randomText :: IO T.Text
randomText = do
    gen <- newStdGen
    let s = T.pack . take 128 $ filter isAlphaNum $ randomRs ('A', 'z') gen
    return s
