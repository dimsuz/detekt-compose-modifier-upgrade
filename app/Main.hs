module Main where

import qualified MyLib (migrate)

main :: IO ()
main = do MyLib.migrate
