module Main where

import           Lib
import           Network.Wai.Handler.Warp

main :: IO ()
main = startApp

startApp :: IO ()
startApp = run 8080 app

