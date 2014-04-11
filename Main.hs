module Main where

import System.Console.GetOpt
import System.Environment

import qualified Options as O
import qualified GPSDClient as G

main :: IO ()
main = 
  do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute O.optionDescriptions args

    -- Here we thread defaultOptions through all supplied option actions
    opts <- foldl (>>=) (return O.defaultOptions) actions

    time_str <- G.processGPSDStream opts

    let formatted_time_str = takeWhile ( /= '.' ) time_str
    putStrLn formatted_time_str

