{-
http://leiffrenzel.de/papers/commandline-options-in-haskell.html
import Control.Monad
-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where
import Data.List
import Data.Char
import Data.Maybe( fromMaybe )
import Prelude.Unicode
import System.Console.GetOpt
import System.Environment(getArgs,getProgName)
import System.Exit
import System.Directory
import System.IO
import Text.SOML
import UI.NCurses
import Lib

toAlg (x,y) = [chr (x + 96), chr (y + 48)]

main = do  
    putStrLn =<< getProgName
    cfg ← getDataSOML "./data-soml/config.soml"
    case cfg of
       (Right cs) → do
           let dbs = lookup "Database" cs
           case dbs of
                (Just se) → putStrLn $ show se
                Nothing → error "Секция Database не найдена в /data-soml/config.soml"
       (Left ss) → putStrLn $ show ss
    args ← getArgs
    let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
    opts ← foldl (>>=) (return defaultOptions) actions
    let Options { optInput = input,
                optOutput = output } = opts
    input >>= output

 {-   case getOpt RequireOrder options args of
      (flags, [],      [])     → print $ length flags
      (_,     nonOpts, [])     → error $ "unrecognized arguments: " ++ unwords nonOpts
      (_,     _,       msgs)   → error $ concat msgs ++ usageInfo header options

  runCurses $ do
    setEcho False
    w ← defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev → ev == EventCharacter 'q' || ev == EventCharacter 'Q')
-}

waitFor :: Window → (Event → Bool) → Curses ()
waitFor w p = loop where
    loop = do
        ev ← getEvent w Nothing
        case ev of
            Nothing → loop
            Just ev' → if p ev' then return () else loop

data Options = Options  {
    optInput  :: IO String,
    optOutput :: String → IO ()
  }


defaultOptions :: Options
defaultOptions = Options {
    optInput  = getContents,
    optOutput = putStr
  }

options :: [OptDescr (Options → IO Options)]
options = [
    Option ['h'] ["help"        ] (NoArg showHelp           )       "Справка",
    Option ['v'] ["version"     ] (NoArg showVersion        )       "show version number",
    Option ['i'] ["input"       ] (ReqArg readInput "FILE"  )       "some option that requires a file argument",
    Option ['o'] ["output"]  (ReqArg writeOutput "FILE") "some option with an optional file argument"
  ]

showHelp, showVersion  :: Options → IO Options
showHelp _ = do
  putStrLn "Справка 1"
  exitWith ExitSuccess

showVersion _ = do
  putStrLn "v0.1"
  exitWith ExitSuccess


readInput arg opt = return opt { optInput = readFile arg }
writeOutput arg opt = return opt { optOutput = writeFile arg }