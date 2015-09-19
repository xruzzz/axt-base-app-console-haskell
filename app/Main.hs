{-# LANGUAGE UnicodeSyntax #-}
module Main where
import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char
import UI.NCurses
import Lib
-- import Prelude.Unicode
{-
import Control.Monad
import Text.TOML
-}

toAlg (x,y) = [chr (x + 96), chr (y + 48)]

ap3 :: (a->a)->a->a
ap3 f x = f(f (f x)) 

{-
main = do  
    args <- getArgs
    progName <- getProgName
    putStrLn progName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn $ ap3 ('b':) "asdf"
-}
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
