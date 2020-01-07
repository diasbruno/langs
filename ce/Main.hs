module Main where

import Language.Javascript.ES1.Parser
import Language.Javascript.Types
import Control.Monad
import Data.IORef
import System.Console.ANSI
import System.IO

data TermPos =
  TermPos !Int !Int !Int !Int
  deriving Show

type TermBuffer =
  String

data Term =
  Term { cursor :: TermPos
       , buffer :: TermBuffer }
  deriving Show

data Source a =
  Source String a
  deriving Show

data Buffer a =
  Buffer Term !(Source a)
  deriving Show

type BufferRef a =
  IORef (Buffer a)

processCommand :: Char -> TermPos -> IO TermPos
processCommand 'h' tp =
  moveForwardCommand tp
processCommand 'j' tp =
  moveUpCommand tp
processCommand 'k' tp =
  moveDownCommand tp
processCommand 'l' tp =
  moveBackwardCommand tp
processCommand  _  _ =
  showCursor >> error "quit"

withTerm :: Term -> (TermPos -> IO TermPos) -> IO TermPos
withTerm (Term p _) f = hideCursor >> (f p >>= \x -> showCursor >> return x)

withBuffer :: BufferRef a -> (Buffer a -> IO (Buffer a)) -> IO (BufferRef a)
withBuffer buf io =
  readIORef buf >>= io >>=
  \buf' -> writeIORef buf buf' >> return buf

(&>) :: BufferRef a -> (Buffer a -> IO (Buffer a)) -> IO (BufferRef a)
(&>) = withBuffer

redrawAll :: String -> Int -> Int -> IO ()
redrawAll b w h =
  let ls = lines b
      missing = map (const (replicate w ' ')) [0..(h - length ls - 1)]
      all' = ls ++ missing
  in setCursorPosition 0 0 >>
     mapM_ (\(i,l) ->
              setCursorPosition i 0 >>
              putStr (l ++ replicate (w - length l) ' ')) (zip [0..h] all')

run :: BufferRef a -> IO (BufferRef a)
run buf = do
  c <- getChar
  hSetBuffering stdin (BlockBuffering Nothing)
  buf &> (\(Buffer p s) -> do
             let (TermPos _ _ w h) = cursor p
             redrawAll (buffer p) w h >>
               withTerm p (processCommand c) >>=
               \p'@(TermPos x y _ _) ->
                 hSetBuffering stdin NoBuffering >>
                 setCursorPosition x y >>
                 return (Buffer (Term p' (buffer p)) s))

parseFile :: String -> IO [Statement]
parseFile = return . parse

makeTermBuffer :: (Num b, Enum b) => Int -> b -> String
makeTermBuffer width height =
  mconcat $ map (const (replicate width ' ')) [0..height]

initScreen :: IO TermPos
initScreen =
  clearScreen >>
  setCursorPosition 0 0 >>
  getTerminalSize >>= \(Just (w, h)) ->
                        return (TermPos 0 0 h w)

moveForwardCommand
  , moveUpCommand
  , moveDownCommand
  , moveBackwardCommand
    :: TermPos -> IO TermPos

moveForwardCommand (TermPos x y w h) =
  let y' = max 0 (y - 1)
  in setCursorPosition x y' >> return (TermPos x y' w h)

moveUpCommand (TermPos x y w h) =
  let x' = min (max 0 (x - 1)) w
  in setCursorPosition x' y >> return (TermPos x' y w h)

moveDownCommand (TermPos x y w h) =
  let x' = min (x + 1) w
  in setCursorPosition x' y >> return (TermPos x' y w h)

moveBackwardCommand (TermPos x y w h) =
  let y' = min h (y + 1)
  in setCursorPosition x y' >> return (TermPos x y' w h)

main :: IO ()
main = do
  pos <- initScreen
  withFile sourceName ReadWriteMode $
    hGetContents >=>
      \c -> parseFile c >>= \s -> do
        pref <- newIORef (Buffer (Term pos c) (Source sourceName s))
        putStrLn c >> setCursorPosition 0 0 >>
          forever (run pref)
  where sourceName = "./test.t"
