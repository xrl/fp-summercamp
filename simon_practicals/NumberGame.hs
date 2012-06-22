import System.IO
import Control.Concurrent
import Control.Monad

import Text.Printf

import Async

type StringLimit = Int
data GameState = GameState String StringLimit
data GameEvent = AddCharacter Char
               | RemoveCharacter Char

main :: IO ()
main = do
  setupIO
  game <- newMVar (GameState "" 10)
  addingChars  <- addCharacters game
  showingChars <- showCharacters game
  gameEngine   <- applyUserInput game

  wait gameEngine

  return ()

addCharacters :: (MVar GameState) -> IO (Async ())
addCharacters gameVar = async $ forever $ do
  threadDelay 1000000
  (GameState string limit) <- takeMVar gameVar
  putMVar gameVar (GameState ('q':string) limit)

showCharacters :: (MVar GameState) -> IO (Async ())
showCharacters gameVar = async $ forever $ do
  gm1@(GameState strone _) <- takeMVar gameVar
  putMVar gameVar gm1

  gm2@(GameState strtwo _) <- takeMVar gameVar
  putMVar gameVar gm2

  when (strone /= strtwo)
    (do
      printf "%s" ( replicate (length strone) '\8')
      printf "%s" strtwo
    )
  return ()

applyUserInput :: (MVar GameState) -> IO (Async ())
applyUserInput gameVar = async $ forever $ do
  char <- getChar
  (GameState string limit) <- takeMVar gameVar
  putMVar gameVar (GameState (char:string) limit)

setupIO :: IO ()
setupIO = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False