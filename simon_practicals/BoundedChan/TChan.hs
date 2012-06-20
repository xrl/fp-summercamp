--module BoundedChan
--where
import Prelude hiding (reads)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar

data TChan a = TChan (TVar [a]) (TVar [a])

main :: IO ()
main = do
  join $ atomically $ task
  return ()

task :: STM (IO ())
task = do
  newChan <- emptyTChan
  push newChan "1"
  push newChan "2"
  val <- pop newChan
  push newChan "3"
  push newChan "4"
  pop newChan
  pop newChan
  pop newChan
  let printIt = putStrLn $ val
  return (printIt)

emptyTChan :: STM (TChan a)
emptyTChan = do
  readq  <- newTVar []
  writeq <- newTVar []
  return (TChan readq writeq)

push :: TChan a -> a -> STM (TChan a)
push (TChan readq writeq) val = do
  writes <- readTVar writeq
  _ <- writeTVar writeq (val:writes)
  return $ TChan readq writeq

pop :: TChan a -> STM a
pop (TChan readqvar writeqvar) = do
  reads <- readTVar readqvar
  case length reads of
    0 -> do
      writes <- readTVar writeqvar
      case length writes of
        0 -> retry
        otherwise -> do
          let (val:rest) = writes
          writeTVar writeqvar []
          writeTVar readqvar rest
          return val
    otherwise -> do
      let val:rest = reads
      writeTVar readqvar rest
      return val
