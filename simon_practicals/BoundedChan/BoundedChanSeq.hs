--module BoundedChan
--where
import Prelude hiding (length)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import qualified Data.Sequence as Seq

type MaxSize = Integer
type ReadQueue  a = Seq.Seq a
type WriteQueue a = Seq.Seq a

data TChan a = TChan (TVar (ReadQueue a)) (TVar (WriteQueue a))

main :: IO ()
main = do
  join $ atomically $ task
  return ()

task :: STM (IO ())
task = do
  newChan <- emptyTChan
  push newChan "asdf"
  push newChan "fdsa"
  val <- pop newChan
  push newChan "what"
  push newChan "ohhh"
  pop newChan
  pop newChan
  pop newChan
  pop newChan
  let printIt = putStrLn $ val
  return (printIt)

emptyTChan :: STM (TChan a)
emptyTChan = do
  readq  <- newTVar (Seq.empty)
  writeq <- newTVar (Seq.empty)
  return (TChan readq writeq)

push :: TChan a -> a -> STM (TChan a)
push (TChan readq writeq) val = do
  writeseq <- readTVar writeq
  _ <- writeTVar writeq (writeseq Seq.|> val)
  return $ TChan readq writeq

pop :: TChan a -> STM a
pop (TChan readqvar writeqvar) = do
  readseq <- readTVar readqvar
  case Seq.length readseq of
    0 -> do
      writeseq <- readTVar writeqvar
      case Seq.length writeseq of
        0 -> retry
        otherwise -> do
          let (val,rest) = Seq.splitAt 1 writeseq
          writeTVar writeqvar Seq.empty
          writeTVar readqvar rest
          return (Seq.index val 0)
    otherwise -> do
      let (val,rest) = Seq.splitAt 1 readseq
      writeTVar readqvar rest
      return (Seq.index val 0)
