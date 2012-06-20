--module BoundedChan
--where
import Prelude hiding (length)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Sequence

type MaxSize = Integer
type BChan a = BChan MaxSize (Seq a) 
type TBChan a = TVar (BChan a)

main :: IO ()
main = do
  mychan <- newIO 5
  --let list = [1,2,3]
  --atomically $ do
  --  mapM_ (\num -> add num mychan) list
  return ()

new :: MaxSize -> STM (TBChan a)
new size = newTVar (empty)

newIO :: MaxSize -> IO (TBChan a)
newIO size = atomically $ new size

add :: a -> TBChan a -> STM ()
add entry tbchan = do
  bchan <- readTVar tbchan
  case (length bchan) of
    0         -> retry
    otherwise -> modifyTVar' tbchan (flip (|>) entry)

size :: TBChan a -> STM Int
size tbchan = do
  bchan <- readTVar tbchan
  return $ sizeBChan bchan

addBChan :: a -> BChan a -> BChan a
addBChan entry rest = BChan ((sizeBChan rest)-1) entry rest

sizeBChan :: BChan a -> Int
sizeBChan (Nil size)       = size
sizeBChan (BChan size _ _) = size

pop :: TBChan a -> STM a
pop tbchan = do
  bchan <- readTVar tbchan
  hd = index bchan 0
  writeTVar ()


popBChan (Nil _)             = error "can't pop an empty bchan"
popBChan (BChan _ entry end) = (entry,end)