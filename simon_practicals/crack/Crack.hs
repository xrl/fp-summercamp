import CRC32_ACC
import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA     as CUDA

main :: IO ()
main = do
  s <- readFile "/usr/share/dict/american-english"
  let ls = lines s
  let [r] = toList $ CUDA.run $ find $ crcAll ls
  print (ls !! r)