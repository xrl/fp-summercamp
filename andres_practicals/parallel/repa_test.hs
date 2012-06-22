import Prelude hiding (map)
import Data.Array.Repa

main :: IO ()
main = do
  let oneDim       = (fromListUnboxed (Z:.10)   [1..10])       :: Array U (Z:.Int)      Int
  let twoDim       = (fromListUnboxed (Z:.2:.5) [1..10])       :: Array U (Z:.Int:.Int) Int
  let twoDoubleDim = (fromListUnboxed (Z:.2:.5) [1.0 .. 10.0]) :: Array U (Z:.Int:.Int) Float

  timesTwo <- computeUnboxedP $ map (* 2) oneDim
  --putStrLn $ index timesTwo  2

  let funcy = fromFunction (Z:.1) (\x -> 1)
  putStrLn $ show funcy

  --putStrLn $ show timesTwo