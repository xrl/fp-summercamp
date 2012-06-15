import Control.Monad.State
import Tree

--machine :: State Int Int
--machine = do
--  n <- get
--  put (n+1)
--  return ()



sumS :: Num a => [a] -> State a a
sumS []     = do
  what_i_want <- get
  return what_i_want
sumS (x:xs) = do
  current_count <- get
  let next_count = current_count+x
  put next_count
  sumS xs

decorate :: Tree a -> State Int (Tree (a,Int))
decorate Empty        = do
  count <- get
  return Empty
decorate (Node l y r) = do
  new_left  <- decorate l
  current_count <- get
  let next_count = current_count + 1
  put next_count
  new_right <- decorate r
  return $ Node new_left (y,next_count) new_right

main :: IO ()
main = putStrLn "hi"