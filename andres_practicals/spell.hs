import Prelude  hiding (filter)
import Data.Char
import qualified Data.Map as Map

type NormalizedString = String
type Dictionary = Map.Map NormalizedString String



spellCheck :: Dictionary -> String -> Dictionary
spellCheck dict = Map.filter (notInDict dict) . wordsToMap

wordsToMap = Map.fromList . normalize . words
normalize = map (\x -> ( (map toLower) x, x) )
notInDict = not . flip Map.member



spellCheckFiles :: FilePath -> FilePath -> IO ()
spellCheckFiles inputPath dictPath = do
  dictData  <- readFile dictPath
  inputData <- readFile inputPath
  let incorrectWords = spellCheck (wordsToMap dictData) inputData
  putStrLn $ show incorrectWords