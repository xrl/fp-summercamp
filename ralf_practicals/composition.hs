word_count :: String -> Int
word_count = length . words

line_count :: String -> Int
line_count = length . lines

{-
  Has issues. Will goof up on ellipsis.
-}
sentence_count :: String -> Integer
sentence_count str = counter 0 str
  where counter cnt [] = cnt
        counter cnt (c:cs)
          | c == '.'  = counter (cnt+1) cs
          | c == "!"  = counter (cnt+1) cs
          | otherwise = counter cnt     cs

Hello there... this is bad.

avg_words_line :: String -> Double
avg_words_line = (fromIntegral $ sum counts) / (fromIntegral $ length counts)
  where counts = (map word_count) lines
