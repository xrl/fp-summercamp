type Dictionary = [String]

spellCheck :: Dictionary -> (String -> Dictionary)
spellCheck dict str = 
  words = unwords str

member :: Dictionary -> String -> Bool
member []     y = False
member (x:xs) y
  | x == y    = True
  | otherwise = member xs y
  