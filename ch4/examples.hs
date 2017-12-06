module Examples where

-- to check if a string has 'a' as the second char
test (_:'a':_) = True
test _ = False
