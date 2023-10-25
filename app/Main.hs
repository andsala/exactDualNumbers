module Main  where
import           CReal
import           ExactReal


main =
  do
    s <- getLine
    n <- return (read s)
--    print (unR newHalf n)
    print (unR fastNewHalf n)
