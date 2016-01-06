import P (f)
import Q (g)

h :: Int -> Int
h = f . g

main :: IO ()
main = print $ h 42
