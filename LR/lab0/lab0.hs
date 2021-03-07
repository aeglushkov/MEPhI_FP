import Control.Monad
import qualified Data.ByteString.Char8 as C

pascal :: Int -> Int -> Int
pascal 0 _ = 1
pascal c r = if c > r then 0 else pascal (c - 1) (r - 1) + pascal c (r - 1)

printIt :: Int -> C.ByteString
printIt n = C.pack $ show $ [pascal y x | x <- [0..n], y <- [0..x]]

printItIo :: Int -> IO ()
printItIo n = mapM_ print [[pascal y x | y <- [0..x]] | x <- [0..n]]
