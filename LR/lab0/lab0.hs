import Control.Monad
import qualified Data.ByteString.Char8 as C

pascal :: Int -> Int -> Int
pascal c r = 1 -- зедсь должно появиться решение

printIt :: Int -> C.ByteString
printIt n = C.pack $ show $ [pascal y x | x <- [0..n], y <- [0..x]]

printItIo :: Int -> IO ()
printItIo n = mapM_ print [[pascal y x | y <- [0..x]] | x <- [0..n]]
