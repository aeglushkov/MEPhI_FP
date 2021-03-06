import Control.Monad

pascal :: Int -> Int -> Int
pascal 0 _ = 1
pascal c r 
    | c > r = 0
    | otherwise = (pascal c (r - 1)) + (pascal (c - 1) (r - 1))

printRows :: Int -> IO ()
printRows n = mapM_ print [[pascal y x | y <- [0..x]] | x <- [0..n]]

printIt

-- printIt2 :: Int -> C.ByteString
-- printIt2 n = C.pack $ show $ (map $ (flip (++) "\r\n") . show ) $ [[pascal y x | y <- [0..x]] | x <- [0..n]]
