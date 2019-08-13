validPoundCoins = [ 1, 2, 5, 10, 20, 50, 100, 200 ]
getChange :: [Int] -> Int -> [Int] -> [Int]
getChange [] _ _ = []
getChange _ 0 _ = []
getChange ys x acc
            | x == mx = mx:acc
            | otherwise = getChange ys (x-mx) (mx:acc)
            where mx = maximum [ y | y <- ys, y <= x, x < 5000 ]

getChangeInPounds :: Int -> [Int]
getChangeInPounds x = getChange validPoundCoins x []
