chunkIt :: Int -> [a] -> [[a]]
chunkIt _ [] = []
chunkIt n xs = take n xs : chunkIt n (drop n xs)


firstLoc :: Eq a => a -> [a] -> Maybe Int
firstLoc target xs = foldr (\(i, x) acc -> if x == target then Just i else acc) Nothing (zip [0..] xs)




main = do
    print (chunkIt 2 [1,2,3,4,5])      -- Expected: [[1,2], [3,4], [5]]
    print (firstLoc 3 [1,2,3,4,3])     -- Expected: Just 2

