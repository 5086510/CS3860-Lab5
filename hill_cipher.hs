import Data.Char (ord, chr)

-- Convert character to integer (A = 0, B = 1, ..., Z = 25)
charToInt :: Char -> Int
charToInt c = ord c - ord 'A'

-- Convert integer to character
intToChar :: Int -> Char
intToChar n = chr (n + ord 'A')

-- Convert string to list of integers
stringToInts :: String -> [Int]
stringToInts = map charToInt

-- Convert list of integers to string
intsToString :: [Int] -> String
intsToString = map intToChar

-- Create a 3xN matrix from a flat list of integers
createMatrix :: [Int] -> [[Int]]
createMatrix ints
  | length ints `mod` 3 /= 0 = error "List length must be divisible by 3"
  | otherwise = transpose (chunksOf 3 ints)

-- Helper function to split list into chunks of 3
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Matrix multiplication without mod 26 (for intermediate steps)
matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult a b = [[sum [rowA !! k * colB !! k | k <- [0..length rowA - 1]] | colB <- transpose b] | rowA <- a]

-- Matrix multiplication with mod 26
matrixModMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixModMult a b = [[((sum [rowA !! k * colB !! k | k <- [0..length rowA - 1]]) `mod` 26 + 26) `mod` 26 | colB <- transpose b] | rowA <- a]

-- Transpose a matrix
transpose :: [[Int]] -> [[Int]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- Flatten a matrix to a list
flatten :: [[Int]] -> [Int]
flatten xs = concat xs

-- Cipher function: Encrypt plaintext using the cipher matrix
cipher :: [[Int]] -> String -> IO String
cipher key plaintext = do
    let ints = stringToInts plaintext
        matrix = createMatrix ints
        intermediateResult = matrixMult key matrix  -- Compute without mod 26
        result = matrixModMult key matrix  -- Compute with mod 26
        colWiseResult = flatten (transpose result)
    putStrLn $ "Debug: Integer List = " ++ show ints
    putStrLn $ "Debug: Matrix = " ++ show matrix
    putStrLn $ "Debug: Product Matrix (before mod 26) = " ++ show intermediateResult
    putStrLn $ "Debug: Matrix after mod 26 = " ++ show result
    putStrLn $ "Debug: Column-wise Read = " ++ show colWiseResult
    return $ intsToString colWiseResult

-- Decipher function: Decrypt ciphertext using the decipher matrix
decipher :: [[Int]] -> String -> IO String
decipher key ciphertext = do
    let ints = stringToInts ciphertext
        matrix = createMatrix ints
        intermediateResult = matrixMult key matrix  -- Compute without mod 26
        result = matrixModMult key matrix  -- Compute with mod 26
        colWiseResult = flatten (transpose result)
    
    -- Debug statements for the part in the screenshot
    putStrLn "\n--- Decryption Debug ---"
    putStrLn $ "Debug: Ciphertext Matrix = " ++ show matrix
    putStrLn $ "Debug: Product Matrix (before mod 26) = " ++ show intermediateResult
    putStrLn $ "Debug: Matrix after mod 26 = " ++ show result

    return $ intsToString colWiseResult


-- Example cipher and decipher matrices from the assignment
cipherMatrix :: [[Int]]
cipherMatrix = [[6, 24, 1], [13, 16, 10], [20, 17, 15]]

decipherMatrix :: [[Int]]
decipherMatrix = [[8, 5, 10], [21, 8, 21], [21, 12, 8]]

-- Main function for user interaction
main :: IO ()
main = do
  putStrLn "Enter plaintext (UPPERCASE only, length divisible by 3):"
  plaintext <- getLine
  ciphertext <- cipher cipherMatrix plaintext  -- Note: <- because cipher now returns IO String
  putStrLn $ "Ciphertext: " ++ ciphertext

  -- Print decipher matrix for debug
  putStrLn "\nDebug: Decipher Matrix ="
  mapM_ print decipherMatrix

  -- Directly reuse the ciphertext for decryption
  decryptedText <- decipher decipherMatrix ciphertext  -- <- because decipher now returns IO String
  putStrLn $ "Decrypted Plaintext: " ++ decryptedText

