hallarDivisores :: Int -> [Int]
hallarDivisores n = buscarDesde n 1

buscarDesde :: Int -> Int -> [Int]
buscarDesde n k
  | n == k          = [n]
  | n > k && n `mod` k == 0 = k : buscarDesde n (k+1)
  | otherwise       = buscarDesde n (k+1)
