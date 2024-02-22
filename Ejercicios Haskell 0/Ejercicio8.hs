fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci k = fibonacci (k-1) + fibonacci (k-2)
