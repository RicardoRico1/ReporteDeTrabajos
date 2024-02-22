seleccionarPares :: [Int] -> [Int]
seleccionarPares [] = []
seleccionarPares (z:zs)
  | even z    = z : seleccionarPares zs
  | otherwise = seleccionarPares zs
