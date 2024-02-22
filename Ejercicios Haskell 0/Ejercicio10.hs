verificarPalindromo :: String -> Bool
verificarPalindromo cadena = normalizada == reverse normalizada
  where normalizada = map toLower $ filter isAlpha cadena

ejecutaPalindromo :: IO ()
ejecutaPalindromo = do
  putStrLn "Introduce texto para verificar palindromía:"
  texto <- getLine
  if verificarPalindromo texto
    then putStrLn "Es un palíndromo."
    else putStrLn "No es un palíndromo."
