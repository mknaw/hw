main :: IO ()
main =
    do one <- return 1
       let two = 2
       putStrLn $ show (one + two)

