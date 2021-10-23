main =
    putStrLn "Greetings! whats your name" >>
    getLine >>=
    (\inpStr -> putStrLn $ "welcome " ++ inpStr)
