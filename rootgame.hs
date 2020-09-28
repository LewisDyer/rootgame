rootgame :: (Int -> Int) -> Int -> IO ()
rootgame fn guesses = turn fn guesses False


check :: (Int -> Int) -> Int -> (Bool, Int)
check fn input =
    let
        output = fn input
    in
        (output == 0, output)

turn :: (Int -> Int) -> Int -> Bool -> IO ()
turn fn guesses correct =
    do if guesses==0
        then putStrLn "You lose..."
        else if correct
            then putStrLn "You win!"
            else mkguess fn guesses


mkguess :: (Int -> Int) -> Int -> IO ()
mkguess fn guesses =
  do putStrLn (take guesses (repeat '*')) 
     putStr "Enter an input value: "
     rawInput <- getLine
     let input = read rawInput :: Int
     let (correct, output) = check fn input
     putStrLn ("f(" ++ show input ++ ") = " ++ show output)
     let guesses' = if correct then guesses else guesses-1
     turn fn guesses' correct
