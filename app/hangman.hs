import System.IO
import System.Random
import Data.Char
import Data.List.Split (splitOn)

main :: IO ()
main = forca

forca :: IO ()
forca = do
    putStrLn "\n========================= JOGO DA FORCA ========================="
    putStrLn "INSTRUÇÕES:"
    putStrLn "  - Exceto pelos comandos especiais abaixo, você só pode chutar uma letra por vez."
    putStrLn "  - Se quiser uma dica, digite ':hint' como seu chute."
    putStrLn "  - Se quiser desistir e revelar a palavra, digite ':quit' como seu chute.\n"
    (mysteryWord, hint) <- pickRandomWord "words.txt"
    let wordHidden = ['-' | _ <- mysteryWord]
    putStrLn ("Jogador, a dica inicial da palavra que você deve adivinhar é: " ++ wordHidden)
    play mysteryWord wordHidden hint "" 1

pickRandomWord :: String -> IO (String, String)
pickRandomWord fileName = do
    words <- loadWords fileName
    randomIdx <- randomRIO (1, length words - 1)
    let wordAndHint = splitOn ";" (words !! randomIdx)
    if length wordAndHint == 2 then return ((wordAndHint!!0), (wordAndHint!!1))
    else error ("Esperava ler do arquivo a palavra seguida de sua dica, separados por ';', mas li: '" ++ (words !! randomIdx) ++ "'.")
    where
        loadWords :: FilePath -> IO [String]
        loadWords file  = do
            contents <- readFile file
            let wordList = words contents
            return wordList

play :: String -> String -> String -> String -> Int -> IO ()
play palavra revealedWord hint missedChars currentRound = do
    putStrLn ("\n-------------------------- RODADA " ++ (show currentRound) ++ " --------------------------")
    putStrLn ("Você ainda tem " ++ (show (6 - (length missedChars `div` 2))) ++ " tentativas.")
    drawHangman (length missedChars `div` 2) -- divide by two because we add a whitespace after each character in the string
    (palavra, revealedWord, hint) <- checkWordChange palavra revealedWord hint -- evaluate if we're changing the word
    putStr "SEU CHUTE: "
    hFlush stdout -- flush the output buffer to print prompt before witing for user input
    guessLine <- getLine
    if (map toUpper guessLine) == ":QUIT" then do
        putStrLn ("Você desistiu! A palavra era " ++ "'" ++ palavra ++ "'.")
        return ()
    else if (map toUpper guessLine) == ":HINT" then do
        putStrLn ("\tSua dica é: '" ++ hint ++ "'.")
        play palavra revealedWord hint missedChars currentRound
    else if length guessLine == 1 then do
        let guessWord = revealedWord ++ [guessLine !! 0] -- append the guessed character to the list of correct guesses
        let result = match palavra guessWord           -- get the word with the characters that have been guessed correctly
        let newMissedChars = checkError revealedWord result missedChars (guessLine !! 0)
        putStrLn ("\tChutes incorretos: " ++ newMissedChars)
        putStrLn ("\tResultado: " ++ result)
        if result == palavra then do
            putStrLn "Você acertou! Parabéns, você venceu!"
            retry
        else if length newMissedChars `div` 2 == 6 then do -- the hangman has been fully drawn and the user has lost the game (divide by 2 because we add a whitespace after each character in the string)
            drawHangman (length newMissedChars `div` 2) -- divide by two because we add a whitespace after each character in the string
            putStrLn "O enforcado está completo... Você perdeu. :("
        else 
            play palavra result hint newMissedChars (currentRound+1)
    else do
        putStrLn "\tVocê só pode chutar uma letra por vez. Por favor, tente novamente."
        play palavra revealedWord hint missedChars currentRound
    where 
        match :: String -> String -> String
        match palavra chute = [if elem x chute then x else '-' | x <- palavra]
        retry :: IO ()
        retry = do
            putStr "Você quer jogar de novo? [s/n]: "
            hFlush stdout -- flush the output buffer to print prompt before witing for user input
            response <- getLine
            let responseU = map toUpper response
            if responseU == "S" then forca
            else if responseU == "N" then putStrLn "Obrigado por jogar!"
            else do
                putStrLn "\tOpção inválida. Por favor, tente novamente."
                retry
        checkWordChange :: String -> String -> String -> IO (String, String, String)
        checkWordChange mysteryWord revealedMysteryWord hint = do
            let missingChars = length (filter (== '-') revealedMysteryWord)
            if missingChars == 1 then do
                r <- (randomRIO :: (Int, Int) -> IO Int) (1, 10)
                if r <= 3 then do -- simulate 30% of chance
                    (newMysteryWord, newHint) <- pickRandomWord "words.txt"
                    let newRevealedMysteryWord = match newMysteryWord revealedMysteryWord -- match the current characters the user had guessed in the previous word with the new mystery word
                    putStrLn ("ATENÇÃO: A PALAVRA MUDOU! Sua nova dica é: " ++ newRevealedMysteryWord)
                    return (newMysteryWord, newRevealedMysteryWord, newHint)
                else return (mysteryWord, revealedMysteryWord, hint)
            else return (mysteryWord, revealedMysteryWord, hint)
        checkError :: String -> String -> String -> Char -> String
        checkError prevRevealedWord newRevealedWord missedChars guessedChar 
            | newRevealedWord == prevRevealedWord && not (elem guessedChar missedChars) = missedChars++[guessedChar]++" " -- the user's guess was incorrect and they have not been penalized for it yet
            | otherwise = missedChars
        drawHangman :: Int -> IO ()
        drawHangman misses = do
            let members = [" O", "      /", "|", "\\", "     / ", "\\"]
            let head = buildHead members misses
            let body = buildBody members misses
            let legs = buildLegs members misses
            putStrLn (" |------" ++ head)
            putStrLn (" |"       ++ body)
            putStrLn ("_|_"      ++ legs)
            where
                buildHead :: [String] -> Int -> String
                buildHead members misses | misses == 0 = ""
                                        | otherwise   = members !! 0
                buildBody :: [String] -> Int -> String
                buildBody members misses | misses < 2 = ""
                                        | misses > 4 = buildBody members (misses-1)
                                        | otherwise  = (buildBody members (misses-1)) ++ (members !! (misses-1))
                buildLegs :: [String] -> Int -> String
                buildLegs members misses | misses < 5 = ""
                                        | otherwise  = (buildLegs members (misses-1)) ++ (members !! (misses-1))