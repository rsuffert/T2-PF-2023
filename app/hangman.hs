import System.IO
import System.Random
import Data.Char

main :: IO ()
main = forca

forca :: IO ()
forca = do
    putStrLn "\n------------------- JOGO DA FORCA -------------------"
    putStrLn "INSTRUÇÕES:"
    putStrLn "  - Você só pode chutar uma letra por vez."
    putStrLn "  - Se quiser desistir e revelar a palavra, digite 'sair' como seu chute.\n"
    mysteryWord <- pickRandomWord "words.txt"
    let wordHidden = ['-' | _ <- mysteryWord]
    putStrLn ("Jogador, a dica da palavra que você deve adivinhar é: " ++ wordHidden)
    play mysteryWord wordHidden

pickRandomWord :: String -> IO String
pickRandomWord fileName = do
    words <- loadWords fileName
    randomIdx <- randomRIO (0, length words - 1)
    return (words !! randomIdx)
    where
        loadWords :: FilePath -> IO [String]
        loadWords file  = do
            contents <- readFile file
            let wordList = words contents
            return wordList

play :: String -> String -> IO ()
play palavra revealedWord = do
    (palavra, revealedWord) <- checkWordChange palavra revealedWord -- evaluate if we're changing the word
    putStr "SEU CHUTE: "
    hFlush stdout -- flush the output buffer to print prompt before witing for user input
    guessLine <- getLine
    if (map toUpper guessLine) == "SAIR" then do
        putStrLn ("Você desistiu! A palavra era " ++ "'" ++ palavra ++ "'.")
        return ()
    else if length guessLine == 1 then do
        let guessWord = revealedWord ++ [guessLine !! 0] -- append the guessed character to the list of correct guesses
        let result = match palavra guessWord           -- get the word with the characters that have been guessed correctly
        putStrLn ("\tResultado: " ++ result)
        if result == palavra then do
            putStrLn "Você acertou! Parabéns, você venceu!"
            retry
        else 
            play palavra result
    else do
        putStrLn "\tVocê só pode chutar um caractere por vez. Por favor, tente novamente."
        play palavra revealedWord
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
        checkWordChange :: String -> String -> IO (String, String)
        checkWordChange mysteryWord revealedMysteryWord = do
            let missingChars = length (filter (== '-') revealedMysteryWord)
            if missingChars == 1 then do
                r <- (randomRIO :: (Int, Int) -> IO Int) (1, 10)
                if r <= 3 then do -- simulate 30% of chance
                    newMysteryWord <- pickRandomWord "words.txt"
                    let newRevealedMysteryWord = match newMysteryWord revealedMysteryWord -- match the current characters the user had guessed in the previous word with the new mystery word
                    putStrLn ("ATENÇÃO: A PALAVRA MUDOU! Sua nova dica é: " ++ newRevealedMysteryWord)
                    return (newMysteryWord, newRevealedMysteryWord)
                else return (mysteryWord, revealedMysteryWord)
            else return (mysteryWord, revealedMysteryWord)