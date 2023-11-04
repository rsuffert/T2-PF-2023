import System.IO
import System.Random
import Data.Char
import Data.List.Split (splitOn)

main :: IO ()
main = forca

forca :: IO ()
forca = do
    putStrLn "\n----------------------- JOGO DA FORCA -----------------------"
    putStrLn "INSTRUÇÕES:"
    putStrLn "  - Exceto pelos comandos especiais abaixo, você só pode chutar uma letra por vez."
    putStrLn "  - Se quiser uma dica, digite ':hint' como seu chute."
    putStrLn "  - Se quiser desistir e revelar a palavra, digite ':quit' como seu chute.\n"
    (mysteryWord, hint) <- pickRandomWord "words.txt"
    let wordHidden = ['-' | _ <- mysteryWord]
    putStrLn ("Jogador, a dica inicial da palavra que você deve adivinhar é: " ++ wordHidden)
    play mysteryWord wordHidden hint []

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

play :: String -> String -> String -> [Char] -> IO ()
play palavra revealedWord hint incorrectGuesses = do
    (palavra, revealedWord, hint) <- checkWordChange palavra revealedWord hint -- evaluate if we're changing the word
    putStrLn ("Letras incorretas: " ++ incorrectGuesses)
    putStr "SEU CHUTE: "
    hFlush stdout -- flush the output buffer to print prompt before waiting for user input
    guessLine <- getLine
    if (map toUpper guessLine) == ":QUIT" then do
        putStrLn ("Você desistiu! A palavra era " ++ "'" ++ palavra ++ "'.")
        return ()
    else if (map toUpper guessLine) == ":HINT" then do
        putStrLn ("\tSua dica é: '" ++ hint ++ "'.")
        play palavra revealedWord hint incorrectGuesses
    else if length guessLine == 1 then do
        let guessedChar = guessLine !! 0
        if guessedChar `elem` palavra then do
            let guessWord = revealedWord ++ [guessedChar] 
            let result = match palavra guessWord           
            putStrLn ("\tResultado: " ++ result)
            if result == palavra then do
                putStrLn "Você acertou! Parabéns, você venceu!"
                retry
            else 
                play palavra result hint incorrectGuesses
        else do
            putStrLn ("\tA letra '" ++ [guessedChar] ++ "' não está na palavra.")
            if guessedChar `elem` incorrectGuesses then do
                putStrLn "\tVocê já chutou essa letra antes. Por favor, tente novamente."
                play palavra revealedWord hint incorrectGuesses
            else
                play palavra revealedWord hint (incorrectGuesses ++ [guessedChar])
    else do
        putStrLn "\tVocê só pode chutar uma letra por vez. Por favor, tente novamente."
        play palavra revealedWord hint incorrectGuesses
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
