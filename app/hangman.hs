import System.IO
import System.Random

main :: IO ()
main = forca

forca :: IO ()
forca = do
    putStrLn "\n------------------- JOGO DA FORCA -------------------"
    word <- pickRandomWord "words.txt"
    let wordHidden = ['-' | _ <- word]
    putStrLn ("Jogador, você deve adivinhar a palavra: " ++ wordHidden)
    play word ""

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
    putStr "SEU CHUTE: "
    hFlush stdout -- flush the output buffer to print prompt before witing for user input
    guessLine <- getLine
    if length guessLine == 1 then do
        let guessWord = revealedWord ++ [guessLine !! 0] -- append the guessed character to the list of correct guesses
        let result = match guessWord           -- get the word with the characters that have been guessed correctly
        putStrLn ("\tResultado: " ++ result)
        if result == palavra then putStrLn "Você acertou! Parabéns, você venceu!"
        else play palavra guessWord
    else do
        putStrLn "\tVocê só pode chutar um caractere por vez. Por favor, tente novamente."
        play palavra revealedWord
    where 
        match :: String -> String
        match chute = [if elem x chute then x else '-' | x <- palavra]