import System.IO

main :: IO ()
main = forca

forca :: IO ()
forca = do
    putStr "Jogador 1, digite uma palavra: "
    hFlush stdout -- flush the output buffer to print prompt before witing for user input
    palavra <- sgetLine
    putStrLn "\nJogador 2, tente adivinhar a palavra!"
    play palavra ""

sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n' then do
        putChar x
        return []
    else do
        putChar '-'
        xs <- sgetLine
        return (x:xs)

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

play :: String -> String -> IO ()
play palavra revealedWord = do
    putStr "SEU CHUTE: "
    hFlush stdout -- flush the output buffer to print prompt before witing for user input
    guessLine <- getLine
    if length guessLine == 1 then do
        let guessWord = revealedWord ++ [guessLine!!0] -- append the guessed character to the list of correct guesses
        let result = match palavra guessWord           -- get the word with the characters that have been guessed correctly
        putStrLn ("\tResultado: " ++ result)
        if result == palavra then putStrLn "Você acertou! Parabéns, você venceu!"
        else play palavra guessWord
    else do
        putStrLn "\tVocê só pode chutar um caractere por vez. Por favor, tente novamente."
        play palavra revealedWord

match :: String -> String -> String
match palavra chute = [if elem x chute then x else '-' | x <- palavra ]