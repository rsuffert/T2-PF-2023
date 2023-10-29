import System.IO

main = forca

forca :: IO ()
forca = do
    putStrLn "Digite uma palavra:"
    palavra <- sgetLine
    putStrLn "Tente adivinhar a palavra:"
    play palavra

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

play :: String -> IO ()
play palavra = do
    putStr "? "
    chute <- getLine
    if chute == palavra then
        putStrLn "Você acertou!"
    else do
        putStrLn (match palavra chute)
        play palavra

match :: String -> String -> String
match palavra chute = [if elem x chute then x else '-' | x <- palavra ]