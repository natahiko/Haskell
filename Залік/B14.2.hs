{-# OPTIONS_GHC -Wall #-}
module B14 where

mergeFiles :: IO()
mergeFiles = do putStr "Filenames> "
                line <- getLine
                let files = split ' ' line
                file1 <- readFile (files!!0)
                file2 <- readFile (files!!1)
                file3 <- readFile (files!!2)
                writeFile "stdout.txt" (file1 ++ "\n" ++ file2 ++ "\n" ++ file3)

split :: Char -> String -> [String]
split _ [] = [""]
split ch (c:cs) | c == ch  = "" : rest
                | otherwise = (c : head rest) : tail rest
    where rest = split ch cs
