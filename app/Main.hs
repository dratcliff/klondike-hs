module Main where

import Lib
import System.Random

shuf :: (Deck, [Integer]) -> (Deck, [Integer])
shuf (a, []) = (a, [])
shuf (a, x:xs) = shuf (c, xs)
    where b = merge (drop (fromIntegral x) a) (take (fromIntegral x) a)
          c = merge (take (fromIntegral x) b) (drop (fromIntegral x) b)

testDeck = fst $ shuf (deck, val) where
    val = [1, 25, 4, 15, 19, 21, 3]

testDeck' = fst $ shuf (deck, val) where
    val = [1, 2, 3, 4]

run gen 0 = return ()
run gen n = do
    let
        val               = take 7 $ randomRs (1 :: Integer, 26 :: Integer) gen
        (shuffledDeck, x) = shuf (deck, val)
        board             = deal shuffledDeck
        newBoard          = solveBoard board 0 []
        sum = foldr (\x y -> y + (length (snd x))) 0 (foundation newBoard)
    gen <- newStdGen
    
    if sum == 52 then do 
        print sum
        --print newBoard
    else do
        putStr "."
    run gen (n - 1)

main = do
    gen <- getStdGen
    run gen 100  
    --putStrLn "Aces available: "
    --print $ isRankAvailable board 1
    --putStrLn ""
    --putStrLn "Visible cards: "
    --mapM_ print $ getVisibleCards board
    --putStrLn ""
    --putStrLn "Tableau: "
    --mapM_ print $ tableau board
    
