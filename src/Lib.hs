module Lib where

import Data.List
import Debug.Trace

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Show)

data Color = Black | Red deriving Show

instance Eq Color where
    (==) Red Red        = True
    (==) Black Black    = True
    (==) _ _            = False

data Card = Card {
    rank :: Integer
  , suit :: Suit }
  deriving (Eq, Show)

data TableauCard = TableauCard {
    card    :: Card
  , visible :: Bool }
  deriving (Eq, Show)

type Deck = [Card]
type Stock = [Card]
type Discard = [Card]
type Tableau = [(Int, [TableauCard])]
type Foundation = [(Suit, [Card])]

data Board = Board {
    stock      :: Stock
  , discard    :: Discard
  , tableau    :: Tableau
  , foundation :: Foundation }
  deriving (Eq)

listToString :: (Show a) => [a] -> String
listToString = foldr (\x y -> show x ++ ('\n' : y)) ""
  
instance Show Board where
    show Board{stock=s, discard=d, tableau=t, foundation=f} = 
        "stock: " ++ s' ++
        "\ndiscard: " ++ d' ++
        "\ntableau: " ++ t' ++
        "\nfoundation: " ++ f' where
        
        s' = listToString s
        d' = listToString d
        t' = listToString t
        f' = listToString f

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

color :: Suit -> Color
color Club = Black
color Diamond = Red
color Heart = Red
color Spade = Black

deck :: Deck
deck = let
    ranks = [1..13]
    suits = [Club, Diamond, Heart, Spade]
    d = [Card{rank=r, suit=s} | r <- ranks, s <- suits]
    in d    

items = [(1, []), (2, []), (3, []), (4, []), (5, []), (6, []), (7, [])]
emptyFoundation = [(Club, []), (Heart, []), (Spade, []), (Diamond, [])]

deal :: Deck -> Board
deal d = go deal b 7 where
    b = Board{stock=d, discard=[], tableau=[], foundation=emptyFoundation}
    go deal b' 0 = b'
    go deal b' n = let
            stock' = stock b'
            newStock = drop n stock'
            pileCards = take n stock'
            lastVisible = (n, [TableauCard{card=c, visible=False} | c <- (init pileCards)] 
                ++ [TableauCard{card=(last pileCards), visible=True}])
            newTableau = lastVisible : (tableau b')
            newBoard = Board{stock=newStock, tableau=newTableau, discard=(discard b'), foundation=(foundation b')}
        in go deal newBoard (n - 1)

nextToFoundation :: [TableauCard] -> Foundation -> Maybe TableauCard
nextToFoundation (t:ts) f = 
    let testHigh = 5
        next = case find (\x -> ((suit (card t)) == (fst x)) && 
            ((rank (card t)) == 1 || (length (snd x)) > 0 && (rank (card t)) == (rank (last (snd x))) + 1)) f of
           Just n -> Just t
           Nothing -> nextToFoundation ts f
        oppositeColorPiles = filter (\x -> (color (fst x)) /= (color (suit (card t)))) f
        oppositeColorCards = map (\x -> (snd x)) oppositeColorPiles
        flattened = concat oppositeColorCards
        next' = case next of
            Just n ->
                case (rank (card n)) < testHigh of
                    True    -> Just n
                    False   -> case (length (filter (\x -> (rank x) == ((rank (card n)) - (testHigh - 3))) flattened)) > 0 of
                        True    -> Just n
                        False   -> nextToFoundation ts f
            Nothing -> Nothing
    in next'
nextToFoundation [] f = Nothing

testAceToFoundation = let
        tableauCard = TableauCard{card=Card{rank=1, suit=Spade}, visible=True}
        t = [tableauCard]
        f = emptyFoundation
        next = case nextToFoundation t f of
            Just n -> n
            Nothing -> error "Should have found next card"
        pass = case (next == tableauCard) of
            True    -> "pass"
            False   -> "fail"
    in pass

testNextToFoundation = let
        tc2 = TableauCard{card=Card{rank=3, suit=Spade}, visible=True}
        t = [tc2]
        f = [(Club, []), (Heart, []), (Diamond, []), (Spade, [Card{rank=1, suit=Spade}, Card{rank=2, suit=Spade}])]
        next = case nextToFoundation t f of
            Just n -> n
            Nothing -> error "Should have found next card"
        pass = case (next == tc2) of
            True    -> "pass"
            False   -> "fail"
    in pass

showLast :: Tableau -> Tableau
showLast x = map (\x -> (fst x, showLast' $ snd x)) x where
    showLast' x = case (length x) == 0 of
        True -> x
        False -> init x ++ [hide' $ last x] where
            hide' TableauCard{card=c, visible=v} = TableauCard{card=c, visible=True}

toTableauCards :: [(Int, [Card])] -> [(Int, [TableauCard])]
toTableauCards x = case (length x == 0) of
    True -> error "x is empty (toTableauCards)"
    False -> showLast cards' where
        cards = map (\x -> (fst x, hide $ snd x)) x
        cards' = case (length cards) == 0 of
            True -> error "cards is empty"
            False -> cards
        hide x = map (\x -> TableauCard{card=x, visible=False}) x
    
tryDiscard :: Board -> Int -> PastMoves -> Board
tryDiscard b i p = go b i p 0 where
    go b' c p' c' = if c' > 52 then b' else
        let count = c + 1
            result = case (length (discard b')) == 0 of
                True    -> case (length (stock b')) == 0 of
                    True    -> b'
                    False   -> go Board{stock=(drop 1 (stock b')), discard=[(head (stock b'))], 
                        tableau=(tableau b'), foundation=(foundation b')} count p' (c' + 1)
                False   -> case nextToFoundation [TableauCard{card=(head (discard b')), visible=True}] (foundation b') of
                    Just n  -> solveBoard (moveToFoundation b' (card n)) (count + 1) p'
                    Nothing -> discardToTableau where
                        discardToTableau = let
                            nextMove = case nextMoveForCard (0, TableauCard{card=(head (discard b')), visible=True}) (tableau b') p of
                                Just n  -> moveFromDiscard n --trace("next move was " ++ show n) $ moveFromDiscard n
                                Nothing -> case (length (stock b')) == 0 of
                                    True  -> go Board{stock=(reverse (discard b')), discard=[], 
                                        tableau=(tableau b'), foundation=(foundation b')} count p' (c' + 1)
                                    False -> go Board{stock=(drop 1 (stock b')), discard=(head (stock b')) : (discard b'),
                                        tableau=(tableau b'), foundation=(foundation b')} count p' (c' + 1) 
                            moveFromDiscard n = let
                                    (ys, zs) = splitAt (fst n) (tableau b')
                                    nt = (init ys) ++ [(fst (last ys), snd (last ys) ++ 
                                        [TableauCard{card=(head (discard b')), visible=True}])] ++ zs
                                    nb = Board{stock=(stock b'), discard=(tail (discard b')), tableau=nt, foundation=(foundation b')}
                                in solveBoard nb count p' --trace("n = " ++ show n ++ " discard = " ++ (show (discard b'))) $ solveBoard nb count p'
                            in nextMove
            in result --trace("try discard called " ++ show (length (stock b'))) $ result

solveBoard :: Board -> Int -> PastMoves -> Board
--solveBoard b i | trace ("b = " ++ show b) False = undefined
solveBoard b@Board{stock=s, discard=d, tableau=t:ts, foundation=f} lastCount p = 
    if lastCount > 600 then b else
    let visibleCards = getLastCardsInPiles b
        count = lastCount + 1
        tryFoundation = case nextToFoundation visibleCards f of
            Just n -> solveNext (moveToFoundation b (card n))
            Nothing -> moveInPiles
        solveNext b' = solveBoard Board{stock=(stock b'), discard=(discard b'), 
            tableau=(showLast (tableau b')), foundation=(foundation b')} count p
        moveInPiles = case (nextMoveableCards (tableau b)) of
            Just n  -> makeMove n
            Nothing -> tryDiscard b count p
        makeMove card = case nextMoveForCard card (tableau b) p of
            Just n  -> moveIt card n --trace("next move for " ++ show card ++ " was " ++ show n) $ moveIt card n
            Nothing -> tryDiscard b count p
        moveIt from to = let 
                t' = moveCard from (tableau b) to
                p' = (from, to) : p
            in solveBoard Board{stock=s, discard=d, tableau=(showLast t'), foundation=f} count p'
    in tryFoundation
solveBoard Board{stock=s, discard=d, tableau=[], foundation=f} _ _ = 
    Board{stock=[], discard=[], tableau=[], foundation=[]}

hasVisible :: [TableauCard] -> Integer -> Bool
hasVisible x y = any (\x -> visible x y) x where
    visible t@TableauCard{visible=True} y = (rank (card t)) == y
    visible TableauCard{visible=False} y = False

lastCardRankMatches :: [TableauCard]
lastCardRankMatches = undefined

checkEach :: Tableau -> Integer -> Bool
checkEach x y = any (\x -> hasVisible (snd x) y) x
    
isRankAvailable :: Board -> Integer -> Bool
isRankAvailable x y = checkEach t y where
    t = tableau x
        
getLastCardsInPiles :: Board -> [TableauCard]
getLastCardsInPiles b = foldr (\x y -> 
    if length (snd x) > 0 then (last (snd x)) : y else y) [] (tableau b)

moveToFoundation :: Board -> Card -> Board
moveToFoundation board card = 
    let d = discard board
        newBoard = case ((length d) > 0) && ((head d) == card) of 
            True    -> Board {stock=s, discard=d', tableau=t', foundation=f'} where
                s = stock board
                t' = tableau board
                d' = drop 1 d
                f = foundation board
                f' = addToFoundation card f
            False   -> Board {stock=s, discard=d, tableau=t', foundation=f'} where
                s = stock board
                t = tableau board
                t' = removeFromTableau card t
                d = discard board
                f = foundation board
                f' = addToFoundation card f
    in newBoard

testMoveToFoundation = let
    b = deal deck
    newB = moveToFoundation b Card {rank=7, suit=Spade}
    in newB

addToFoundation :: Card -> Foundation -> Foundation
addToFoundation card foundation =
    let thisCardsPile = filter (\x -> (fst x) == (suit card)) foundation
        newFoundation = case (length thisCardsPile) == 0 of
            True  -> foundation
            False -> newPile : otherPiles where
                otherPiles = filter (\x -> (fst x) /= (suit card)) foundation
                newPile = ((suit card), (snd (head thisCardsPile)) ++ [card])
    in newFoundation

removeFromTableau :: Card -> Tableau -> Tableau
removeFromTableau c tableau = 
    let index = whichIndexContains c tableau
        tableauPile     = case lookup index tableau of
            Just n  -> n
            Nothing -> error "index not found"
        newTableauPile  = delete TableauCard{card=c, visible=True} tableauPile
        splitTableau    = splitAt (index - 1) tableau 
        newSnd          = (index, newTableauPile) : (tail (snd splitTableau))
        newTableau      = (fst splitTableau) ++ newSnd
    in newTableau

whichIndexContains :: Card -> Tableau -> Int
whichIndexContains card tableau = 
    let item = filter (\x -> elem TableauCard{card=card, visible=True} (snd x)) tableau
        index = case (length item) == 0 of
            True  -> error "card not found"
            False -> fst $ head item
    in index

nextMoveableCards :: Tableau -> Maybe (Int, TableauCard)
nextMoveableCards [] = Nothing
nextMoveableCards ts = let 
        moveableCards = map (\x -> getMoveableCards x) ts
        flattened = concat $ map (\x -> map (\y -> (fst x, y)) (snd x)) moveableCards
        result = case (length flattened) == 0 of
            True  -> Nothing
            False -> nextMove flattened where
                nextMove [] = Nothing
                nextMove cs = nextMove' where 
                    ts' = filter (\x -> (fst x) /= (fst (last cs))) ts
                    nextMove' = case nextMoveForCard (last cs) ts' [] of
                        Just n  -> Just (last cs)
                        Nothing -> nextMove (init cs)
                        
    in result

testNextMoveableCards = let
        tableau = [(1, [TableauCard{card=Card{suit=Club, rank=5}, visible=True},
            TableauCard{card=Card{suit=Heart, rank=4}, visible=True}, 
            TableauCard{card=Card{suit=Spade, rank=3}, visible=True}]),
            (2, [TableauCard{card=Card{suit=Heart, rank=6}, visible=True}])]
        tableau' = [(1, [TableauCard{card=Card{suit=Club, rank=5}, visible=True},
            TableauCard{card=Card{suit=Heart, rank=4}, visible=True}, 
            TableauCard{card=Card{suit=Spade, rank=3}, visible=True}]),
            (2, [TableauCard{card=Card{suit=Heart, rank=7}, visible=True}])]
        shouldPass = nextMoveableCards tableau
        shouldFail = nextMoveableCards tableau'
    in (shouldPass, shouldFail)

getMoveableCards :: (Int, [TableauCard]) -> (Int, [TableauCard])
getMoveableCards t = let
        cards = snd t
        tsuit x = suit (card x)
        trank x = rank (card x)
        adjacent x y = ((visible x) && (visible y)) &&
                (color (tsuit x)) /= (color (tsuit y)) &&
                (trank x) == ((trank y) + 1)
        result = case (length cards) < 2 of
            True  -> t
            False -> (fst t, moveableCards) where
                moveableCards = foldr (\x y -> if (adjacent x (head y)) then (x:y) else y) [(last cards)] (init cards)
        result' = case (result == t) of
            True    -> (fst t, [])
            False   -> result
    in result

testGetMoveableCards = let
        tableau = (1, [TableauCard{card=Card{suit=Club, rank=5}, visible=True},
                        TableauCard{card=Card{suit=Heart, rank=4}, visible=True}, 
                        TableauCard{card=Card{suit=Spade, rank=3}, visible=True}])
        result = getMoveableCards tableau
    in result

type PastMoves = [((Int, TableauCard), (Int, TableauCard))]

nextMoveForCard :: (Int, TableauCard) -> Tableau -> PastMoves -> Maybe (Int, TableauCard)
nextMoveForCard tc [] p     = Nothing
nextMoveForCard tc (t:ts) p = let
        matches tc' t' = case (length (snd t')) == 0 of
            True  -> (rank (card tc')) == 13
            False -> (color (suit (card tc'))) /= (color (suit (card (last (snd t'))))) &&
                (rank (card tc')) == (rank (card (last (snd t'))) - 1) &&
                (visible tc') &&
                (visible (last (snd t')))
        tc'' = snd tc
        match = case matches tc'' t of
            True  -> Just (fst t, if (length (snd t)) == 0 then TableauCard{card=Card{suit=Spade, rank=100}, visible=True} else (last (snd t)))
            False -> nextMoveForCard tc ts p
        match' = case match of
            Just n -> if (length (filter (\x -> ((fst x) == tc) && ((snd x) == n)) p)) /= 0 then Nothing else Just n
            Nothing -> Nothing
    in match'

testnextMoveForCard = let
        tc = (3, TableauCard{card=Card{suit=Heart, rank=9}, visible=True})
        tableau = [(1, [TableauCard{card=Card{suit=Diamond, rank=10}, visible=True}]),
                    (2, [TableauCard{card=Card{suit=Club, rank=12}, visible=True},
                            TableauCard{card=Card{suit=Heart, rank=11}, visible=True}, 
                            TableauCard{card=Card{suit=Spade, rank=10}, visible=True}])]
        result = nextMoveForCard tc tableau
    in result

moveCard :: (Int, TableauCard) -> Tableau -> (Int, TableauCard) -> Tableau
moveCard from t to = let
        srcPile = case lookup (fst from) t of
            Just n -> n
            Nothing -> error "not found"
        toMove = dropWhile (\x -> (card x) /= (card (snd from))) srcPile
        toStay = takeWhile (\x -> (card x) /= (card (snd from))) srcPile
        newTableau = let 
                (ys, zs) = splitAt (fst from) t 
                ys' = delete (fst from, srcPile) ys
            in ys' ++ [(fst from, toStay)] ++ zs
        dstPile = case lookup (fst to) t of
            Just n -> n
            Nothing -> error "not found"
        newTableau' = let
                (ys, zs) = splitAt (fst to) newTableau
                ys'      = delete (fst to, dstPile) ys
            in ys' ++ [(fst to, dstPile ++ toMove)] ++ zs
    in newTableau'
    

testMoveCard = let
        tc = (3, TableauCard{card=Card{suit=Heart, rank=9}, visible=True})
        tableau = [(1, [TableauCard{card=Card{suit=Diamond, rank=10}, visible=True}]),
                        (2, [TableauCard{card=Card{suit=Club, rank=12}, visible=True},
                                TableauCard{card=Card{suit=Heart, rank=11}, visible=True}, 
                                TableauCard{card=Card{suit=Spade, rank=10}, visible=True}]),
                        (3, [TableauCard{card=Card{suit=Heart, rank=9}, visible=True},
                              TableauCard{card=Card{suit=Spade, rank=8}, visible=True}])]
        nextMove = case nextMoveForCard tc tableau [] of
            Nothing -> error "should've found move"
            Just n  -> n
        newTableau = moveCard tc tableau nextMove
    in newTableau
