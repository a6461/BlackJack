module BlackJack where

import System.Random

data Suit = Diamonds | Hearts | Clubs | Spades
  deriving (Bounded, Enum, Eq, Show)

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Bounded, Enum, Eq, Show)

data Card = Card {v :: Value, s :: Suit}
  deriving (Eq, Show)
 
-- Тип "рука" 
type Hand = [Card]

-- Тип "колода"
type Deck = [Card]

-- Тип "положение в игре" - (колода, счет, ставка)
type StateGame = (Deck, Int, Int)

data Result = Player | Draw | Dealer
  deriving (Eq, Show)

sameValue :: Card -> Card -> Bool
sameValue (Card v1 _) (Card v2 _) = v1 == v2
  
-- Bool - флаг наличия туза
scoreCard :: Card -> Bool -> Int 
scoreCard (Card Ace _) False = 11 
scoreCard (Card v _) _ = min (fromEnum v + 1) 10

-- Очки "руки" 
scoreHand :: Hand -> Int 
scoreHand h = fst $ foldl (\(s,b) c -> if (not b && v c == Ace) then (s + (scoreCard c False), True) else (s + (scoreCard c b), b)) (0,False) h

-- Случайная карта
randomCard :: IO Card
randomCard = do
  g <- newStdGen
  let v = fst $ randomR (0, 12) g
  g <- newStdGen
  let s = fst $ randomR (0, 3) g
  let c = Card ((toEnum v) :: Value) ((toEnum s) :: Suit)
  return $ Card ((toEnum v) :: Value) ((toEnum s) :: Suit)

-- Случайная колода (от 0 до 52 карт)
shuffleDeck :: Int -> IO Deck
shuffleDeck n
  | (n >= 0 && n < 53) = shuffleDeck' n []
  | otherwise = error "The number of cards in a range [0..52]"
  where
   shuffleDeck' :: Int -> [Card] -> IO Deck 
   shuffleDeck' 0 cs = return cs
   shuffleDeck' i cs = randomCard >>= (\c -> if (elem c cs) then (shuffleDeck' i cs) else (shuffleDeck' (i-1) (c:cs)))