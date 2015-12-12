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

-- Bool - флаг наличия туза
scoreCard :: Card -> Bool -> Int 
scoreCard (Card Ace _) False = 11 
scoreCard (Card v _) _ = min (fromEnum v + 1) 10

scoreHand :: Hand -> Int 
scoreHand h = fst $ foldl (\(s,b) c -> if (not b && v c == Ace) then (s + (scoreCard c False), True) else (s + (scoreCard c b), b)) (0,False) h

randomCard :: IO Card
randomCard = do
  g <- newStdGen
  let v = fst $ randomR (0, 12) g
  g <- newStdGen
  let s = fst $ randomR (0, 3) g
  let c = Card ((toEnum v) :: Value) ((toEnum s) :: Suit)
  return $ Card ((toEnum v) :: Value) ((toEnum s) :: Suit)