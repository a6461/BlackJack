module BlackJack where

data Suit = Diamonds | Hearts | Clubs | Spades
  deriving (Bounded, Enum, Eq, Show)

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Bounded, Enum, Eq, Show)

data Card = Card {v :: Value, s :: Suit}
  deriving (Eq, Show)
  
-- Bool - флаг наличия туза
scoreCard :: Card -> Bool -> Int 
scoreCard (Card Ace _) False = 11 
scoreCard (Card v _) _ = min (fromEnum v + 1) 10