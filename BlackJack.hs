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
scoreCard (Card Ace _) _ = 1 
scoreCard (Card Two _) _ = 2 
scoreCard (Card Three _) _ = 3 
scoreCard (Card Four _) _ = 4 
scoreCard (Card Five _) _ = 5 
scoreCard (Card Six _) _ = 6 
scoreCard (Card Seven _) _ = 7 
scoreCard (Card Eight _) _ = 8 
scoreCard (Card Nine _) _ = 9 
scoreCard (Card _ _) _ = 10