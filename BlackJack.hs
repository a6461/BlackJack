module BlackJack where

data Suit = Diamonds | Hearts | Clubs | Spades
  deriving (Bounded, Enum, Eq, Show)

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Bounded, Enum, Eq, Show)

data Card = Card {v :: Value, s :: Suit}
  deriving (Eq, Show)