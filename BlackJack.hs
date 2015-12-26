module BlackJack where

import Control.Monad.State
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

-- Тип "результат игры"
data Result = Player | Draw | Dealer
  deriving (Eq, Show)

-- Загрузка "положения в игре"  
loadStateGame :: StateGame -> StateT StateGame IO ()
loadStateGame sg = put sg >> return ()

-- Вытаскивание карты
pullCard :: StateT StateGame IO Card
pullCard = do
  (cs, cash, bet) <- get
  put (tail cs, cash, bet)
  return $ head cs

-- Изменение банка  
change_cash :: Result -> StateT StateGame IO ()
change_cash r = do
  (cs, cash, bet) <- get
  if (r == Player) then put (cs, cash + bet, bet)
  else do
   if (r == Dealer) then put (cs, cash - bet, bet)
   else put (cs, cash, bet)

-- Информация о текущем банке   
current_cash :: StateT StateGame IO Int
current_cash = do
  (cs, cash, bet) <- get
  return cash

-- Информация о текущей ставке  
current_bet :: StateT StateGame IO Int
current_bet = do
  (cs, cash, bet) <- get
  return bet

-- Размер колоды  
count :: StateT StateGame IO Int
count = do
  (cs, cash, bet) <- get
  return $ length cs

-- Проверка равенства значений карт
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
   
start :: (Int, Int) -> StateT StateGame IO (Hand, Hand) 
start (cash, bet) = do 
  deck <- lift $ shuffleDeck 52 
  loadStateGame (deck, cash, bet) 
  p1 <- pullCard 
  d1 <- pullCard
  p2 <- pullCard
  d2 <- pullCard 
  return ([p1, p2], [d1, d2])
  
player :: Hand -> StateT StateGame IO Int
player h = do
  (lift $ putStr "Your cards: ") >> (lift $ print h)
  let sh = scoreHand h
  (lift $ putStr "Your score: ") >> (lift $ print sh)
  if (sh > 21) then (lift $ putStrLn "Bust. You lose!" >> return sh)
  else ((lift $ putStr "Take card? ") >> (lift $ getLine) >>= (\s -> if (s == "Y") then (pullCard >>= (\c -> player (c:h))) else (return sh)))

-- Моделирует действия дилера
dealer :: (Hand, Int) -> StateT StateGame IO Result
dealer (h, plres) = do
  lift $ putStr "Dealer cards: "
  lift $ print h
  let sh = scoreHand h
  lift $ putStr "Dealer score: "
  lift $ print sh
  if (sh > plres && sh < 22) then return Dealer
  else do
   if (sh == plres && sh >= 17) then return Draw
    else do if (sh <= plres && sh < 17) then do c <- pullCard; dealer $ (c:h, plres) else return Player

main''' :: (Hand, Hand) -> StateT StateGame IO ()
main''' (ph, dh) = do
  lift $ putStr "Your cash: "
  cash <- current_cash
  lift $ print cash
  lift $ putStr "Dealer's first card: "
  lift $ print $ head dh
  plres <- player ph
  if (plres > 21) then do
    change_cash Dealer
    lift $ putStr "Win "
    lift $ print Dealer
    lift $ putStr "Play more? "
    s <- lift $ getLine
    if (s == "Y") then main'' else lift $ putStr "Game over!"
  else do
    d <- dealer (dh, plres)
    change_cash d
    lift $ putStr "Win "
    lift $ print d
    lift $ putStr "Play more? "
    s <- lift $ getLine
    if (s == "Y") then main'' else lift $ putStr "Game over!"

main'' :: StateT StateGame IO ()
main'' = do
  lift $ putStrLn ""
  lc <- count
  if (lc < 18) then do
   deck <- lift $ shuffleDeck 52
   cash <- current_cash
   bet <- current_bet
   loadStateGame (deck, cash, bet)
   p1 <- pullCard
   p2 <- pullCard
   d1 <- pullCard
   d2 <- pullCard
   main''' ([p1,p2],[d1,d2])
  else do
   p1 <- pullCard
   p2 <- pullCard
   d1 <- pullCard
   d2 <- pullCard
   main''' ([p1,p2],[d1,d2])

main' :: StateT StateGame IO ()
main' = do
  lift $ putStr "Enter start money: "
  cash <- lift $ getLine
  lift $ putStr "Enter bet: "
  bet <- lift $ getLine
  lift $ putStrLn ""
  let c = read cash :: Int
  let b = read bet :: Int
  if (c < b) then do
   sp <- start (c, c)
   main''' sp
  else do
   sp <- start (c, b)
   main''' sp

main = execStateT main' ([], 0, 0)