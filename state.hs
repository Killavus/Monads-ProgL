-- Monady i inne śmieszki.
-- Autor: Marcin Grzywaczewski (killavus[at]gmail[dot]com)
-- IIUWr 2013

-- Importujemy klasę Monad ze standardowej biblioteki.
import Control.Monad
-- To bez znaczenia, do zaimplementowania głupiej funkcji na dole.
import Data.Char

-- Definicja klasy, dla typu m:
-- (>>=) - operator bind, podstawowa operacja w monadach. 
-- Typ: m a -> (a -> m b) -> m b
-- return - tworzy nam z elementu jakiegoś typu monadę: a -> m a
-- fail - niepowodzenie. Szczerze mówiąc nie implementuję tego zazwyczaj.


-- Monada Stanowa:
-- Czasem mamy do czynienia z obliczeniami, które trzymają ze 
-- sobą jakiś wewnętrzny stan.
-- Przykład: 
-- * Struktury danych, np. drzewa, kolejki, kopce, stosy.
-- * Generatory liczb pseudolosowych
-- * Parsery

-- Z tego też powodu możemy zamknąć ten stan w monadzie. Wtedy możemy go wysłać
-- raz, po czym modyfikować za pomocą odpowiednich funkcji.
-- Uładnia nam to ogólne używanie tego typu danych.
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State f) >>= g = State $ \s ->
    let (value, newState) = f s
        (State h) = g value
    in h newState

type Stack = [Integer]

pop :: State Stack Integer
pop = State $ \(x:xs) -> (x, xs)

push :: Integer -> State Stack ()
push x = State $ \xs -> ((), x:xs)

stateTest = do
  push 3
  pop
  pop
  pop


-- Monada Maybe
-- Mamy takie obliczenia, które mogą się zepsuć w środku - wtedy nie chcemy,
-- żeby były one kontynuowane.

-- Taki model obliczeń reprezentuje monada Maybe.
data MyMaybe a = MyJust a | MyNothing deriving Show

instance Monad MyMaybe where
  return x = MyJust x
  MyNothing >>= _ = MyNothing
  MyJust a >>= f = f a

myTest :: Integer -> MyMaybe Integer
myTest x 
  | x `mod` 3 == 0 = MyJust $ (x-2) * (x+1)
  | otherwise = MyNothing

-- Tutaj coś liczącego się potencjalnie długo.
myVeryHardFunction :: Integer -> MyMaybe Integer
myVeryHardFunction x = MyJust $ product [1..x]

-- Monada Maybe pozwala nam odsiać bezsensowne obliczenia.
maybeTest x y = do
  p <- myTest x
  q <- myTest y
  myVeryHardFunction $ p + q


-- MONADA LISTOWA:
-- Czasem potrzebujemy niedeterministycznego modelu obliczeń jak w Prologu -
-- czyli z nawrotami.

-- Monada listowa modeluje nam takie obliczenia.
-- Nie korzystamy z listy Haskellowej, w celu edukacyjnym implementuję listę
-- "od zera".
data MyList a = Node a (MyList a) | Nil deriving Show

myMap :: (a -> b) -> MyList a -> MyList b
myMap _ Nil = Nil
myMap f (Node a l) = Node (f a) (myMap f l)

myAppend :: MyList a -> MyList a -> MyList a
myAppend (Node a1 l1) l = Node a1 $ myAppend l1 l
myAppend Nil l = l

myFoldr :: (a -> b -> b) -> b -> MyList a -> b
myFoldr _ e Nil = e
myFoldr f e (Node a l) = f a $ myFoldr f e l

myConcat :: MyList (MyList a) -> MyList a
myConcat = myFoldr myAppend Nil

myConcatMap :: (a -> MyList b) -> MyList a -> MyList b
myConcatMap f = myConcat . myMap f 

instance Monad MyList where
  return x = Node x Nil
  l >>= f = myConcatMap f l


-- Powtarzam: Ten typ został zdefiniowany tylko i wyłącznie żeby to 
-- czysto zaimplementować.
-- Normalnie używamy Haskellowych list.

-- Przykład zastosowania: Chcemy wygenerować wszystkie zapełnienia planszy jakimiś 
-- symbolami podstawiając je za '*'. 
-- Wpp. mamy mieć 0.

-- Bardzo dobrą praktyką jest przepisanie sobie podstawowych typów i "przezwanie"
-- tak, żeby pasowały do Twojej domeny problemu. Metody też powinniśmy nazywać
-- w ten sposób, żeby to odzwierciedlały - czyli "znajdźStatek", 
-- a nie "wygenerujListęCharówKtóraTworzyWierszRozwiązania".
type Elem = Char
type Row = [Elem]
type Board = [Row]

-- Jakieś tam dane.
input :: Board
input = ["x**76**1x",
         "i8**65#@@",
         "i89271345"]

-- Jakaś tablica symboli, na które mają być podpisywane gwiazdki.
getSymbols :: Elem -> [Elem]
getSymbols '*' = ['T', 'W', 'i']
getSymbols  c  = [c]

-- Generator wiersza.
-- Odcinajcie jak najszybciej!
generateRow :: Row -> [Row]
generateRow [] = [[]]
generateRow (r:rs) = do
  sym <- getSymbols r

  map (sym:) (generateRow rs)

-- Jakaś funkcja zwracająca Bool. Tutaj jakiś bezsensowny warunek typu:
-- "Wszystkie takie wiersze, których suma kodów ASCII literek jest podzielna
--  przez 2".
checkForCorectness :: Row -> Bool
checkForCorectness r = s `mod` 2 == 0
  where
    s = foldr (+) 0 (map ord r)

-- Protip: Guardując rozwiązania, starajcie się to robić jak najwcześniej.
-- W razie potrzeby podajcie dodatkowe dane do generatora.
-- W 90% przypadków odcinanie beznadziejnych rozwiązań szybko sprawia, że
-- program działa o wiele szybciej.

-- Generator planszy. Korzysta z generatora wierszy. Zawsze starajcie się
-- składać złożone typy z mniejszych. To esencja programowania funkcyjnego ;).
generateBoard :: Board -> [Board]
generateBoard [] = [[]]
generateBoard (r:rs) = do
  gen <- generateRow r
  guard $ checkForCorectness gen -- Coś a'la odcięcie prologowe. 
                                 -- Korzystamy z tego, żeby odsiać 
                                 -- złe rozwiązania.

  map (r:) (generateBoard rs)


-- Monada IO:

-- Monada ta została wprowadzona, aby odseparować doskonały i piękny świat
-- funkcyjny od imperatywnego, barbarzyńskiego świata z efektami ubocznymi
-- działania funkcji. Takie efekty uboczne posiada czytanie i pisanie wejścia.
-- Operacje te łamią także podstawowe prawo matematycznych funkcji, czyli
-- że dla danego argumentu wynik funkcji jest zawsze taki sam.

-- Stąd też monada IO.

-- Tutaj używa się tego raczej bez zrozumienia, implementacja jest bodajże w C.
-- Rzeczy, o których warto wiedzieć:

-- Czytanie z wejścia:
--
-- main = do
--   line <- getLine -- czyta linię tekstu.
-- Potem możemy za pomocą read czytać to do typów w Haskellu. Uwaga! read jest
-- mocno kapryśny. Dlatego najlepiej przeczyścić sobie dane - usunąć "\n" itd.
-- Drugie podejście to użycie hGetContents, które czyta całe wejście na raz.

-- Wypisywanie do wyjścia:
-- main = do
--   putStrLn "lsdpalpad"