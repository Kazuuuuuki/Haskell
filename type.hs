import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | 
             Rectangle Point Point
  deriving (Show) 

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2))= (abs $ x2 - x1) * (abs $ y2 - y1)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)

data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)

data Vector a = Vector a a a deriving (Show)

--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker "　++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList 
  [ (100, (Taken, "ZD391"))
  , (101, (Free, "JAH31"))
  , (103, (Free, "IQSA9"))
  ]

--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--data List a = Empty | Cons { listhead :: a, listTail :: List a}
--  deriving (Show, Read, Eq, Ord)

--infixr 5 :-:
--data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

instance Eq TrafficLight where 
  Red ==  Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ False

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where 
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a 
yesnoIf yesnoVal yesResult noResult = 
  if yesno yesnoVal 
    then yesResult
    else noResult

instance Functor Maybe where 
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right)
    = Node (f x) (fmap f left) (fmap f right)








