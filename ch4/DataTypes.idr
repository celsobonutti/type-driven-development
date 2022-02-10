module DataTypes

import Data.Fin
import Data.Vect

data BSTree : Type -> Type where
  Empty : Ord type => BSTree type
  Node : Ord type => (left : BSTree type) -> (val : type) -> (right : BSTree type) -> BSTree type

%name BSTree tree, tree1

insert : {elem : _} -> elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                    LT => Node (insert x left) val right
                                    EQ => orig
                                    GT => Node left val (insert x right)

listToTree : {type : _} -> Ord type => List type -> BSTree type
listToTree = foldr (\x, tree => insert x tree) Empty

treeToList : {type : _} -> BSTree type -> List type
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

data Expr
  = Single Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

evaluate : Expr -> Int
evaluate (Single x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Nothing
maxMaybe (Just x) (Just y) = Just $ max x y

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle _ -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels (Car _) = 4
wheels (Bus _) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car (fuel + 100)
refuel (Bus fuel) = Bus (fuel + 200)
refuel (Motorcycle fuel) = Motorcycle (fuel + 50)

data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : (x : a) -> (xs : MyVect k a) -> MyVect (S k) a

myAppend : MyVect n type -> MyVect m type -> MyVect (n + m) type
myAppend [] y = y
myAppend (x :: xs) y = x :: myAppend xs y

myZip : MyVect n a -> MyVect n b -> MyVect n (a, b)
myZip [] [] = []
myZip (x :: xs) (y :: z) = (x, y) :: myZip xs z

myIndex : Fin n -> MyVect n a -> a
myIndex FZ (x :: xs) = x
myIndex (FS x) (y :: ys) = myIndex x ys

tryIndex : {n : _} -> Integer -> MyVect n a -> Maybe a
tryIndex {n} x xs = case integerToFin x n of
                         Nothing => Nothing
                         (Just y) => Just $ myIndex y xs

myTake : (n : Nat) -> Vect (n + m) type -> Vect n type
myTake 0 xs = []
myTake (S k) (x :: xs) = x :: myTake k xs

sumEntriesFin : Num a => Vect n a -> Vect n a -> Fin n -> a
sumEntriesFin (x :: xs) (y :: ys) FZ = x + y
sumEntriesFin (x :: xs) (y :: ys) (FS k) = sumEntriesFin xs ys k

sumEntries : {n : _} -> Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos xs ys = map (sumEntriesFin xs ys) (integerToFin pos n)

keepOpen : a
keepOpen = ?hole
