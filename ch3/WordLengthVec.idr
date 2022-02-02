module WordLengthVec

import Data.Vect

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

insert : Ord type => (x : type) -> (xsSorted : Vect k type) -> Vect (S k) type
insert x [] = [x]
insert x (y :: ys) = case x < y of
                          False => y :: insert x ys
                          True => x :: (y :: ys)

insSort : Ord type => Vect n type -> Vect n type
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                    insert x xsSorted

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

snoc : type -> List type -> List type
snoc x [] = [x]
snoc x (y :: ys) = y :: snoc x ys

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = snoc x (myReverse xs)

snocVec : type -> Vect size type -> Vect (S size) type
snocVec x [] = [x]
snocVec x (y :: xs) = y :: snocVec x xs

myReverseVec : Vect size type -> Vect size type
myReverseVec [] = []
myReverseVec (x :: xs) = snocVec x (myReverseVec xs)

map : (a -> b) -> List a -> List b
map f [] = []
map f (x :: xs) = f x :: map f xs

mapVec : (a -> b) -> Vect size a -> Vect size b
mapVec f [] = []
mapVec f (x :: xs) = f x :: mapVec f xs
