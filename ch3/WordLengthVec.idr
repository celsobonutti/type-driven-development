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

fromVect : Vect size a -> List a
fromVect [] = []
fromVect (x :: xs) = x :: fromVect xs

makeRange : { size : Nat } -> Vect size Nat
makeRange { size = Z } = []
makeRange { size = (S k) } = S k :: makeRange { size = k }

input : Vect 100 Nat
input = makeRange

myFromList : (l : List type) -> Vect (length l) type
myFromList [] = []
myFromList (x :: xs) = x :: myFromList xs

main : IO ()
main = putStrLn . show . fromVect . myReverseVec $ input

Matrix : Nat -> Nat -> Type -> Type
Matrix x y type = Vect x (Vect y type)

sumVect : Num numType => Vect x numType -> Vect x numType -> Vect x numType
sumVect [] [] = []
sumVect (x :: xs) (y :: ys) = x + y :: sumVect xs ys

createEmpties : {n : _} -> Vect n (Vect 0 type)
createEmpties {n = 0} = []
createEmpties {n = (S k)}= [] :: createEmpties

transposeHelper :
  Vect n type
  -> Vect n (Vect k type)
  -> Vect n (Vect (S k) type)
transposeHelper x xsTrans = zipWith (::) x xsTrans

transposeMatrix : {n : _} -> Num numType => Matrix m n numType -> Matrix n m numType
transposeMatrix [] = createEmpties
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
                                          transposeHelper x xsTrans


sumMatrix : Num numType => Matrix m n numType -> Matrix m n numType -> Matrix m n numType
sumMatrix xs ys = zipWith sumVect xs ys

makeRow : Num numType => Vect m numType -> Matrix p m numType -> Vect p numType
makeRow xs ys = map (\row => sum (zipWith (*) xs row)) ys

holezinho : Num numType =>
  Matrix n m numType
  -> Matrix p m numType
  -> Matrix n p numType
holezinho [] _ = []
holezinho (x :: xs) ys = makeRow x ys :: holezinho xs ys

mulMatrix : Num numType => {n, p : _}
  -> Matrix n m numType
  -> Matrix m p numType
  -> Matrix n p numType
mulMatrix xs ys = let ysTrans = transposeMatrix ys in
                            holezinho xs ysTrans

matrixOne : Matrix 3 2 Int
matrixOne = [ [1, 2]
            , [3, 4]
            , [5, 6]
            ]

matrixTwo : Matrix 2 4 Int
matrixTwo = [ [7, 8, 9, 10]
            , [11, 12, 13, 14]
            ]
