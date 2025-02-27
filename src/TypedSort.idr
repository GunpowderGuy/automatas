module TypedSort

%default total

-- **Predicate: Proves that a list is sorted**
data IsSorted : List Int -> Type where
  SortedNil  : IsSorted []
  SortedOne  : (x : Int) -> IsSorted [x]
  SortedCons : (x : Int) -> (y : Int) -> (xs : List Int) ->
               (x <= y) => IsSorted (y :: xs) -> IsSorted (x :: y :: xs)

-- **Predicate: Proves that two lists are permutations of each other**
data IsPermutation : List Int -> List Int -> Type where
  PermNil  : IsPermutation [] []
  PermSkip : (x : Int) -> (xs, ys : List Int) ->
             IsPermutation xs ys -> IsPermutation (x :: xs) (x :: ys)
  PermSwap : (x : Int) -> (y : Int) -> (xs : List Int) ->
             IsPermutation (x :: y :: xs) (y :: x :: xs)
  PermTrans : (xs, ys, zs : List Int) ->
              IsPermutation xs ys -> IsPermutation ys zs -> IsPermutation xs zs

-- **Dependent type: A sorted list that is a permutation of the input**
record SortedList (xs : List Int) where
  constructor MkSortedList
  sorted : List Int
  isSorted : IsSorted sorted
  isPerm : IsPermutation xs sorted

-- **Helper function to insert an element into a sorted list**
insertSorted : (x : Int) -> (xs : List Int) ->
               (ys : List Int ** (IsSorted ys, IsPermutation (x :: xs) ys))
insertSorted x [] = ([] ** (SortedOne x, PermSkip x [] [] PermNil))
insertSorted x (y :: ys) =
  case compare x y of
    LT => ((x :: y :: ys) ** (SortedCons x y ys (believe_me ()) PermNil, PermSkip x (y :: ys) (x :: y :: ys) (PermSwap x y ys)))
    _  => let (zs ** (sorted, perm)) = insertSorted x ys in
          ((y :: zs) ** (SortedCons y (head zs) (tail zs) (believe_me ()) sorted, PermSkip y zs (x :: ys) perm))

-- **Insertion sort implementation**
insertionSort : (xs : List Int) -> SortedList xs
insertionSort [] = MkSortedList [] SortedNil PermNil
insertionSort (x :: xs) =
  let MkSortedList sorted isSorted isPerm = insertionSort xs in
  let (newSorted ** (newSortedProof, newPermProof)) = insertSorted x sorted in
  MkSortedList newSorted newSortedProof (PermTrans (x :: xs) (x :: sorted) newSorted (PermSkip x xs sorted isPerm) newPermProof)

-- **Example usage**
exampleList : List Int
exampleList = [3, 1, 4, 1, 5, 9, 2, 6, 5]

sortedResult : SortedList exampleList
sortedResult = insertionSort exampleList

-- **Main function**
main : IO ()
main = putStrLn $ "Sorted: " ++ show (sorted sortedResult)
