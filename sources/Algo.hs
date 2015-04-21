{-# LANGUAGE TemplateHaskell #-}

module Algo (Recipe,
            Edit(..),
            levenshteinDist,
            getTransformationRecipe,
            recipeToNew,
            runTests)
    where

import Data.Vector as V hiding ((++))
import Data.List (intersperse)

import Test.QuickCheck

data EditType = AddT | DeleteT | ReplaceT | RemainT deriving (Eq, Show)

-- Replace constructor example: Replace oldChar newChar
data Edit a = Add a | Delete a | Replace a a | Remain a

type Recipe a = [Edit a]

getTransformationRecipe :: Eq a => [a] -> [a] -> Recipe a
getTransformationRecipe l1 l2 = attachChar (getEditPath l1 l2) l1 l2 
    where
        attachChar :: [EditType] -> [a] -> [a] -> Recipe a
        attachChar [] [] [] = []
        attachChar (AddT:l) s1 (h2:t2) = Add h2 : attachChar l s1 t2
        attachChar (DeleteT:l) (h1:t1) s2 = Delete h1 : attachChar l t1 s2
        attachChar (RemainT:l) (h1:t1) (h2:t2) = Remain h1 : attachChar l t1 t2
        attachChar (ReplaceT:l) (h1:t1) (h2:t2) = Replace h1 h2 : attachChar l t1 t2

recipeToNew :: Recipe a -> [a]
recipeToNew [] = []
recipeToNew (Add a:l) = a : recipeToNew l
recipeToNew (Replace _ a:l) = a : recipeToNew l
recipeToNew (Remain a:l) = a : recipeToNew l
recipeToNew (_:l) = recipeToNew l

getEditPath :: Eq a => [a] -> [a] -> [EditType]
getEditPath [] l2 = Prelude.replicate (Prelude.length l2) AddT
getEditPath l1 [] = Prelude.replicate (Prelude.length l1) DeleteT 
getEditPath l1 l2 = Prelude.reverse $ recoverPath (Prelude.length l1) (Prelude.length l2)
    where
        matrix = levenshteinMatrix l1 l2

        borderedMatrix :: Vector (Vector Int)
        borderedMatrix = V.imap V.cons (topBorder `V.cons` matrix)
            where
                topBorder = fromList $ Prelude.take (V.length $ V.head matrix) [1..]

        bm = borderedMatrix

        recoverPath :: Int -> Int -> [EditType]
        recoverPath i j | i == 0 && j == 0 = []
                        | i == 0 = AddT : recoverPath i (j - 1)
                        | j == 0 = DeleteT : recoverPath (i - 1) j
                        | bm ! i ! j == (bm ! i ! (j - 1)) + 1 = AddT : recoverPath i (j - 1)
                        | bm ! i ! j == (bm ! (i - 1) ! j) + 1 = DeleteT : recoverPath (i - 1) j
                        | bm ! i ! j == bm ! (i - 1) ! (j - 1) = RemainT : recoverPath (i - 1) (j - 1)
                        | otherwise = ReplaceT : recoverPath (i - 1) (j - 1)

levenshteinDist :: Eq a => [a] -> [a] -> Int
levenshteinDist [] l2 = Prelude.length l2
levenshteinDist l1 [] = Prelude.length l1
levenshteinDist l1 l2 = V.last $ V.last $ levenshteinMatrix l1 l2

-- should NOT be called if on of arguments is []
levenshteinMatrix :: Eq a => [a] -> [a] -> Vector (Vector Int)
levenshteinMatrix l1 l2 = V.unfoldrN (V.length a1) (uncurry getNext) (0, zerosVector)
    where
        a1 = V.fromList l1
        a2 = V.fromList l2

        zerosVector = V.fromList $ Prelude.take (V.length a2) [1..]

        getNext :: Int -> V.Vector Int -> Maybe (V.Vector Int, (Int, V.Vector Int))
        getNext ind1 v = wrap (ind1 + 1) $ V.constructN (V.length v) f
            where
                wrap :: b -> a -> Maybe (a, (b, a))
                wrap ind v = Just (v, (ind, v))

                getter :: Int -> V.Vector Int -> Int
                getter ind v | ind >= 0 = v V.! ind
                getter _ _ = ind1

                f :: V.Vector Int -> Int
                f g = let
                        curInd = V.length g
                        zeroIfEqual = if (a1 V.! ind1) == (a2 V.! curInd)
                            then 0
                            else 1

                        replaceOrStay = zeroIfEqual + getter (curInd - 1) v
                    in 
                        if V.null g
                        then min replaceOrStay (V.head v + 1)
                        else min replaceOrStay $ min (V.last g + 1) ((v V.! curInd) + 1)

-- tests

prop_levenshteinDist_bounds :: String -> String -> Bool
prop_levenshteinDist_bounds s1 s2 = (answer <= max l1 l2) && (answer >= abs (l1 - l2))
    where
        l1 = Prelude.length s1
        l2 = Prelude.length s2
        answer = levenshteinDist s1 s2

prop_levenshteinDist_symmetric :: String -> String -> Bool
prop_levenshteinDist_symmetric s1 s2 = levenshteinDist s1 s2 == levenshteinDist s2 s1

prop_levenshteinDist_intersperse :: String -> Bool
prop_levenshteinDist_intersperse s = levenshteinDist s generated == (Prelude.length generated - Prelude.length s)
    where
        generated = intersperse 'x' s

prop_levenshteinDist_equalToEditDist :: String -> String -> Bool
prop_levenshteinDist_equalToEditDist s1 s2 = levenshteinDist s1 s2 == filteredDistLength
    where
        filteredDistLength = Prelude.length $ Prelude.filter (/= RemainT) $ getEditPath s1 s2

prop_levenshteinDist_simpleCases :: Property
prop_levenshteinDist_simpleCases = forAll (elements testsList) (\(s1, s2, ans) -> levenshteinDist s1 s2 == ans)
    where
        testsList = [("abbaab", "bac", 4),
                     ("12345", "54321", 4),   
                     ("3214567", "1234567", 2),
                     ("1726354", "1234567", 5)
                    ]
prop_formStrFromEditPath :: String -> String -> Bool
prop_formStrFromEditPath s1 s2 = check path s1 s2
    where
        path :: [EditType]
        path = getEditPath s1 s2

        check :: [EditType] -> String -> String -> Bool
        check [] [] [] = True
        check (AddT:l) s1 (_:t2) = check l s1 t2
        check (DeleteT:l) (_:t1) s2 = check l t1 s2
        check (RemainT:l) (h1:t1) (h2:t2) | h1 == h2 = check l t1 t2
        check (ReplaceT:l) (h1:t1) (h2:t2) | h1 /= h2 = check l t1 t2
        check _ _ _ = False

prop_recipeToNew :: String -> String -> Bool
prop_recipeToNew s1 s2 = (recipeToNew $ getTransformationRecipe s1 s2) == s2

runTests = $quickCheckAll
