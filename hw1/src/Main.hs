module Main where

import Block1
import Block2
import Block3
import Block4
import Block5

import Data.List
import Data.Monoid (Sum (..))
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

sepLine :: IO()
sepLine = putStrLn "============================="

printTestName :: String -> IO()
printTestName a = do
    sepLine
    putStrLn(a)
    sepLine

printTestRes :: (Show a, Eq a) => a -> a -> IO()
printTestRes result expected = do
    if result == expected
    then putStrLn "OK"
    else do
          putStrLn "FAILED :("
          putStrLn "Expected: "
          print $ expected
          putStrLn "Found: "
          print $ result

testOrder3 :: IO()
testOrder3 = let list3    = permutations [13, 2, 45]
                 result   = map (order3 . toTuple) list3
                 expected = map (toTuple . sort) list3
                 toTuple [a, b, c] = (a, b, c)
             in
                 printTestRes result expected

testSmartReplicate :: IO()
testSmartReplicate = let list     = [1, 2, 3]
                         result   = smartReplicate list
                         expected = [1, 2, 2, 3, 3, 3]
                     in
                         printTestRes result expected

testContains :: IO()
testContains = let lists    = [[1..5], [2,0], [3,4]]
                   result   = Block1.contains 3 lists
                   expected = [[1,2,3,4,5],[3,4]]
               in
                   printTestRes result expected

testStringSum :: IO()
testStringSum = let strings  = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t"
                               , "010 020 030", " 123 456 789 ", "-1", "-1 -2 -3"
                               , "\t-12345\t", " -123 -456 -789 ", "\n1\t\n3   555  -1\n\n\n-5"
                               , "123\t\n\t\n\t\n321 -4 -40"
                               ]
                    result   = map stringSum strings
                    expected = [1, 6, 1, 1, 1, 12345, 60, 1368, -1, -6, -12345, -1368, 553, 400]
                in
                    printTestRes result expected


testMergeSort :: IO()
testMergeSort = do
                 example <- randomIntList 8 (-10) 10
                 let result   = mergeSort example
                     expected = sort example
                 printTestRes result expected

testDays :: IO()
testDays = do
            putStrLn "nextDay SUN:"
            print (nextDay SUN)
            putStrLn "afterDays THU 5:"
            print (afterDays THU 5)
            putStrLn "isWeekend SAT:"
            print (isWeekend SAT)
            putStrLn "isWeekend WED:"
            print (isWeekend WED)
            putStrLn "daysToParty MON:"
            print (daysToParty MON)

testNat :: IO()
testNat = do
           putStrLn "natFromInt 6"
           print (natFromInt 6)
           putStrLn "intFromNat (S Z)"
           print (intFromNat (S Z))
           putStrLn "intFromNat (add (natFromInt 6) (natFromInt 4))"
           print (intFromNat (add (natFromInt 6) (natFromInt 4)))
           putStrLn "intFromNat (sub (natFromInt 6) (natFromInt 4))"
           print (intFromNat (sub (natFromInt 6) (natFromInt 4)))
           putStrLn "intFromNat (mul (natFromInt 6) (natFromInt 4))"
           print (intFromNat (mul (natFromInt 6) (natFromInt 4)))
           putStrLn "(natFromInt 6) >= (natFromInt 4)"
           print ((natFromInt 6) >= (natFromInt 4))
           putStrLn "isPrime (natFromInt 6)"
           print (isPrime (natFromInt 6))
           putStrLn "isPrime (natFromInt 5)"
           print (isPrime (natFromInt 5))
           putStrLn "intFromNat (divNat (natFromInt 6) (natFromInt 4))"
           print (intFromNat (divNat (natFromInt 6) (natFromInt 4)))
           putStrLn "intFromNat (modDivNat (natFromInt 6) (natFromInt 4))"
           print (intFromNat (modDivNat (natFromInt 6) (natFromInt 4)))

testTree :: IO()
testTree = let tree = Block3.fromList [3, 12, 5, 0, 6, 4, 3, 12]
               result = Block3.contains (addN tree 42) 42
                     && Block3.contains tree 0
                     && Block3.contains tree 12
                     && not (Block3.contains tree 7)
           in
               printTestRes result True

testFoldable :: IO()
testFoldable = let result   = foldr (++) "" (Pair "test" "complete")
                   expected = "testcomplete"
               in
                   printTestRes result expected

testSplitJoin :: IO()
testSplitJoin = let s      = "prihodite na vibori 18 marta"
                    result = joinWith 'i' (toList (splitOn 'i' s))
                in
                    printTestRes result s

testMaybeConcat :: IO()
testMaybeConcat = let lst      = [Just[1,2,3], Nothing, Just[4,5,6], Nothing]
                      result   = maybeConcat lst
                      expected = [1,2,3,4,5,6]
                  in
                      printTestRes result expected

testEitherConcat :: IO()
testEitherConcat = let lst      = [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
                       result   = eitherConcat lst
                       expected = (Sum {getSum = 8}, [1,2,3,4,5])
                   in
                       printTestRes result expected

testStringBuilder :: IO()
testStringBuilder = let s      = "prihoditenavibori"
                        result = toString (fromString s)
                    in
                        printTestRes result s

main :: IO ()
main = do
        testOrder3
        printTestName "|| Testing smartReplicate"
        testSmartReplicate
        printTestName "|| Testing contains"
        testContains
        printTestName "|| Testing stringSum"
        testStringSum
        putStrLn "||==== Block 2 Testing ====||"
        printTestName "|| Testing mergeSort"
        testMergeSort
        putStrLn "||==== Block 3 Testing ====||"
        printTestName "|| Testing days ||"
        testDays
        printTestName "|| Testing Nat ||"
        testNat
        printTestName "|| Testing BSTree ||"
        testTree
        putStrLn "||==== Block 4 Testing ====||"
        printTestName "|| Testing Foldable"
        testFoldable
        printTestName "|| Testing splitOn & joinWith"
        testSplitJoin
        putStrLn "||==== Block 5 Testing ====||"
        printTestName "|| Testing maybeConcat ||"
        testMaybeConcat
        printTestName "|| Testing eitherConcat ||"
        testEitherConcat
        printTestName "|| Testing stringBuilder ||"
        testStringBuilder
        printTestName "|| All tests passed ||"
