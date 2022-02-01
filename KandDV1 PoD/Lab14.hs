module Main where
import Debug.Trace
import Test.HUnit
import Text.Printf -- for printing stuff out


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = trace ("n: " ++ show n) $ fib (n - 1) + fib (n - 2)
 
foo x = x * x

test1 = TestCase (assertEqual "for (foo 3)," 9 (foo 3))
test2 = TestCase (assertBool "foo 10 > 0" (foo 10 > 0))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main = putStrLn $ "fib 4: " ++ show (fib 4)


{-
For each program:

Step 1: Apply it to various inputs to see whether you can spot a bug -- this should be easy, the programs are very broken.

Step 2: Write test cases for the programs using HUnit. Run your test cases. Write other test cases that you expect to succeed that test the boundary conditions.

Step 3: Use the trace functionality to better understand what's going on inside the buggy function. Does this help you locate the bug?

Hint: to make the output of the tracing easier to read, consider writing a tracing helper function.

Step 4 (Optional): Run the debugger on the code for some buggy input. Step through the program. Print out intermediate values to see whether they are what you expect (in general, try out all the commands available in the debugger). Do you understand the output the debugger gives?

Step 5 (Optional): Try to use QuickCheck to automatically find bugs in your program. Read the documentation, formulate some properties, try to code them up, and run QuickCheck warning, QuickCheck is not easy, but it is a very useful tool.

Step 6: If you find the bugs, try to fix them (this is not the most important part of the lab) and run your test case on them.

Hint: fixing one bug may reveal another one that may have been hidden/masked.

Step 7: Discuss the following points:
- Which of these tools could you use in which circumstances?
- What strategies for bug-finding did you use?
- How well do you imagine that these tools could be applied to larger programs?
-}


-- Return whether a string contains balanced brackets.  Nothing indicates a
-- balanced string, while (Just i) means an imbalance was found at, or just
-- after, the i'th bracket.  We assume the string contains only brackets ("[" and "]").


isBalanced :: String -> Maybe Int
isBalanced = bal (-1) 0
  where bal :: Int -> Int -> String -> Maybe Int
        bal _   0      []  = Nothing
        bal i   _      []  = Just i
        bal i (-1)      _  = Just i
        bal i   n ('[':bs) = bal (i+1) (n+1) bs
        bal i   n (']':bs) = bal (i+1) (n-1) bs
        -- bal i   _      []  = Just i
        -- bal i (-1)      _  = Nothing
        -- bal i   n ('[':bs) = trace ("n:" ++ show n ++ "    i:" ++ show i ++ "     bs:" ++ show bs) $ bal (i+1) (n+1) bs
        -- bal i   n (']':bs) = trace ("n:" ++ show n ++ "    i:" ++ show i ++ "     bs:" ++ show bs) $ bal (i+1) (n+1) bs
 
-- Print a string, indicating whether it contains balanced brackets.  If not,
-- indicate the bracket at which the imbalance was found.
check :: String -> IO ()
check s = maybe (good s) (bad s) (isBalanced s)
  where good s   = printf "Good \"%s\"\n" s
        bad  s n = printf "Bad  \"%s\"\n%*s^\n" s (n+6) " "

t1 = isBalanced "[[]]" -- == Just 3
t2 = isBalanced "[]]" -- == Just 2
t3 = isBalanced "" -- == Nothing
t4 = isBalanced "][][" -- == Just 3


-- 1
-- t1 == t2 == t3 == t4 *** Exception: Lab14.hs:(54,9)-(58,47): Non-exhaustive patterns in function bal

--2
testP101 = TestCase $ assertEqual "isBalanced empty string" Nothing (isBalanced "") -- edge case
testP102 = TestCase $ assertEqual "isBalanced balanced string" Nothing (isBalanced "[[]]") -- see if it is working
testP103 = TestCase $ assertEqual "isBalanced unbalanced string" (Just 0) (isBalanced "][][") -- see if it is working
-- fler tester

runP1tests = runTestTT $ TestList [TestLabel "isBalanced empty string" testP101, TestLabel "isBalanced balanced string" testP102, TestLabel "isBalanced unbalanced string" testP103] 

-- == ### Failure in: 1:isBalanced balanced string
-- Lab14.hs:80
-- isBalanced balanced string
-- expected: Nothing
--  but got: Just 3
-- ### Failure in: 2:isBalanced unbalanced string
-- Lab14.hs:81
-- isBalanced unbalanced string
-- expected: Just 0
--  but got: Just 3
-- Cases: 3  Tried: 3  Errors: 0  Failures: 2
-- Counts {cases = 3, tried = 3, errors = 0, failures = 2}

-- This means that an empty string does not cause any errors and returns what we expect. 
-- 

-- 3
{-
isBalanced "[[]]"
n:0    i:-1     bs:"[]]"
n:1    i:0     bs:"]]"
n:2    i:1     bs:"]"
n:3    i:2     bs:""
Just 3

isBalanced "][]["
n:0    i:-1     bs:"[]["
n:1    i:0     bs:"]["
n:2    i:1     bs:"["
n:3    i:2     bs:""
Just 3
-}

-- 6
{-

So i seems to be the index ("of the previous round" in the tracing, it is correct in the code)
I assume n to be to keep track on left vs right brackets, but it is only adding upwards, it should be negative if it is a right bracket,
 and then return Just i when n = -1 or we end on n > 0
the only case when  there is a balanced string is when we end on n == 0
-} 

-- 7
{-
- Which of these tools could you use in which circumstances? 
Trace made it clear were the bugs were, and the HUnit tests were quick to just see whether it was working or not, just trying inputs is quite similar to HUnit, but a bit less efficient
- What strategies for bug-finding did you use? 
testing boundary values, tracing
- How well do you imagine that these tools could be applied to larger programs? HUnit very well, 
-}

-- Nya tester varje gång något nytt kommer till

-- Problem 2
-- sort the input list into increasing order
qsort :: (Ord a, Show a) => [a] -> [a] -- (show a when tracing)
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ x : qsort [y | y <- xs, y > x]
-- qsort (x:xs) = trace ("r1:" ++ show r1 ++ "    r2:" ++ show r2) $ qsort [y | y <- xs, y < x] ++ qsort [y | y <- xs, y > x]
--                 where 
--                     r1 = qsort [y | y <- xs, y < x]
--                     r2 = qsort [y | y <- xs, y > x]

-- 1
t201 = [] 
t202 = [1,2,3,4] 
t203 = [5,2,7,(-34)]
t204 = [1,1,1]
-- t201-204 == []

-- 2
testP201 = TestCase $ assertEqual "qsort empty list" "" (qsort "") -- edge case
testP202 = TestCase $ assertEqual "qsort sorted list" [1,2,3] (qsort [1,2,3]) -- see if it is working
testP203 = TestCase $ assertEqual "qsort list w same elements" [1,1,1] (qsort [1,1,1]) -- edge case
testP204 = TestCase $ assertEqual "qsort unsorted list" [(-34),2,5,7] (qsort [5,2,7,(-34)]) --see if it is working

runP2tests = runTestTT $ TestList [TestLabel "qsort empty list" testP201, TestLabel "qsort sorted list" testP202, TestLabel "qsort list w same elements" testP203, TestLabel "qsort unsorted list" testP204]

{-
### Failure in: 1:qsort sorted list
Lab14.hs:154
qsort sorted list
expected: [1,2,3]
 but got: []
### Failure in: 2:qsort list w same elements
Lab14.hs:155
qsort list w same elements
expected: [1,1,1]
 but got: []
### Failure in: 3:qsort unsorted list
Lab14.hs:156
qsort unsorted list
expected: [-34,2,5,7]
 but got: []
Cases: 4  Tried: 4  Errors: 0  Failures: 3
Counts {cases = 4, tried = 4, errors = 0, failures = 3} 

-- only empty list works
-}

-- 3

{- 
qsort [1,1,1]
r1:[]    r2:[]
[]

qsort [1,2,3]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
[]

qsort [(-3),2,1]    
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
r1:[]    r2:[]
[]

-}

-- 6
{-
we never check when y == x, and we never add x
-}

-- 7
{-
-- Which of these tools could you use in which circumstances? 
-- What strategies for bug-finding did you use?
Review data structures - not all (no) data was sotored
-- How well do you imagine that these tools could be applied to larger programs?
-}