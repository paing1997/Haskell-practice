import Test.HUnit
import Test.LeanCheck
import Lab2


main :: IO ()
main = do
    runTestTT testList 
    return ()

testList = TestList [ TestLabel "insertToEmptyTree" test1,
             TestLabel "insertToRightSubtree" test2,
             TestLabel "insertToLeftSubtree" test3,
             TestLabel "deleteFromEmptyTree" test4,
             TestLabel "deleteFromNodeTree" test5,
             TestLabel "deleteFromNodeMismatch" test6,
             TestLabel "Eq test1" test7,
             TestLabel "Eq test2" test8,
             TestLabel "Eq test2" (TestCase (assertBool "insertTOtreePropertyTest" test9))
           ]


test1 = TestCase (assertEqual "insertToEmptyTree" (Node Empty 'a' Empty) (insertToTree Empty 'a'))
test2 = TestCase (assertEqual "insertToRightSubtree" (Node (Node Empty 'a' Empty) 'b' Empty) (insertToTree (Node Empty 'b' Empty) 'a'))
test3 = TestCase (assertEqual "insertToLeftSubtree" (Node Empty 'a' (Node Empty 'b' Empty)) (insertToTree (Node Empty 'a' Empty) 'b'))
test4 = TestCase (assertEqual "deleteFromEmptyTree" Empty (deleteFromTree Empty 'a'))
test5 = TestCase (assertEqual "deleteFromNodeTree" Empty (deleteFromTree (Node Empty 'a' Empty) 'a'))
test6 = TestCase (assertEqual "deleteFromNodeMismatch" (Node Empty 'a' Empty) (deleteFromTree (Node Empty 'a' Empty) 'b'))
test7 = TestCase (assertBool "Eq test1" (Node Empty 'a' Empty == Node Empty 'a' Empty))
test8 = TestCase (assertBool "Eq test2" (Node (Node Empty 'a' Empty) 'b' Empty == Node Empty 'a' (Node Empty 'b' Empty)))

test9 = holds 100 $ \x -> insertToTree (Empty::Tree Int) x == Node Empty x Empty
