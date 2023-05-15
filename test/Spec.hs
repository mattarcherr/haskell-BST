{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.QuickCheck (Property, orderedList)
import Test.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit hiding (assertString, assert, assertEqual, Assertion)


import Tree
import Dictionary

import Prelude hiding (lookup)
import qualified Tree
import Data.String (String)
import Tree (treeFromList)
import Dictionary (dictFromList, emptyDict)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All tests"
 [
    initTests,
    insertTests,
    lookupTests,
    removeAtTests,
    orderedListTests
 ]

insertTestData :: Dictionary
insertTestData = dictFromList
  [
     (22,"Jane"),
      (22,"Mary"),
      (0,"Harold"),
      (9,"Edward"),
      (37,"Victoria"),
      (4,"Matilda"),
      (26,"Oliver"),
      (42,"Elizabeth"),
      (19,"Henry"),
      (4,"Stephen"),
      (24,"James"),
      (-1,"Edward"),
      (31,"Anne"),
      (23,"Elizabeth"),
      (1,"William"),
      (26,"Charles")
   ]

-- ************* INIT**************

initTests :: TestTree
initTests = testGroup "Initilisation tests"
 [
   testCase "Initilise an empty tree" hunit_initTree_emptyTree,
   testCase "Initilise a tree with a single element" hunit_initTree_singleEle,
   testCase "Initlise a tree with many elements" hunit_initTree_manyEle,
   testProperty "An empty tree will always fail a lookup call" prop_initTree_emptyTree,
   testProperty "Initlising a tree with 1 element will increase it's length by 1" prop_initTree_incrementLength

   -- testCase "Initilise an empty dict"
   -- testProperty "Initlise a tree with many elements"
 ]

-- TREE FUNCTIONS
hunit_initTree_emptyTree :: Assertion
hunit_initTree_emptyTree =
   let
      tree = emptyTree
   in
     assertEqual "" (Tree.orderedList tree) []

prop_initTree_emptyTree :: Int -> Property
prop_initTree_emptyTree key =
   let
      tree = emptyTree
   in
     property (lookupTree key tree === "")

hunit_initTree_singleEle :: Assertion
hunit_initTree_singleEle =
   let
      tree = treeFromList [(0,"Matt")]
   in
     assertEqual "" (Tree.orderedList tree) [(0,"Matt")]

hunit_initTree_manyEle :: Assertion
hunit_initTree_manyEle =
   let
      tree = treeFromList [
         (22,"Jane"),(22,"Mary"),
         (0,"Harold"),(9,"Edward"),
         (37,"Victoria"),(4,"Matilda"),
         (26,"Oliver"),(42,"Elizabeth"),
         (19,"Henry"),(4,"Stephen"),
         (24,"James"),(-1,"Edward"),
         (31,"Anne"),(23,"Elizabeth"),
         (1,"William"),(26,"Charles")]
   in
     assertEqual "" (Tree.orderedList tree) [
      (-1,"Edward"),(0,"Harold"),
      (1,"William"),(4,"Matilda"),
      (9,"Edward"),(19,"Henry"),
      (22,"Jane"),(23,"Elizabeth"),
      (24,"James"),(26,"Oliver"),
      (31,"Anne"),(37,"Victoria"),
      (42,"Elizabeth")]

prop_initTree_incrementLength :: (Int,String) -> Property
prop_initTree_incrementLength a =
   -- (head a /= (9999,(snd (head a)))) ==>
   let
      lengthA = length (Tree.orderedList (treeFromList [a]))
   in
      property (lengthA === 1)


-- DICT FUNCTIONS
-- hunit_initDict_emptyTree :: Assertion
-- hunit_initDict_emptyTree =
--    let
--       dict = emptyTree
--    in
--      assertEqual "" (Tree.orderedList dict) []

-- prop_initDict_emptyDict :: Int -> Property
-- prop_initDict_emptyDict key =
--    let
--       dict = emptyDict
--    in
--      property (lookup key tree === "")

-- hunit_initDict_singleEle :: Assertion
-- hunit_initDict_singleEle =
--    let
--       tree = treeFromList [(0,"Matt")]
--    in
--      assertEqual "" (Tree.orderedList tree) [(0,"Matt")]

-- hunit_initDict_manyEle :: Assertion
-- hunit_initDict_manyEle =
--    let
--       tree = treeFromList [
--          (22,"Jane"),(22,"Mary"),
--          (0,"Harold"),(9,"Edward"),
--          (37,"Victoria"),(4,"Matilda"),
--          (26,"Oliver"),(42,"Elizabeth"),
--          (19,"Henry"),(4,"Stephen"),
--          (24,"James"),(-1,"Edward"),
--          (31,"Anne"),(23,"Elizabeth"),
--          (1,"William"),(26,"Charles")]
--    in
--      assertEqual "" (Tree.orderedList tree) [
--       (-1,"Edward"),(0,"Harold"),
--       (1,"William"),(4,"Matilda"),
--       (9,"Edward"),(19,"Henry"),
--       (22,"Jane"),(23,"Elizabeth"),
--       (24,"James"),(26,"Oliver"),
--       (31,"Anne"),(37,"Victoria"),
--       (42,"Elizabeth")]

-- prop_initDict_incrementLength :: (Int,String) -> Property
-- prop_initDict_incrementLength a =
--    -- (head a /= (9999,(snd (head a)))) ==>
--    let
--       lengthA = length (Tree.orderedList (treeFromList [a]))
--    in
--       property (lengthA === 1)

-- ************* INSERT**************

insertTests :: TestTree
insertTests = testGroup "Insert tests"
 [
   testCase "Insert new single item" hunit_insert_newInsertion,
   testCase "Insert existing single item" hunit_insert_newInsertion,
   testCase "Insert single item with a negative key" hunit_insert_negativeKeyInsertion,
   testCase "Insert single entry with empty item field" hunit_insert_emptyItemInsertion,
   testCase "Insert a single entry into an empty dictionary" hunit_insert_emptyDictInsertion,
   testCase "Insert multiple of the same item" hunit_insert_multipleSame,
   testProperty "An insertion of a new item should increase the length of the tree by one" prop_insert_oneInsertionIncrementLength
   -- testProperty "An insertion of many new items should increase the length of the tree by the correct amount" prop_insert_manyInsertionIncrementLength
 ]

hunit_insert_newInsertion :: Assertion
hunit_insert_newInsertion =
   let
      dict = insert 100 "Matt" insertTestData
   in
     assertEqual "" (lookup 100 dict) "Matt"

hunit_insert_existingInsertion :: Assertion
hunit_insert_existingInsertion =
   let
      dict = insert 31 "Matt" insertTestData
   in
     assertEqual "" ((lookup 31 dict) == "Matt" && (lookup 31 insertTestData) == "Anne") True

hunit_insert_negativeKeyInsertion :: Assertion
hunit_insert_negativeKeyInsertion =
   let
      dict = insert (-100) "Matt" insertTestData
   in
     assertEqual "" (lookup (-100) dict) "Matt"

hunit_insert_emptyItemInsertion :: Assertion
hunit_insert_emptyItemInsertion =
   let
      dict = insert 100 "" insertTestData
   in
     assertEqual "" (lookup 100 dict) ""

hunit_insert_emptyDictInsertion :: Assertion
hunit_insert_emptyDictInsertion =
   let
      dict = insert 100 "Matt" emptyDict
   in
     assertEqual "" (lookup 100 dict) "Matt"

hunit_insert_multipleSame :: Assertion
hunit_insert_multipleSame =
   let
      dict = insert 100 "Matt" ( insert 100 "Matt" (insert 100 "Matt" emptyDict))
   in
     assertEqual "" (lookup 100 dict) "Matt"

prop_insert_oneInsertionIncrementLength :: [(Int,String)] -> Property
prop_insert_oneInsertionIncrementLength a =
   -- (head a /= (9999,(snd (head a)))) ==>
   let
      tree = treeFromList a
      before = length (Tree.orderedList tree)
      after = length (Tree.orderedList (insertTree 9999 "Matt" tree))
   in
      property ((before+1) === after)

-- prop_insert_manyInsertionIncrementLength :: [(Int,String)] -> [(Int,String)] -> Property
-- prop_insert_manyInsertionIncrementLength a b =
--    (head a /= head b) ==>
--    let
--       tree = treeFromList a
--       lengthB = length b
--       before = length (Tree.orderedList tree)
--       after = length (Tree.orderedList (treeFromList (a ++ b)))
--    in
--       property ((before+lengthB) === after)


-- ************* LOOKUP **************

lookupTests :: TestTree
lookupTests = testGroup "Lookup tests"
 [
   testCase "Looking up a key that exists in a dictionary will return the item" hunit_lookup_normalLookup,
   testCase "Looking up a negative key that exists in a dictionary will return the item" hunit_lookup_negativeLookup,
   testCase "Looking up a large key" hunit_lookup_largeKeyLookup,
   testCase "Looking up a key of 0" hunit_lookup_zeroKeyLookup,
   testProperty "An empty dictionary should always fail a lookup" prop_lookup_emptyDict
 ]

hunit_lookup_normalLookup :: Assertion
hunit_lookup_normalLookup =
   let
      dict = insertTestData
   in
      assertEqual "" (lookup 4 dict) "Matilda"

hunit_lookup_negativeLookup :: Assertion
hunit_lookup_negativeLookup =
   let
      dict = insertTestData
   in
      assertEqual "" (lookup (-1) dict) "Edward"

hunit_lookup_largeKeyLookup :: Assertion
hunit_lookup_largeKeyLookup =
   let
      dict = insertTestData
   in
      assertEqual "" (lookup 999999999999 dict) ""

hunit_lookup_zeroKeyLookup :: Assertion
hunit_lookup_zeroKeyLookup =
   let
      dict = insertTestData
   in
      assertEqual "" (lookup 0 dict) "Harold"


prop_lookup_emptyDict :: Int -> Property
prop_lookup_emptyDict key =
   let
      dict = emptyDict
   in
     property (lookup key dict === "")

-- ************* REMOVE AT **************

removeAtTests :: TestTree
removeAtTests = testGroup "removeAt tests"
 [
   testCase "Removing from an empty dictionary" hunit_removeAt_removeEmptyDict,
   testCase "Removing from a tree with one node" hunit_removeAt_removeChildlessRoot,
   testCase "Removing from a tree with one left branch" hunit_removeAt_removeLeftBranch,
   testCase "Removing from a tree with one right branch" hunit_removeAt_removeRightBranch,
   testCase "Removing a node with children" hunit_removeAt_removeNodeWithChildren,
   testProperty "Removing from an empty dictionary" prop_removeAt_EmptyDict
 ]

hunit_removeAt_removeEmptyDict :: Assertion
hunit_removeAt_removeEmptyDict =
   let 
      dict = removeAt 15 emptyDict
   in
     assertEqual "" (lookup 15 dict) ""

hunit_removeAt_removeChildlessRoot :: Assertion
hunit_removeAt_removeChildlessRoot =
   let 
      dictA = insert 15 "John" emptyDict
      dict = removeAt 15 dictA
   in
     assertEqual "" (lookup 15 dict) ""

hunit_removeAt_removeLeftBranch :: Assertion
hunit_removeAt_removeLeftBranch =
   let 
      dictA = dictFromList [ (5,"Jane"),(10,"Mary")]
      dict = removeAt 5 dictA
   in
     assertEqual "" (lookup 5 dict == "" && lookup 10 dict == "Mary") True

hunit_removeAt_removeRightBranch :: Assertion
hunit_removeAt_removeRightBranch =
   let 
      dictA = dictFromList [ (15,"Jane"),(10,"Mary")]
      dict = removeAt 15 dictA
   in
    assertEqual "" (lookup 15 dict == "" && lookup 10 dict == "Mary") True

hunit_removeAt_removeChildlessNode :: Assertion
hunit_removeAt_removeChildlessNode =
   let 
      dictA = insertTestData
      dict = removeAt 37 dict
   in
     assertEqual "" (lookup 37 dict) ""

hunit_removeAt_removeNodeWithChildren :: Assertion
hunit_removeAt_removeNodeWithChildren =
   let 
      dictA = insertTestData
      dict = removeAt 23 dictA
   in
     assertEqual "" (lookup 23 dict == "" && lookup 9 dict == "Edward") True

prop_removeAt_EmptyDict :: Int -> Property
prop_removeAt_EmptyDict key =
   let 
      dict = removeAt key emptyDict
   in
     property (lookup key dict === "")

-- ************* Ordered List **************

orderedListTests :: TestTree
orderedListTests = testGroup "orderedList tests"
 [
   testCase "Ordered list of an empty tree is empty" hunit_orderedList_listEmptyTree,
   testCase "Ordered list of single element returns that element" hunit_orderedList_listSingleEle,
   testCase "Ordered list is independant of item name" hunit_orderedList_multipleEleSameName,
   testCase "Ordered list of a full tree returns the ordered elements" hunit_orderedList_listFullTree
 ]

hunit_orderedList_listEmptyTree :: Assertion
hunit_orderedList_listEmptyTree =
   let 
      dict = emptyTree
   in
     assertEqual "" (Tree.orderedList dict) []

hunit_orderedList_listSingleEle :: Assertion
hunit_orderedList_listSingleEle =
   let 
      dict = insertTree 0 "Matt" emptyTree
   in
     assertEqual "" (Tree.orderedList dict) [(0,"Matt")]

hunit_orderedList_multipleEleSameName :: Assertion
hunit_orderedList_multipleEleSameName =
   let 
      dict = treeFromList [(13,"Matt"),(1,"Matt"),(8,"Matt"),(5,"Matt"),(3,"Matt"),(2,"Matt")]
   in
     assertEqual "" (Tree.orderedList dict) [(1,"Matt"),(2,"Matt"),(3,"Matt"),(5,"Matt"),(8,"Matt"),(13,"Matt")]

hunit_orderedList_listFullTree :: Assertion
hunit_orderedList_listFullTree =
   let 
      dict = treeFromList [
         (22,"Jane"),(22,"Mary"),
         (0,"Harold"),(9,"Edward"),
         (37,"Victoria"),(4,"Matilda"),
         (26,"Oliver"),(42,"Elizabeth"),
         (19,"Henry"),(4,"Stephen"),
         (24,"James"),(-1,"Edward"),
         (31,"Anne"),(23,"Elizabeth"),
         (1,"William"),(26,"Charles")]
   in
     assertEqual "" (Tree.orderedList dict) [
         (-1,"Edward"),(0,"Harold"),
         (1,"William"),(4,"Matilda"),
         (9,"Edward"),(19,"Henry"),
         (22,"Jane"),(23,"Elizabeth"),
         (24,"James"),(26,"Oliver"),
         (31,"Anne"),(37,"Victoria"),
         (42,"Elizabeth")
     ]