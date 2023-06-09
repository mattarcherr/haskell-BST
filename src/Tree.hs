{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tree (Tree,emptyTree,treeFromList,lookupTree,insertTree,orderedList,removeAtTree,removeIfTree,removeIfTreeTest) where
import Prelude hiding (lookup)

type Key = Int
type Item = String
type Left = Tree
type Right = Tree

data Tree = Leaf | Branch Key Item Left Right

-- ############# INIT TREE #############

emptyTree :: Tree
emptyTree = Leaf

treeFromList :: [(Key,Item)] -> Tree
treeFromList [] = Leaf
treeFromList ((key,item):rest) = insertTree key item (treeFromList rest) 

-- ############# LOOKUP ############# 

lookupTree :: Key -> Tree -> String
lookupTree soughtKey Leaf = ""
lookupTree soughtKey (Branch nodeKey nodeItem left right)
  | nodeKey == soughtKey = nodeItem
  | soughtKey > nodeKey = lookupTree soughtKey right
  | soughtKey < nodeKey = lookupTree soughtKey left

-- ############# INSERT ############# 

insertTree :: Key -> Item -> Tree -> Tree
insertTree key item Leaf = Branch key item Leaf Leaf
insertTree key item (Branch nodeKey nodeItem left right)
  | key == nodeKey = Branch key item left right
  | key < nodeKey = Branch nodeKey nodeItem (insertTree key item left) right
  | key > nodeKey = Branch nodeKey nodeItem left (insertTree key item right)

-- ############# PRINT ############## 

orderedList :: Tree -> [(Key, Item)]
orderedList Leaf = []
orderedList (Branch key item left right) = orderedList left ++ [(key,item)] ++ orderedList right

-- ############# REMOVE ############# 

removeAtTree :: Key -> Tree -> Tree
removeAtTree _ Leaf = Leaf
removeAtTree key (Branch nodeKey nodeItem left right)
  | key == nodeKey = removeTreeBranch (Branch nodeKey nodeItem left right)
  | key < nodeKey = Branch nodeKey nodeItem (removeAtTree key left) right
  | key > nodeKey = Branch nodeKey nodeItem left (removeAtTree key right)

removeIfTree :: (Int -> Bool) -> Tree -> Tree
removeIfTree _ Leaf = Leaf
removeIfTree f (Branch nodeKey nodeItem left right) = ( Branch nodeKey nodeItem left (removeIfTree f right) )
removeIfTree f (Branch nodeKey nodeItem Leaf right) = ( Branch nodeKey nodeItem Leaf (removeIfTree f right) )
removeIfTree f (Branch nodeKey nodeItem left Leaf) = ( Branch nodeKey nodeItem (removeIfTree f left) Leaf )
removeIfTree f (Branch nodeKey nodeItem Leaf Leaf)
  | f nodeKey = Leaf
  | otherwise = Branch nodeKey nodeItem Leaf Leaf


removeIfTreeTest :: (Int -> Bool) -> Tree -> IO()
removeIfTreeTest f (Branch nodeKey nodeItem left right) = print "one"
removeIfTreeTest f (Branch nodeKey nodeItem Leaf right) = print "two"
removeIfTreeTest f (Branch nodeKey nodeItem left Leaf) = print "three"
removeIfTreeTest f (Branch nodeKey nodeItem Leaf Leaf)
  | f nodeKey = print nodeKey
  | otherwise = print "false"

removeTreeBranch :: Tree -> Tree
removeTreeBranch (Branch key item Leaf Leaf) = Leaf
removeTreeBranch (Branch key item Leaf right) = right
removeTreeBranch (Branch key item left Leaf) = left
removeTreeBranch (Branch key item left right) = Branch nodeKey nodeItem left nodeRight
  where ((nodeKey,nodeItem),nodeRight) = leftmostNode left

leftmostNode :: Tree -> ((Key,Item),Tree)
leftmostNode (Branch key item Leaf right) = ((key,item),right)
leftmostNode (Branch key item left _) = leftmostNode left

-- ############# ROTATION ############# 

leftRotation :: Tree -> Tree
leftRotation Leaf = Leaf

rightRotation :: Tree -> Tree
rightRotation Leaf = Leaf

