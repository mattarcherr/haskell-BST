module Dictionary (Dictionary, emptyDict, dictFromList, lookup, insert, printDict, removeAt) where
import Prelude hiding (lookup)

import Tree


type Key = Int
type Item = String

data Dictionary = EmptyDict | DictTree Tree

-- ############# INIT DICT #############

emptyDict :: Dictionary
emptyDict = EmptyDict

dictFromList :: [(Key,Item)] -> Dictionary
dictFromList [] = EmptyDict
dictFromList ((key,item):rest) = DictTree (insertTree key item (treeFromList rest) )

-- ############# LOOKUP ############# 

lookup :: Key -> Dictionary -> String
lookup _ EmptyDict = ""
lookup key (DictTree a) = lookupTree key a

-- ############# INSERT ############# 

insert :: Key -> Item -> Dictionary -> Dictionary
insert key item EmptyDict = DictTree (insertTree key item emptyTree)
insert key item (DictTree a) = DictTree (insertTree key item a)

-- ############# PRINT ############## 

formatData :: (Key, Item) -> String
formatData (key,item) = show key ++ "-" ++ item

printDict :: Dictionary -> IO()
printDict EmptyDict = print ""
printDict (DictTree a) = mapM_ (print . formatData) (orderedList a)

-- ############# REMOVE ############## 

removeAt :: Key -> Dictionary -> Dictionary
removeAt _ EmptyDict = emptyDict
removeAt key (DictTree a) = DictTree (removeAtTree key a)

removeIf :: (Int -> Bool) -> Dictionary -> Dictionary
removeIf _ EmptyDict = emptyDict
removeIf f (DictTree a) = DictTree (removeIfTree f a)


