module Main (main) where
import Prelude hiding (lookup)

import Dictionary

import Data.Bits

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

isEven :: Int -> Bool
isEven a = complement (testBit a 0)

main :: IO()
main = printDict ( removeIf isEven insertTestData)

--main :: IO()
--main = printDict insertTestData 
