module Bugs where

type KernelVersion = String

-- A Kernel Bug Entry
data Entry = Entry { comment :: String,
     	     	     sql     :: String,
                     name    :: String,
                     table   :: [(Integer,Integer)]
                   }
  deriving (Eq,Ord,Read,Show)


-- Bugs
data WithOrWithout = With | Without
  deriving (Eq,Ord,Show,Read)

data BugSelect = BugSelect { bs_mode :: WithOrWithout ,
                             bs_list :: [String]
                           }
  deriving (Eq,Ord,Show,Read)
