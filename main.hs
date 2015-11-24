import Doodle
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

data Rec t = Rec{title :: String, ts :: Map Int (t, t), ps :: Map Int String } deriving Show

instance Doodle Rec where
    initialize x = myinit x
    add (t0, t1) d = myAddTs (t0, t1) d
    remove k d = myRemove k d
    toogle s k d = myToogle s k d

myinit s = Rec{title = s, ts = Map.empty, ps = Map.empty}
myAddTs (t0, t1) d = Rec{title = title d, ts = addHelper (t0, t1) (ts d), ps = ps d}
myRemove k d = Rec{title = title d, ts = Map.delete k (ts d), ps = ps d}

myToogle s k d = Rec{title = title d, ts = ts d,
                     ps = (Map.insert k s (ps d)) }

myFreshKey p
    | Map.null p == False = succ (fst $ Map.findMax p)
    | otherwise = 1

-- helper for above
addHelper (t0, t1) d = (Map.insert (myFreshKey d) (t0, t1) d)

data RecMap k v = RecMap (Map k v) k deriving (Show)

instance Pool RecMap  where
        freshKey (RecMap _ i) = succ i
        get k (RecMap p _)= Map.lookup k p
        set k d (RecMap p i) = mySet k d (RecMap p i)

mySet k v (RecMap d _) = RecMap (Map.insert k v d) k

main :: IO ()
main = do
          let w = RecMap Map.empty 0 :: RecMap Int (Rec String)
          run w
--           return ()
