import Doodle
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T


-- data Rec f k p = Rec { title :: p, startTime :: f k } deriving Show

-- data SimpleRec a b = SimpleRec (a b) deriving Show

-- data MyEither a b = String a | Integer b
--
-- instance Functor (MyEither a) where
--     fmap f (Integer x) = Integer (f x)
--     fmap f (String x) = String x

-- instance Functor (SimpleRec a) where
--     fmap f (SimpleRec b) = SimpleRec (f b)
--
-- instance Functor (Rec a b) where
--     fmap f (Rec {title = x, startTime = y}) =
--         Rec {title = f x, startTime = y}
-- data Frank a b  = Frank {title :: a, } deriving (Show)

data Time t = Time t deriving Show
data Rec t = Rec{title :: String, ts :: [(t, t)]} deriving Show

instance Doodle Rec where
    initialize x = myinit x
    add (t0, t1) d = myAdd (t0, t1) d

-- myinit :: k -> Rec d
myinit x = Rec{title = x, ts = []}
myAdd (t0, t1) d = Rec{title = title d, ts = (t0, t1):ts d}

instance Pool Map where
        freshKey p = succ ( last $ Map.keys p )
        get k p = Map.lookup k p
        set k p = Map.insert k p

myMap n = Map.fromList (map makePair [1..n])
    where makePair x = (x, [x])

main :: IO ()
main = do
        let m = myMap 5
        let f = freshKey m
        print f
