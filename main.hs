import Doodle
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.PrettyPrint
import Data.List


data Ts t = Ts {st :: t , et:: t , ps :: [String]}  deriving Show
data Rec t = Rec{title :: String, tsMap :: Map Int (Ts t)}

instance Doodle Rec where
    initialize x = myinit x
    add (t0, t1) d = myAddTsWrapper (t0, t1) d
    remove k d = myRemove k d
    toogle s k d = myToogle s k d

instance Show (Rec t) where
  show r
    | Map.null (tsMap r) == False = printRec r
    -- TODO: dirty hack not working
    | otherwise = "\&"

printRec r = "+------------------------------------------------------+\n"
             ++ "| " ++ (title r) ++ "| \n" ++
             "+------------------------------------------------------+"
            ++ map (\v -> et v) ( tsMap r)

myinit s = Rec{title = s, tsMap = Map.empty}
myAddTsWrapper ts d = Rec{title = title d,
                        tsMap = myInsertTs ts (tsMap d)}
myInsertTs (t0, t1) d = (Map.insert (myFreshKey d)
                        Ts{st = t0, et = t1, ps =[] } d)

myRemove k d = Rec{title = title d, tsMap = Map.delete k (tsMap d)}

myToogle v k d = Rec{title = title d, tsMap = addPs k v (tsMap d) }

addPs k v tsm = Map.update(
                    \x -> Just (Ts{st = st x, et = et x, ps = v : (ps x)})
                 ) k tsm

myFreshKey p
    | Map.null p == False = succ (fst $ Map.findMax p)
    | otherwise = 1

-- helper for above

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
--
---- a type for records
data T = T { make  :: String
           , model :: String
           , years :: [Int] }
    deriving Show

-- test data
test =
    [ T "foo" "avengersadsaddsa" [1990, 1992]
    , T "bar" "eagle"   [1980, 1982]
    ]

-- print lists of records: a header, then each row
draw :: [T] -> Doc
draw xs =
    text "Make\t|\tModel\t|\tYear"
   $+$
    vcat (map row xs)
 where
    -- print a row
    row t = foldl1 (<|>) [ text (make t)
                         , text (model t)
                         , foldl1 (<^>) (map int (years t))
                         ]

-- helpers
x <|> y = x <> text "\t|\t" <> y
x <^> y = x <> text "," <+> y
