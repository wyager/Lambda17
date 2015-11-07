{-# LANGUAGE ScopedTypeVariables #-}
module CPU.Buffer (Buffer, empty, insert, insert', full, take, intStats) where

import CLaSH.Prelude hiding (drop, empty, take) 
import Text.Printf (printf)
import qualified Prelude as P


data Count n = Empty | Count (Index n) deriving (Eq)

instance KnownNat n => Enum (Count n) where
    succ Empty = Count 0
    succ (Count n) = Count (n+1)
    pred (Count 0) = Empty
    pred (Count n) = Count (n-1)
    toEnum 0 = Empty
    toEnum n = Count (toEnum $ n-1)
    fromEnum Empty = 0
    fromEnum (Count n) = 1 + fromEnum n

full :: KnownNat n => Buffer n a -> Bool
full buf@(Buffer count _) = count == maxCount buf

maxCount :: KnownNat n => Buffer n a -> Count n
maxCount _ = Count maxBound

count :: KnownNat n => Buffer n a -> Count n
count (Buffer c _) = c

-- Ring buffer
data Buffer n a = Buffer (Count n) (Vec n a)

instance (Show a, KnownNat n) => Show (Buffer n a) where
    show (Buffer count vec) = printf "[Buffer <%s>]" (format count (toList vec))
        where
        format Empty [] = ""
        format Empty [_] = "_"
        format Empty (x : xs) = "_, " P.++ format Empty xs
        format (Count 0) [x] = show x
        format count (x : xs) = show x P.++ ", " P.++ format (pred count) xs

intStats :: (KnownNat n) => Buffer n a -> (Int, Int)
intStats buf = (fromEnum (maxCount buf), fromEnum (count buf))

empty :: (KnownNat n) => Buffer n a
empty = Buffer Empty (repeat undefined)

-- Force insertion, no overflow check
insert' :: (KnownNat n) => Buffer n a -> a -> Buffer n a
insert' buf@(Buffer count vec) a | full buf  = error "Inserting into full buffer" 
                               a | otherwise = Buffer (succ count) (replace count a vec)

insert :: (KnownNat n) => Buffer n a -> a -> Buffer n a
insert buf@(Buffer count vec) a = if full buf
    then buf
    else Buffer (succ count) (replace count a vec)

insertMany :: (KnownNat n, KnownNat m) => Buffer n a -> Vec m (Maybe a) -> Buffer n a
insertMany buf vec = foldl insert' buf vec
    where 
    insert' buf Nothing  = buf
    insert' buf (Just a) = insert buf a

take :: (KnownNat (n+1)) => Buffer (n+1) a -> (Buffer (n+1) a, Maybe a)
take (Buffer Empty vec) = (Buffer Empty vec, Nothing)
take (Buffer count vec) = (Buffer (pred count) (vec <<+ undefined), Just (head vec))

drop :: (KnownNat n) => Buffer n a -> Buffer n a
drop (Buffer Empty vec) = Buffer Empty vec
drop (Buffer count vec) = Buffer (pred count) (vec <<+ undefined)

peek :: (KnownNat (n+1)) => Buffer (n+1) a -> Maybe a
peek (Buffer Empty vec) = Nothing
peek (Buffer count vec) = Just (head vec)

takeMany :: forall m n a . (KnownNat (n+1), KnownNat (m+1)) => Buffer (n+1) a -> (Buffer (n+1) a, Vec (m+1) (Maybe a))
takeMany buf = (drop (last drops), map peek drops)
    where drops = iterateI drop buf :: Vec (m+1) (Buffer (n+1) a)

