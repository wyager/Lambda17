{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Fetch (FetchState, empty, fetch) where

import CLaSH.Prelude hiding (all, any, empty)
import CPU.Defs (MemRead(..), Read(..), Fetch(..), RIx)
import qualified CPU.InstructionSet as I
import CPU.Op (Op, Fetched, convert)
import CPU.OpBuffer (OpBuffer(fetch_pc), insertMany)
import Data.Maybe 

data FetchState = Fetching | NotFetching deriving Show

empty :: FetchState
empty = NotFetching

fetch :: forall n ob . (KnownNat n, KnownNat ob)
      => FetchState 
      -> OpBuffer ob 
      -> Vec n MemRead 
      -> (FetchState, OpBuffer ob, Vec n Fetch)
fetch NotFetching ib _ = (Fetching, ib, reqs)
    where
    reqs = map Fetch $ iterate (snat :: SNat n) (+1) $ fetch_pc ib
fetch Fetching ib reads
    | all (== NothingRead) reads = (Fetching, ib, repeat NoFetch)
    | any (== NothingRead) reads = error "Some fetch reads have completed, some haven't"
    | otherwise = (Fetching, ib', reqs)
    where
    bytes = map (\(ReadSome x) -> x) reads
    instrs = map parse' bytes
    parse' w = case I.parse w of
        Nothing -> error "Invalid opcode"
        Just op -> op
    fetched_pcs = iterate (snat :: SNat n) (+1) $ fetch_pc ib
    fetched_instrs = zipWith I.Fetched instrs fetched_pcs :: Vec n I.FetchedInstruction
    fetched_ops = map convert fetched_instrs :: Vec n (Fetched (Op RIx))
    ib' = insertMany fetched_ops ib
    reqs = map Fetch $ iterate (snat :: SNat n) (+1) $ fetch_pc ib'

all pred = foldr (&&) True  . map pred
any pred = foldr (||) False . map pred
