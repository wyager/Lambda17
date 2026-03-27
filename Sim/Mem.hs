{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Sim.Mem (mem) where

import Clash.Prelude hiding (Read)

import CPU.Defs (S, Clk, W(..), Read(..), Fetch(..), MemRead(..), Addr(..), PC(..))



mem :: forall f l . (Clk, KnownNat l, KnownNat f)
    => Int -> Int -> Vec 65536 W -> S (Vec l Read) -> S (Vec f Fetch) -> S (Vec l MemRead, Vec f MemRead)
mem load_speed fetch_speed bytes loads fetches = bundle (loads', fetches')
    where
    fetches' = fetcher fetch_speed bytes (map fetch2Maybe <$> fetches)
    loads' = fetcher load_speed bytes (map load2Maybe <$> loads)
    load2Maybe :: Read -> Maybe Addr
    load2Maybe NoRead = Nothing
    load2Maybe (Read addr) = Just addr
    fetch2Maybe :: Fetch -> Maybe PC
    fetch2Maybe NoFetch = Nothing
    fetch2Maybe (Fetch pc) = Just pc


fetcher :: forall ix n . (Clk, Enum ix, NFDataX ix, KnownNat n)
        => Int -> Vec 65536 W -> S (Vec n (Maybe ix)) -> S (Vec n MemRead)
fetcher speed bytes reqs = reads
    where
    ptrs = register (repeat $ errorX "undefined load/fetch pointer") ptrs' :: S (Vec n ix)
    ctrs = register (repeat 0)                                       ctrs' :: S (Vec n Int)
    datas  = map (bytes !!) <$> ptrs      :: S (Vec n W)
    readys = map (==1) <$> ctrs           :: S (Vec n Bool)
    reads = zipWith read <$> readys <*> datas :: S (Vec n MemRead)
    read :: Bool -> W -> MemRead
    read True bytes = ReadSome bytes
    read False _    = NothingRead
    ptrs' = (\reqs ptrs ctrs -> update_ptr <$> reqs <*> ptrs <*> ctrs) <$> reqs <*> ptrs <*> ctrs :: S (Vec n ix)
    update_ptr :: Maybe ix -> ix -> Int -> ix
    update_ptr Nothing ptr ctr = if ctr > 0 then ptr else errorX "Expired load/fetch pointer"
    update_ptr (Just pc) _ _ = pc
    ctrs' = (\reqs ctrs -> update_ctr <$> reqs <*> ctrs) <$> reqs <*> ctrs :: S (Vec n Int)
    update_ctr Nothing ctr = if ctr > 0 then ctr - 1 else ctr
    update_ctr (Just _) ctr = speed
