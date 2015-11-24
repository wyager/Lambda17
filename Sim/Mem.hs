{-# LANGUAGE ScopedTypeVariables #-}
module Sim.Mem (mem) where

import CLaSH.Prelude hiding (Read)

import CPU.Defs (W(..), Read(..), Fetch(..), MemRead(..), Addr(..), PC(..))



mem :: forall f l . (KnownNat l, KnownNat f)
    => Int -> Int -> Vec 65536 W -> Signal (Vec l Read) -> Signal (Vec f Fetch) -> Signal (Vec l MemRead, Vec f MemRead)
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
    

fetcher :: forall ix n . (Enum ix, KnownNat n)
        => Int -> Vec 65536 W -> Signal (Vec n (Maybe ix)) -> Signal (Vec n MemRead)
fetcher speed bytes reqs = reads
    where
    ptrs = register (repeat $ error "undefined load/fetch pointer") ptrs' :: Signal (Vec n ix)
    ctrs = register (repeat 0)                                      ctrs' :: Signal (Vec n Int)
    datas  = map (bytes !!) <$> ptrs      :: Signal (Vec n W)
    readys = map (==1) <$> ctrs                                           :: Signal (Vec n Bool)
    reads = zipWith read <$> readys <*> datas                             :: Signal (Vec n MemRead)
    read :: Bool -> W -> MemRead
    read True bytes = ReadSome bytes
    read False _    = NothingRead
    ptrs' =  (\reqs ptrs ctrs -> update_ptr <$> reqs <*> ptrs <*> ctrs) <$> reqs <*> ptrs <*> ctrs :: Signal (Vec n ix)
    update_ptr :: Maybe ix -> ix -> Int -> ix
    update_ptr Nothing ptr ctr = if ctr > 0 then ptr else error "Expired load/fetch pointer" 
    update_ptr (Just pc) _ _ = pc
    ctrs' = (\reqs ctrs -> update_ctr <$> reqs <*> ctrs) <$> reqs <*> ctrs :: Signal (Vec n Int)
    update_ctr Nothing ctr = if ctr > 0 then ctr - 1 else ctr
    update_ctr (Just _) ctr = speed
