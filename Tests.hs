{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Unit tests for the Lambda17 Tomasulo OOO CPU.
--
-- These tests exercise pure combinational logic and small-scale
-- sequential behaviour. They run under plain GHC — no Verilog
-- generation required. Invoke via:
--
--   ghc -isrc Tests.hs -e main
--
-- or load in ghci and call @main@.
module Main (main, tests) where

import Clash.Prelude hiding (empty, take)
import qualified Prelude as P
import qualified Data.List as L

import CPU.Defs (W(..), PC(..), RIx(..), Addr(..), RVal(..), RobID(..),
                 Predicted(..), StationID(..))
import CPU.InstructionSet (Instruction(..), parse)
import qualified CPU.InstructionSet as I
import CPU.Op (Op(..), Fetched(..), grounded)
import qualified CPU.Op as Op
import CPU.Buffer (Buffer(..), Count(..))
import qualified CPU.Buffer as Buf
import CPU.ReorderBuffer (ROB(..), Waiting(..))
import qualified CPU.ReorderBuffer as ROB
import CPU.RStations (RStations(..), RSEntry(..))
import qualified CPU.RStations as RS
import CPU.RegisterFile (RegisterFile(..))
import qualified CPU.RegisterFile as RF
import CPU.BackupRegs (BackupRegs(..))
import qualified CPU.BackupRegs as BR
import CPU.Dispatch (DispatchState(..))
import qualified CPU.Dispatch as D
import CPU.OpBuffer (OpBuffer)
import qualified CPU.OpBuffer as OB
import qualified CPU.FunctionalUnits as FU

-- Test harness ---------------------------------------------------------------

data Result = Pass | Fail String deriving (Eq)

expect :: (Eq a, Show a) => String -> a -> a -> (String, Result)
expect name actual expected
    | actual == expected = (name, Pass)
    | otherwise = (name, Fail msg)
  where
    msg = "expected " P.++ show expected P.++ " but got " P.++ show actual

expectTrue :: String -> Bool -> (String, Result)
expectTrue name b = expect name b True

tests :: [(String, Result)]
tests = decoderTests P.++ bufferTests P.++ robTests P.++ rsTests
     P.++ ldrRegressionTests

report :: [(String, Result)] -> IO ()
report results = do
    P.mapM_ printLine results
    let failures = [msg | (_, Fail msg) <- results]
    P.putStrLn ""
    P.putStrLn $ show (P.length results - P.length failures)
              P.++ "/" P.++ show (P.length results) P.++ " passed"
    if P.null failures
       then P.putStrLn "ALL TESTS PASSED"
       else P.error "TESTS FAILED"
  where
    printLine (name, Pass)    = P.putStrLn $ "PASS  " P.++ name
    printLine (name, Fail m)  = P.putStrLn $ "FAIL  " P.++ name P.++ ": " P.++ m

main :: IO ()
main = report tests

-- Decoder tests --------------------------------------------------------------
-- Encoding: [15:12]=opcode, [11:8]=rA/imm-hi, [7:4]=rB/imm-lo, [3:0]=rT
-- Opcodes: 0=Mov 1=Add 2=Jmp 3=Halt 4=Ld 5=Ldr 6=Jeq

decoderTests :: [(String, Result)]
decoderTests =
    [ expect "decode Mov"  (parse (W 0x0ABC)) (Just (I.Mov (W 0xAB) (RIx 0xC)))
    , expect "decode Add"  (parse (W 0x1123)) (Just (I.Add (RIx 1) (RIx 2) (RIx 3)))
    , expect "decode Jmp"  (parse (W 0x2ABC)) (Just (I.Jmp (PC 0xABC)))
    , expect "decode Halt" (parse (W 0x3000)) (Just I.Halt)
    , expect "decode Ld"   (parse (W 0x4ABC)) (Just (I.Ld (Addr 0xAB) (RIx 0xC)))
    , expect "decode Ldr"  (parse (W 0x5456)) (Just (I.Ldr (RIx 4) (RIx 5) (RIx 6)))
    , expect "decode Jeq"  (parse (W 0x6125)) (Just (I.Jeq (RIx 1) (RIx 2) (PC 5)))
    , expect "decode invalid" (parse (W 0x8000)) Nothing
    ]

-- Buffer tests ---------------------------------------------------------------

type IntBuf = Buffer 4 Int

bufferTests :: [(String, Result)]
bufferTests =
    [ expectTrue "empty buffer not full" (not (Buf.full (Buf.empty :: IntBuf)))
    , expectTrue "fill and full" $
        let b = Buf.insert' (Buf.insert' (Buf.insert' (Buf.insert' (Buf.empty :: IntBuf) 1) 2) 3) 4
        in Buf.full b
    , expectTrue "insert then take FIFO" $
        let b1 = Buf.insert' (Buf.empty :: IntBuf) 10
            b2 = Buf.insert' b1 20
            (b3, x) = Buf.take b2
            (_,  y) = Buf.take b3
        in x == Just 10 && y == Just 20
    , expectTrue "take from empty" $
        let (_, r) = Buf.take (Buf.empty :: IntBuf) in r == Nothing
    ]

-- ROB tests ------------------------------------------------------------------

type TestROB = ROB 8

robTests :: [(String, Result)]
robTests =
    [ expectTrue "rob oneFree when empty"    (ROB.oneFree (ROB.empty :: TestROB))
    , expectTrue "rob twoFree when empty"    (ROB.twoFree (ROB.empty :: TestROB))
    , expectTrue "robInsert returns slot 0"  $
        let (rid, _) = ROB.robInsert nopF (ROB.empty :: TestROB)
        in rid == RobID 0
    , expectTrue "robInsert increments" $
        let (_, r1)  = ROB.robInsert nopF (ROB.empty :: TestROB)
            (rid, _) = ROB.robInsert nopF r1
        in rid == RobID 1
    , expectTrue "robPop waiting returns Nothing" $
        let (_, r1) = ROB.robInsert nopF (ROB.empty :: TestROB)
            (res, _) = ROB.robPop r1
        in res == Nothing
    , expectTrue "rob not twoFree when almost full" $
        let r7 = P.foldr (\_ r -> P.snd (ROB.robInsert nopF r)) (ROB.empty :: TestROB)
                         ([1..7] :: [Int])
        in ROB.oneFree r7 && not (ROB.twoFree r7)
    ]
  where
    nopF :: Fetched (Op (RobID 8))
    nopF = Fetched (PC 0) (Predicted (PC 1)) Nop

-- Reservation station tests --------------------------------------------------

type TestRS = RStations 3 4 8

rsTests :: [(String, Result)]
rsTests =
    [ expectTrue "RS freeSlot on empty"
        (RS.freeSlot (RS.empty :: TestRS) 0 == Just 0)
    , expectTrue "RS insert fills slot" $
        let rs' = RS.insert (StationID 0 0) entry (RS.empty :: TestRS)
        in RS.freeSlot rs' 0 == Just 1
    , expectTrue "RS insert in different FU" $
        let rs' = RS.insert (StationID 1 0) entry (RS.empty :: TestRS)
        in RS.freeSlot rs' 0 == Just 0
    ]
  where
    entry = RSEntry (Nop :: Op (RobID 8)) (RobID 0)

-- Ldr / r0-clobber regression tests ------------------------------------------
--
-- The Ldr instruction decomposes into a virtual Add (computes the address)
-- and a dependent Ld. The virtual Add must not clobber any architectural
-- register when it commits.

type TestDS = DispatchState 4 3 4 8

dispatchTest :: TestDS -> Either TestDS TestDS
dispatchTest = D.dispatch FU.select

ldrRegressionTests :: [(String, Result)]
ldrRegressionTests =
    [ expectTrue "Ldr dispatch puts Nop in ROB" $
        -- After dispatching an Ldr, the first ROB entry (virtual Add slot)
        -- should hold a Nop, not an Add targeting r0.
        case dispatchTest dsLdr of
          Right ds' ->
            let ROB.ROB b _ _ = rob ds'
                (_, first) = Buf.take b
            in case first of
                 Just (Waiting (Fetched _ _ Nop)) -> True
                 _                                -> False
          Left _ -> False

    , expectTrue "Ldr dispatch puts Add in RS" $
        -- The reservation station must still hold a real Add so the
        -- functional unit computes and broadcasts the address.
        case dispatchTest dsLdr of
          Right ds' ->
            let RStations vec = stations ds'
                fpuCol = vec !! (FU.select fakeAddProbe)
            in P.any isAddEntry (toList fpuCol)
          Left _ -> False

    , expectTrue "Ldr dispatch renames only target reg" $
        -- Dispatching Ldr r1 r2 r3 should rename r3, not r0.
        case dispatchTest dsLdr of
          Right ds' ->
            let RegFile v = regFile ds'
                r0 = v !! (0 :: Index 16)
                r3 = v !! (3 :: Index 16)
            in isLiteral r0 && isPending r3
          Left _ -> False

    , expectTrue "Nop is grounded" (grounded (Nop :: Op RIx))
    ]
  where
    -- Initial dispatch state with an Ldr r1 r2 -> r3 in the op buffer
    ldrOp   = Op.Ldr (Pending (RIx 1)) (Pending (RIx 2)) (RIx 3) :: Op RIx
    ldrF    = Fetched (PC 0) (Predicted (PC 1)) ldrOp
    dsLdr   = (D.empty :: TestDS) { opBuffer = OB.insert (OB.empty :: OpBuffer 4) ldrF }

    -- A probe Add just for asking select which FU column Adds go to
    fakeAddProbe = Op.Add (Literal (W 0)) (Literal (W 0)) (RIx 0) :: Op (RobID 8)

    isAddEntry (Just (RSEntry (Op.Add _ _ _) _)) = True
    isAddEntry _                                 = False

    isLiteral (Literal _) = True
    isLiteral _           = False
    isPending (Pending _) = True
    isPending _           = False
