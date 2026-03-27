import Clash.Prelude
import Sim.Testbed (loadHexFile, loadHexFile', withRAM)
import CPU.InstructionSet (parse)

main :: IO ()
main = do
    ws <- loadHexFile' "mem.hex"
    mapM_ (print . parse) ws
    memory <- loadHexFile "mem.hex"
    mapM_ putStrLn $ sampleN 10 $
        withClockResetEnable systemClockGen systemResetGen enableGen $
        withRAM memory