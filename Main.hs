import CLaSH.Prelude
import Sim.Testbed (loadHexFile, loadHexFile', withRAM)
import CPU.InstructionSet (parse)
main = do
    words <- loadHexFile' "mem.hex"
    mapM (print . parse) words
    memory <- loadHexFile "mem.hex"
    mapM putStrLn $ sampleN 10 (withRAM memory)