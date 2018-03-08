import Distribution.Simple
import System.Process

main = do
  runCommand "make"
  defaultMain
