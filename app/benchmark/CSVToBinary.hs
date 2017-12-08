import Criterion.Main
import School.App.AppS (FullConduitAppS, runAppSConduitDefState)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.FileIO.CSVReader (csvToBinary)
import School.Types.PosInt (PosInt)
import School.Types.TypeName (TypeName(DBL))
import School.Utils.SafeEncoding (safeStdEncodings)
import System.Directory (removeFile)
import System.FilePath (takeBaseName)
import Test.QuickCheck.Modifiers (Positive(..))

dataDir :: FilePath
dataDir = "app/benchmark/data/"

convert :: (  FilePath
           -> FilePath
           -> MatrixHeader
           -> FullConduitAppS Double)
        -> FilePath
        -> PosInt
        -> PosInt
        -> IO ()
convert readWrite fName nRows nCols = do
  let header = MatrixHeader DBL nRows nCols
  let outFile = (takeBaseName fName) ++ ".dat"
  let conversion = readWrite (dataDir ++ fName)
                             outFile
                             header
  result <- runAppSConduitDefState conversion
  removeFile outFile
  either putStrLn
         (const $ return ())
         result

main :: IO ()
main = safeStdEncodings >> defaultMain [
  bgroup "csv to binary " [
    bench " 5x3" $ nfIO
      (convert csvToBinary "smallCSV.csv" (Positive 5) (Positive 3))
  , bench " 100x401" $ nfIO
      (convert csvToBinary "digits_fst100.csv" (Positive 100) (Positive 401))
    ]
  ]
