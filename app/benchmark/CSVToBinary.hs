import Criterion.Main
import School.App.AppS (FullConduitAppS, runAppSConduitDefState)
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.App.CSVReader (csvToBinary)
import School.Types.DataType (DataType(..))
import School.Utils.SafeEncoding (safeStdEncodings)
import System.Directory (removeFile)
import System.FilePath (takeBaseName)

dataDir :: FilePath
dataDir = "app/benchmark/data/"

convert :: (  FilePath
           -> FilePath
           -> MatrixHeader
           -> FullConduitAppS Double)
        -> FilePath
        -> Int
        -> Int
        -> IO ()
convert readWrite fName nRows nCols = do
  let header = MatrixHeader DBL64B nRows nCols
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
      (convert csvToBinary "smallCSV.csv" 5 3)
  , bench " 100x401" $ nfIO
      (convert csvToBinary "digits_fst100.csv" 100 401)
    ]
  ]
