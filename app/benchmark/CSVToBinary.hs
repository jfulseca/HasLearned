import Conduit (ConduitM, runConduit)
import Criterion.Main
import Data.Void (Void)
import School.FileIO.AppIO (AppIO, runAppIO)
import School.FileIO.FileHeader (FileHeader(..))
import School.App.CSVReader (csvToBinary)
import School.Types.DataType (DataType(..))
import School.Utils.SafeEncoding (safeStdEncodings)
import System.Directory (removeFile)
import System.FilePath (takeBaseName)

dataDir :: FilePath
dataDir = "app/benchmark/data/"

convert :: (  FilePath
           -> FilePath
           -> FileHeader
           -> ConduitM () Void AppIO ())
        -> FilePath
        -> Int
        -> Int
        -> IO ()
convert readWrite fName nRows nCols = do
  let header = FileHeader DBL64B nRows nCols
  let outFile = (takeBaseName fName) ++ ".dat"
  let conversion = readWrite (dataDir ++ fName)
                             outFile
                             header
  result <- runAppIO . runConduit $ conversion
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
