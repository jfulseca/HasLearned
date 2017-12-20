import Conduit (runConduit)
import Data.Either (either)
import Data.Semigroup ((<>))
import School.App.CSVReader
import School.FileIO.AppIO (runAppIO)
import School.FileIO.FileHeader (FileHeader(..))
import School.Types.DataType (DataType(..))
import Options.Applicative

data Conversion = Conversion { inFile :: String
                             , outFile :: String
                             , nRows :: Int
                             , nCols :: Int }

conversion :: Parser Conversion
conversion = Conversion
  <$> strOption (long "in-file"
              <> short 'i'
              <> metavar "INPUT"
              <> help "Input file (csv)")
  <*> strOption (long "out-file"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output file (binary)")
  <*> option auto (long "rows"
                <> short 'r'
                <> metavar "ROWS"
                <> help "Number of rows")
  <*> option auto (long "columns"
                <> short 'c'
                <> metavar "COLUMNS"
                <> help "Number of columns")

description :: String
description = "Converts a csv file of numbers "
           ++ "to a row order double matrix "
           ++ "in a native format binary file"

title :: String
title = "CSVToBinary"

convert :: Conversion -> IO ()
convert args = do
  let matrixHeader = FileHeader DBL64B (nRows args) (nCols args)
  let conv = csvToBinary (inFile args)
                         (outFile args)
                         matrixHeader
  result <- runAppIO . runConduit $ conv
  either putStrLn return result

main :: IO ()
main = convert =<< execParser opts
  where opts = info (conversion <**> helper)
                    (fullDesc
                  <> progDesc description
                  <> header title)
