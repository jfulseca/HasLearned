import Data.Either (either)
import Data.Semigroup ((<>))
import School.FileIO.AppIO (runAppIO)
import School.FileIO.CSVReader
import School.FileIO.MatrixHeader (MatrixHeader(..))
import School.Types.PosInt (extractPosInts)
import School.Types.TypeName (TypeName(..))
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

failPosInt :: IO ()
failPosInt =
  putStrLn $ "ERROR: both rows and columns "
          ++ "must be positive integers"

convert :: Conversion -> IO ()
convert args = do
  let mDims = extractPosInts [nRows, nCols] args
  case mDims of
    Nothing -> failPosInt
    Just dims -> do
      let matrixHeader = MatrixHeader DBL (dims!!0) (dims!!1)
      result <- runAppIO $
        csvToBinary (inFile args)
                    (outFile args)
                    matrixHeader
      either putStrLn return result

main :: IO ()
main = convert =<< execParser opts
  where opts = info (conversion <**> helper)
                    (fullDesc
                  <> progDesc description
                  <> header title)
