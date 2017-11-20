module Main where

import HML.FileIO.Test.CSVReader (csvReaderTest)
import HML.FileIO.Test.MatrixHeader (matrixHeaderTest)
import HML.FileIO.Test.MatrixSink (matrixSinkTest)
import HML.FileIO.Test.MatrixSource (matrixSourceTest)
import HML.Types.Test.DoubleToBinary (doubleToBinaryTest)
import HML.Types.Test.PosInt (posIntTest)
import HML.Types.Test.TypeName (typeNameTest)
import Test.Tasty

typeProps :: TestTree
typeProps = testGroup "Types"
  [ doubleToBinaryTest
  , posIntTest
  , typeNameTest
  ]

fileIOProps :: TestTree
fileIOProps = testGroup "FileIO"
  [ csvReaderTest
  , matrixHeaderTest
  , matrixSinkTest
  , matrixSourceTest
  ]

hlTest :: TestTree
hlTest = testGroup "HasLearned"
  [ typeProps
  , fileIOProps
  ]

main :: IO ()
main = putStrLn "" >> defaultMain hlTest
