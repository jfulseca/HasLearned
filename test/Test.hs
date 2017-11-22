module Main where

import School.FileIO.Test.CSVReader (csvReaderTest)
import School.FileIO.Test.MatrixHeader (matrixHeaderTest)
import School.FileIO.Test.MatrixSink (matrixSinkTest)
import School.FileIO.Test.MatrixSource (matrixSourceTest)
import School.Types.Test.DoubleToBinary (doubleToBinaryTest)
import School.Types.Test.FloatEq (floatEqTest)
import School.Types.Test.PosInt (posIntTest)
import School.Types.Test.TypeName (typeNameTest)
import Test.Tasty

typeProps :: TestTree
typeProps = testGroup "Types"
  [ doubleToBinaryTest
  , floatEqTest
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
