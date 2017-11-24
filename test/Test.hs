module Main where

import School.FileIO.Test.CSVReader (csvReaderTest)
import School.FileIO.Test.MatrixHeader (matrixHeaderTest)
import School.FileIO.Test.MatrixSink (matrixSinkTest)
import School.FileIO.Test.MatrixSource (matrixSourceTest)
import School.Types.Test.DoubleConversion (doubleToBinaryTest)
import School.Types.Test.FloatEq (floatEqTest)
import School.Types.Test.PosInt (posIntTest)
import School.Types.Test.TypeName (typeNameTest)
import School.Unit.Test.Affine (affineTest)
import School.Unit.Test.RecLin (recLinTest)
import School.Unit.Test.UnitForward (unitForwardTest)
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

unitProps :: TestTree
unitProps = testGroup "Unit"
  [ affineTest
  , recLinTest
  , unitForwardTest
  ]

schoolTest :: TestTree
schoolTest = testGroup "School"
  [ typeProps
  , fileIOProps
  , unitProps
  ]

main :: IO ()
main = putStrLn "" >> defaultMain schoolTest
