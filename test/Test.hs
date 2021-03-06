module Main where

import School.App.Test.CSVReader (csvReaderTest)
import School.App.Test.FileTransformer (fileTransformerTest)
import School.Evaluate.Test.Evaluate (evaluateTest)
import School.FileIO.Test.BinConversion (binConversionTest)
import School.FileIO.Test.FileHeader (matrixHeaderTest)
import School.FileIO.Test.MatrixSink (matrixSinkTest)
import School.FileIO.Test.Source (sourceTest)
import School.Train.Test.BackwardPass (backwardPassTest)
import School.Train.Test.ForwardPass (forwardPassTest)
import School.Train.Test.GradientDescent (gradientDescentTest)
import School.Train.Test.GradientDescentPass (gradientDescentPassTest)
import School.Types.Test.Conversion (conversionTest)
import School.Types.Test.FloatEq (floatEqTest)
import School.Types.Test.DataType (typeNameTest)
import School.Unit.Test.Affine (affineTest)
import School.Unit.Test.LogSoftMax (logSoftMaxTest)
import School.Unit.Test.RecLin (recLinTest)
import School.Unit.Test.MultiNoulli (multiNoulliTest)
import School.Unit.Test.UnitForward (unitForwardTest)
import School.Unit.Test.UnitBackward (unitBackwardTest)
import School.Unit.Test.WeightDecay (weightDecayTest)
import Test.Tasty

appProps :: TestTree
appProps = testGroup "App"
  [ csvReaderTest
  , fileTransformerTest ]

evaluateProps :: TestTree
evaluateProps = testGroup "Evaluate"
  [ evaluateTest ]

typeProps :: TestTree
typeProps = testGroup "Types"
  [ conversionTest
  , floatEqTest
  , typeNameTest
  ]

fileIOProps :: TestTree
fileIOProps = testGroup "FileIO"
  [ binConversionTest
  , matrixHeaderTest
  , matrixSinkTest
  , sourceTest
  ]

unitProps :: TestTree
unitProps = testGroup "Unit"
  [ affineTest
  , logSoftMaxTest
  , multiNoulliTest
  , recLinTest
  , unitBackwardTest
  , unitForwardTest
  , weightDecayTest
  ]

trainProps :: TestTree
trainProps= testGroup "Train"
  [ backwardPassTest
  , forwardPassTest
  , gradientDescentTest
  , gradientDescentPassTest
  ]

schoolTest :: TestTree
schoolTest = testGroup "School"
  [ appProps
  , evaluateProps
  , fileIOProps
  , trainProps
  , typeProps
  , unitProps
  ]

main :: IO ()
main = putStrLn "" >> defaultMain schoolTest
