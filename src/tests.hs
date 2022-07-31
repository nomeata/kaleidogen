import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified TestLogic

main = defaultMain $ testGroup "Tests"
    [ QC.testProperty "Logic actions are complete" TestLogic.canReconstruct
    ]
