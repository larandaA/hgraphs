import Test.Hspec
import qualified Data.Queue.MutableSpec as Q
import qualified Data.Graph.AbstractSpec as GA

main :: IO ()
main = hspec $ do
    Q.spec
    GA.spec
