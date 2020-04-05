import Test.Hspec
import qualified Data.Queue.MutableSpec as Q
import qualified Data.Graph.AbstractSpec as GA
import qualified Data.Graph.Abstract.CommonSpec as GAC
import qualified Data.Graph.Abstract.InstancesSpec as GAI

main :: IO ()
main = hspec $ do
    Q.spec
    GA.spec
    GAC.spec
    GAI.spec
