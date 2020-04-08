import Test.Hspec
import qualified Data.Queue.MutableSpec as Q
import qualified Data.Graph.AbstractSpec as GA
import qualified Data.Graph.Abstract.BuilderSpec as GAB
import qualified Data.Graph.Abstract.CommonSpec as GAC
import qualified Data.Graph.Abstract.InstanceSpec as GAI
import qualified Data.Graph.Abstract.TransformSpec as GAT

main :: IO ()
main = hspec $ do
    Q.spec
    GA.spec
    GAB.spec
    GAC.spec
    GAI.spec
    GAT.spec
