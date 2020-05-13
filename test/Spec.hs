import Test.Hspec
import qualified Data.PriorityQueue.MutableSpec as PQ
import qualified Data.Queue.MutableSpec as Q
import qualified Data.Graph.AbstractSpec as GA
import qualified Data.Graph.Abstract.AccessorSpec as GAA
import qualified Data.Graph.Abstract.Accessor.Algorithm.BfsSpec as AccessorBfs
import qualified Data.Graph.Abstract.Accessor.Algorithm.DfsSpec as AccessorDfs
import qualified Data.Graph.Abstract.Accessor.Algorithm.DistanceSpec as AccessorDistance
import qualified Data.Graph.Abstract.Accessor.Algorithm.MaxFlowSpec as AccessorMaxFlow
import qualified Data.Graph.Abstract.Accessor.Algorithm.MstSpec as AccessorMst
import qualified Data.Graph.Abstract.Accessor.DsuSpec as AccessorDsu
import qualified Data.Graph.Abstract.BuilderSpec as GAB
import qualified Data.Graph.Abstract.CommonSpec as GAC
import qualified Data.Graph.Abstract.InstanceSpec as GAI
import qualified Data.Graph.Abstract.TransformSpec as GAT

main :: IO ()
main = hspec $ do
    PQ.spec
    Q.spec
    GA.spec
    GAA.spec
    AccessorBfs.spec
    AccessorDfs.spec
    AccessorDistance.spec
    AccessorMaxFlow.spec
    AccessorMst.spec
    AccessorDsu.spec
    GAB.spec
    GAC.spec
    GAI.spec
    GAT.spec
