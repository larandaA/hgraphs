import Test.Hspec
import qualified Data.Queue.MutableSpec as Q
import qualified Data.GraphSpec as G

main :: IO ()
main = hspec $ do
    Q.spec
    G.spec
