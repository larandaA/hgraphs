import Test.Hspec
import qualified Data.Queue.MutableSpec as Q

main :: IO ()
main = hspec $ do
    Q.spec    
