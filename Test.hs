import           Data.List
import           System.IO.Unsafe
import           System.Process
import           Test.QuickCheck

externalSort :: (Show a, Read a) => FilePath -> [a] -> [a]
externalSort p l = unsafePerformIO $ do
  (_, s, _) <- readProcessWithExitCode p [] (show l)
  return $ case uncons (reads s) of
    Just ((l', _), _) -> l'
    Nothing           -> l

good_sort :: (Ord a, Show a, Read a) => FilePath -> [a] -> Bool
good_sort p l = sort l == externalSort p l

main :: IO ()
main = do
  quickCheck $ expectFailure (good_sort "echo" :: [Integer] -> Bool)
