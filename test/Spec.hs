import System.Exit

import qualified Chapter2Spec

main :: IO ()
main = do
	good <- and <$> sequence [Chapter2Spec.runTests]
	if good then exitSuccess else exitFailure
