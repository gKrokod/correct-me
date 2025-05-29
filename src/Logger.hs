module Logger (writeLog) where

import Data.Text (Text)
import Data.Text.IO as TIO (putStrLn)

writeLog :: Text -> IO ()
writeLog = TIO.putStrLn
