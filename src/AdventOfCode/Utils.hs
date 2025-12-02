module AdventOfCode.Utils where

import Text.Read (readMaybe)

parseInt :: String -> Either String Int
parseInt possibleInt = maybe (Left $ "Invalid int: " <> possibleInt) Right $ readMaybe possibleInt

parseInteger :: String -> Either String Integer
parseInteger possibleInteger = maybe (Left $ "Invalid integer: " <> possibleInteger) Right $ readMaybe possibleInteger