{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AdventOfCode.Day08.Run where

import AdventOfCode.Day08.Input
import AdventOfCode.Utils
import Data.List.Extra
import qualified Data.HashSet as S
import Data.Hashable
import GHC.Generics
import Data.Ord

data JunctionBox = JunctionBox Int Int Int
  deriving (Show, Eq, Ord, Generic)

instance Hashable JunctionBox where

parseJunctionBox :: String -> Either String JunctionBox
parseJunctionBox junctionBoxString = case splitOn "," junctionBoxString of
  [x, y, z] -> JunctionBox <$> parseInt x <*> parseInt y <*> parseInt z
  _ -> Left $ "Invalid junction box string: " <> junctionBoxString

parseJunctionBoxes :: String -> Either String (S.HashSet JunctionBox)
parseJunctionBoxes boxes = fmap S.fromList $ mapM parseJunctionBox $ lines boxes

data Connection = Connection JunctionBox JunctionBox
  deriving (Show, Eq, Generic)

mkConnection :: JunctionBox -> JunctionBox -> Connection
mkConnection junctionBox1 junctionBox2 = Connection (min junctionBox1 junctionBox2) (max junctionBox1 junctionBox2)

instance Hashable Connection where

newtype Circuit = Circuit [Connection]
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance Hashable Circuit where

isJunctionBoxPartOfConnection :: JunctionBox -> Connection -> Bool
isJunctionBoxPartOfConnection junctionBox (Connection junctionBox1 junctionBox2) =
  junctionBox == junctionBox1 || junctionBox == junctionBox2

isJunctionBoxPartOfCircuit :: JunctionBox -> Circuit -> Bool
isJunctionBoxPartOfCircuit junctionBox (Circuit connections) =
  any (isJunctionBoxPartOfConnection junctionBox) connections

mergeCircuits :: Circuit -> Circuit -> JunctionBox -> JunctionBox -> [Circuit] -> [Circuit]
mergeCircuits circuit1 circuit2 junctionBox1 junctionBox2 circuits =
  let newCircuit = circuit1 <> circuit2 <> Circuit [mkConnection junctionBox1 junctionBox2]
      restCircuits = filter (\circuit -> circuit /= circuit1 && circuit /= circuit2) circuits
  in  newCircuit : restCircuits

withoutCircuit :: Circuit -> [Circuit] -> [Circuit]
withoutCircuit circuit = filter (/= circuit)

addToCircuit :: Circuit -> JunctionBox -> JunctionBox -> [Circuit] -> [Circuit]
addToCircuit circuit junctionBox1 junctionBox2 circuits =
  let newCircuit = circuit <> Circuit [mkConnection junctionBox1 junctionBox2]
      restCircuits = withoutCircuit circuit circuits
  in  newCircuit : restCircuits

addNewCircuit :: JunctionBox -> JunctionBox -> [Circuit] -> [Circuit]
addNewCircuit junctionBox1 junctionBox2 circuits =
  let newCircuit = Circuit [mkConnection junctionBox1 junctionBox2]
      restCircuits = withoutCircuit newCircuit circuits
  in  newCircuit : restCircuits

addConnectionToCircuits :: Connection -> [Circuit] -> [Circuit]
addConnectionToCircuits (Connection junctionBox1 junctionBox2) circuits =
  let circuitFromBox1 = find (isJunctionBoxPartOfCircuit junctionBox1) circuits
      circuitFromBox2 = find (isJunctionBoxPartOfCircuit junctionBox2) circuits
  in  case (circuitFromBox1, circuitFromBox2) of
        (Just circuit1, Just circuit2) -> if circuit1 == circuit2
                                          then circuits
                                          else mergeCircuits circuit1 circuit2 junctionBox1 junctionBox2 circuits
        (Just circuit1, Nothing) -> addToCircuit circuit1 junctionBox1 junctionBox2 circuits
        (Nothing, Just circuit2) -> addToCircuit circuit2 junctionBox1 junctionBox2 circuits
        (Nothing, Nothing) -> addNewCircuit junctionBox1 junctionBox2 circuits

data TaskState = TaskState (S.HashSet JunctionBox) [Circuit]
  deriving (Show, Eq, Generic)

startingTaskState :: String -> Either String TaskState
startingTaskState inputString = do
  junctionBoxes <- parseJunctionBoxes inputString
  pure $ TaskState junctionBoxes []

distanceBetweenJunctionBoxes :: JunctionBox -> JunctionBox -> Double
distanceBetweenJunctionBoxes (JunctionBox x1 y1 z1) (JunctionBox x2 y2 z2) =
  let xDiff = fromIntegral (x1 - x2)
      yDiff = fromIntegral (y1 - y2)
      zDiff = fromIntegral (z1 - z2)
  in  sqrt (xDiff * xDiff + yDiff * yDiff + zDiff * zDiff)

type Distances = [(JunctionBox, JunctionBox, Double)]

getAllDistances :: S.HashSet JunctionBox -> Distances
getAllDistances junctionBoxes = sortOn (\(_, _, distance) -> distance) $ do
  junctionBox1 <- S.toList junctionBoxes
  junctionBox2 <- S.toList junctionBoxes
  if junctionBox1 >= junctionBox2 then [] else [(junctionBox1, junctionBox2, distanceBetweenJunctionBoxes junctionBox1 junctionBox2)]

nextPart1State :: Distances -> Int -> TaskState -> TaskState
nextPart1State [] _ state = state
nextPart1State _ 0 state = state
nextPart1State (lowestDistance : rest) connectionsRemain (TaskState junctionBoxes connections) =
  let (junctionBox1, junctionBox2, _) = lowestDistance
      conn = mkConnection junctionBox1 junctionBox2
      newCircuits = addConnectionToCircuits conn connections
      newState = TaskState junctionBoxes newCircuits
  in  nextPart1State rest (connectionsRemain - 1) newState

nextPart2State :: Distances -> TaskState -> (TaskState, Maybe Connection)
nextPart2State [] state = (state, Nothing)
nextPart2State (lowestDistance : rest) (TaskState junctionBoxes connections) =
  let (junctionBox1, junctionBox2, _) = lowestDistance
      conn = mkConnection junctionBox1 junctionBox2
      newCircuits = addConnectionToCircuits conn connections
      newState = TaskState junctionBoxes newCircuits
      isFinished = fmap circuitSize newCircuits == [length junctionBoxes]
  in  if isFinished then (newState, Just conn) else nextPart2State rest newState

connectionJunctionBoxes :: Connection -> S.HashSet JunctionBox
connectionJunctionBoxes (Connection junctionBox1 junctionBox2) = S.fromList [junctionBox1, junctionBox2]

circuitSize :: Circuit -> Int
circuitSize (Circuit connections) = S.size $ foldMap connectionJunctionBoxes connections

totalCircuitValue :: TaskState -> Int
totalCircuitValue (TaskState _ circuits) =
  let circuitSizes = fmap circuitSize circuits
      threeLargestCircuitSizes = take 3 $ sortBy (comparing Down) circuitSizes
  in  product threeLargestCircuitSizes

solvePart1 :: IO Int
solvePart1 = do
  taskState@(TaskState junctionBoxes _) <- either (error . show) pure $ startingTaskState input
  let distances = getAllDistances junctionBoxes
  let finalPart1State = nextPart1State distances 1000 taskState
  pure $ totalCircuitValue finalPart1State

solvePart2 :: IO Int
solvePart2 = do
  taskState@(TaskState junctionBoxes _) <- either (error . show) pure $ startingTaskState input
  let distances = getAllDistances junctionBoxes
  let finalPart1State = nextPart1State distances 1000 taskState
  let (_, possibleFinalConnection) = nextPart2State distances finalPart1State
  let multipliedXCoords = maybe 0 (\(Connection (JunctionBox x1 _ _) (JunctionBox x2 _ _)) -> x1 * x2) possibleFinalConnection
  pure multipliedXCoords

solve :: [AOCUncomputedResult]
solve =
  [ AOCUncomputedResult 8 1 solvePart1
  , AOCUncomputedResult 8 2 solvePart2
  ]
