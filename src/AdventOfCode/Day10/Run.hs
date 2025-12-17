{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day10.Run where

import AdventOfCode.Day10.Input
import AdventOfCode.Utils
import Data.List.Extra
import Data.Foldable
import Control.Monad
import qualified Data.HashSet as S
import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import Data.Hashable
import GHC.Generics
import Text.Megaparsec
import Data.Either.Extra (mapLeft)
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import qualified Data.Vector as V
import Data.Vector.Instances ()
import Control.Lens hiding ((.>))
import Data.Semigroup
import Control.Parallel.Strategies
import Data.SBV
import Safe

data Light = Off | On
  deriving (Show, Eq, Generic)

instance Hashable Light where

newtype LightIndicators = LightIndicators (S.HashSet Int)
  deriving (Eq, Generic, Semigroup, Monoid)

instance Hashable LightIndicators where

instance Show LightIndicators where
  show (LightIndicators indicators) = "LightIndicators([" <> intercalate "," (show <$> S.toList indicators) <> "])"

newtype ButtonGroup = ButtonGroup (S.HashSet Int)
  deriving (Eq, Generic, Semigroup, Monoid)

getButtonGroupString :: ButtonGroup -> String
getButtonGroupString (ButtonGroup bGroup) = "[" <> intercalate "," (show <$> S.toList bGroup) <> "]"

instance Show ButtonGroup where
  show buttonGroup = "ButtonGroup(" <> getButtonGroupString buttonGroup <> ")"

instance Hashable ButtonGroup where

newtype Buttons = Buttons [ButtonGroup]
  deriving (Eq, Generic, Semigroup, Monoid)

instance Show Buttons where
  show (Buttons buttons) =
    let buttonGroups = getButtonGroupString <$> buttons
    in  "Buttons([" <> intercalate "," buttonGroups <> "])"

instance Hashable Buttons where

buttonsSize :: Buttons -> Int
buttonsSize (Buttons buttons) = length buttons

buttonsList :: Buttons -> [ButtonGroup]
buttonsList (Buttons buttons) = buttons

deleteButtonGroup :: ButtonGroup -> Buttons -> Buttons
deleteButtonGroup buttonGroup (Buttons buttons) = Buttons $ delete buttonGroup buttons

groupContainsButton :: Int -> ButtonGroup -> Bool
groupContainsButton buttonIndex (ButtonGroup buttonGroup) = S.member buttonIndex buttonGroup

type Joltage = Int

type Joltages = V.Vector Joltage

data Machine = Machine LightIndicators Buttons Joltages
  deriving (Show, Eq)

testInput :: String
testInput = """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""

testInput2 :: String
testInput2 = """
[.###.##.#.] (1,3,4,5,6,8) (0,2,3,4,5,6,8,9) (0,1,3,4,7,8,9) (3,4,5,8,9) (4,7) (0,2,5) (1,3,4,5,6,7,9) (4,6,7) (0,1,2,7,8,9) (1,3,5,6,7) (1,3) (0,2,3,4,5,6,7,9) (1,2,3,5,6,8,9) {44,85,54,98,81,77,88,79,54,75}
"""

hashSetIndicatorsFromVector :: V.Vector Light -> LightIndicators
hashSetIndicatorsFromVector vector = LightIndicators $ S.fromList $ V.toList $ V.imapMaybe (\i light -> if light == On then Just i else Nothing) vector

parseLights :: Parsec Void String LightIndicators
parseLights =
  between (string "[") (string "]") (hashSetIndicatorsFromVector . V.fromList <$> many (choice [string "." >> pure Off, string "#" >> pure On]))

parseButtonGroup :: Parsec Void String ButtonGroup
parseButtonGroup =
  ButtonGroup . S.fromList <$> sepBy decimal (string ",")

parseButtons :: Parsec Void String Buttons
parseButtons =
  Buttons <$> sepBy (between (string "(") (string ")") parseButtonGroup) (try (string " " <* lookAhead (string "(")))

parseJoltage :: Parsec Void String Joltages
parseJoltage = do
  between (string "{") (string "}") (V.fromList <$> sepBy decimal (string ","))

parseMachine :: Parsec Void String Machine
parseMachine = do
  lights <- parseLights <* space
  buttons <- try (parseButtons <* space)
  joltage <- parseJoltage
  pure $ Machine lights buttons joltage

parseMachines :: Parsec Void String [Machine]
parseMachines = sepBy parseMachine (try (string "\n" <* lookAhead parseMachine))

parseInput :: String -> Either String [Machine]
parseInput inputString = mapLeft show $ runParser parseMachines "input.txt" inputString

distanceFromGoal :: LightIndicators -> LightIndicators -> Int
distanceFromGoal (LightIndicators targetIndicators) (LightIndicators lightIndicators) =
  let firstDifference = S.difference targetIndicators lightIndicators
      secondDifference = S.difference lightIndicators targetIndicators
  in  S.size firstDifference + S.size secondDifference

applyButton :: LightIndicators -> Int -> LightIndicators
applyButton (LightIndicators lightIndicators) button =
  LightIndicators $ if S.member button lightIndicators then S.delete button lightIndicators else S.insert button lightIndicators

applyButtonGroup :: ButtonGroup -> LightIndicators -> LightIndicators
applyButtonGroup (ButtonGroup buttonGroup) lightIndicators =
  foldl' applyButton lightIndicators buttonGroup

applyButtonsGroupJoltage :: ButtonGroup -> Joltages -> Joltages
applyButtonsGroupJoltage (ButtonGroup buttonGroup) joltages = foldl' applyButtonJoltage joltages buttonGroup

applyButtonJoltage :: Joltages -> Joltage -> Joltages
applyButtonJoltage joltages buttonJoltage = joltages & ix buttonJoltage -~ 1

addOneToDepth :: Maybe (Min Int) -> Maybe (Min Int)
addOneToDepth Nothing = Nothing
addOneToDepth (Just (Min depth)) = Just (Min (depth + 1))

mulTwoToDepth :: Maybe (Min Int) -> Maybe (Min Int)
mulTwoToDepth Nothing = Nothing
mulTwoToDepth (Just (Min depth)) = Just (Min (depth * 2))

type KeyForIndicators = (LightIndicators, Buttons, LightIndicators)

type KeyForJoltage = Joltages

type HashValue = Maybe (Min Int)

type ButtonTable s = H.HashTable s KeyForIndicators HashValue

type JoltageTable s = H.HashTable s KeyForJoltage HashValue

joltageExceeded :: Joltages -> Bool
joltageExceeded = V.any (< 0)

atJoltageGoal :: Joltages -> Bool
atJoltageGoal = V.all (== 0)

joltageGoalDistance :: Joltages -> Int
joltageGoalDistance = V.sum

joltagesAllEven :: Joltages -> Bool
joltagesAllEven = V.all even

halveJoltages :: Joltages -> Joltages
halveJoltages = V.map (`div` 2)

oddJoltageValues :: Joltages -> Int
oddJoltageValues joltages = V.foldl' (\acc joltage -> if odd joltage then acc + joltage else acc) 0 joltages

buttonGroupOrderValue :: Joltages -> ButtonGroup -> (Int, Int, Joltages)
buttonGroupOrderValue joltages buttonGroup =
  let applied = applyButtonsGroupJoltage buttonGroup joltages
      oddValues = oddJoltageValues applied
      distance = joltageGoalDistance applied
  in  (oddValues, distance, applied)

orderButtonSet :: Buttons -> Joltages -> [ButtonGroup]
orderButtonSet buttons joltages =
  fmap fst $
  sortOn snd $
  filter (\(_, (_, _, applied)) -> not $ joltageExceeded applied) $
  fmap (\buttonGroup -> (buttonGroup, buttonGroupOrderValue joltages buttonGroup)) $
  buttonsList buttons

applyButtonsIndicators :: ButtonTable s -> LightIndicators -> Buttons -> LightIndicators -> ST s (Maybe (Min Int))
applyButtonsIndicators buttonTable target remainingButtons currentIndicators = do
  let atGoal = currentIndicators == target
      subSearch = addOneToDepth <$> foldMap' testButtons (buttonsList remainingButtons)
      goalResult = pure $ Just $ Min 0
      testButtons buttonGroup =
        let newIndicators = applyButtonGroup buttonGroup currentIndicators
            newRemainingButtons = deleteButtonGroup buttonGroup remainingButtons
        in  applyButtonsIndicators buttonTable target newRemainingButtons newIndicators
  lookupFromTableOrDefault buttonTable (target, remainingButtons, currentIndicators) (if atGoal then goalResult else subSearch)

solveJoltage :: Buttons -> Joltages -> IO (Maybe Int32)
solveJoltage buttons joltages = do
  let indexedButtons = zip [0..] $ buttonsList buttons :: [(Int, ButtonGroup)]
  let indexedJoltages = zip [0..] $ V.toList joltages :: [(Int, Joltage)]
  res <- optimize Lexicographic $ do
    buttonGroupVars <- forM indexedButtons $ \(bIndex, _) -> do
      var <- sInteger ("buttonGroup-" <> show bIndex)
      constrain $ var .>= 0
      pure var
    forM_ indexedJoltages $ \(jIndex, joltage) -> do
      let buttonsWithJoltage = fst <$> filter (\(_, buttonGroup) -> groupContainsButton jIndex buttonGroup) indexedButtons
      let possibleVars = traverse (atMay buttonGroupVars) buttonsWithJoltage
      forM_ possibleVars $ \vars -> do
        constrain $ sum vars .== fromIntegral joltage
    minimize "minPresses" $ sum buttonGroupVars
  case res of
    LexicographicResult satResult@(Satisfiable _ _) ->
      let dict = getModelDictionary satResult
          getVal bIndex = fromCV <$> Map.lookup ("buttonGroup-" <> show bIndex) dict
          allVals = sequence (getVal . fst <$> indexedButtons) :: Maybe [Integer]
      in  pure (fromIntegral . sum <$> allVals)
    _ -> pure Nothing

solvePart1 :: IO Int
solvePart1 = do
  machines <- either (error . show) pure $ parseInput input
  let applyButtonsOff lights buttons = runST $ do
        buttonTable <- H.new
        applyButtonsIndicators buttonTable lights buttons mempty
  let depths = parMap rdeepseq (\(Machine lights buttons _) -> applyButtonsOff lights buttons) machines
  let totalDepth = fmap (getSum . foldMap (Sum . getMin)) (sequence depths)
  maybe (fail "No solution found.") pure totalDepth

solvePart2 :: IO Int32
solvePart2 = do
  machines <- either (error . show) pure $ parseInput input
  listOfMaybeDepths <- mapConcurrentlyBounded (\(Machine _ buttons joltages) -> solveJoltage buttons joltages) machines
  let depths = sequence listOfMaybeDepths
  let totalDepth = fmap sum depths
  maybe (fail "No solution found.") pure totalDepth

solve :: IO ()
solve = do
  presentResult 10 1 solvePart1
  presentResult 10 2 solvePart2
