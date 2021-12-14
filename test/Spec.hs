import qualified Advent2021.Day1 as Day1
import qualified Advent2021.Day2 as Day2
import Data.Coerce
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ -- properties,
      --   unitTests
      day1Tests,
      day2Tests
    ]

day1Example =
  unlines
    [ "199",
      "200",
      "208",
      "210",
      "200",
      "207",
      "240",
      "269",
      "260",
      "263"
    ]

day1Tests =
  testGroup
    "Day 1"
    [ testCase "Part 1" $
        Day1.findIncreases1 (Day1.mkDepths day1Example) @?= 7,
      testCase "Part 2" $
        Day1.findIncreases3 (Day1.mkDepths day1Example) @?= 5
    ]

day2Example =
  unlines
    [ "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    ]

day2Tests =
  testGroup
    "Day 2"
    [ testCase "Part 1" $
        Day2.runCommandsWithoutAim (Day2.mkCommands day2Example) (Day2.Position 0 0) @?= Day2.Position 15 10,
      testCase "Part 2" $
        let Day2.PositionWithAim {..} = Day2.runCommandsWithAim (Day2.mkCommands day2Example) (Day2.PositionWithAim 0 0 0)
         in (coerce horizontal, coerce depth) @?= (15 :: Int, 60 :: Int)
    ]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]

-- scProps =
--   testGroup
--     "(checked by SmallCheck)"
--     [ SC.testProperty "sort == sort . reverse" $
--         \list -> sort (list :: [Int]) == sort (reverse list),
--       SC.testProperty "Fermat's little theorem" $
--         \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
--       -- the following property does not hold
--       SC.testProperty "Fermat's last theorem" $
--         \x y z n ->
--           (n :: Integer) >= 3 SC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
--     ]

-- qcProps =
--   testGroup
--     "(checked by QuickCheck)"
--     [ QC.testProperty "sort == sort . reverse" $
--         \list -> sort (list :: [Int]) == sort (reverse list),
--       QC.testProperty "Fermat's little theorem" $
--         \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
--       -- the following property does not hold
--       QC.testProperty "Fermat's last theorem" $
--         \x y z n ->
--           (n :: Integer) >= 3 QC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
--     ]

-- unitTests =
--   testGroup
--     "Unit tests"
--     [ testCase "List comparison (different length)" $
--         [1, 2, 3] `compare` [1, 2] @?= GT,
--       -- the following test does not hold
--       testCase "List comparison (same length)" $
--         [1, 2, 3] `compare` [1, 2, 2] @?= LT
--     ]
