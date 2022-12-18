{-# Language GADTs, DataKinds, LambdaCase, BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/19>

To correlate all the scanner readings this program
selects the first scanner to be "correct". All other
scanners will be oriented relative to the first scanner.
As each scanner's location is fixed it will be queued
to be compared to all the uncorrelated scanner outputs.
Scanning in this order ensures no pair of scanners is
compared more than once.

>>> :{
:main +
  "--- scanner 0 ---\n\
  \404,-588,-901\n\
  \528,-643,409\n\
  \-838,591,734\n\
  \390,-675,-793\n\
  \-537,-823,-458\n\
  \-485,-357,347\n\
  \-345,-311,381\n\
  \-661,-816,-575\n\
  \-876,649,763\n\
  \-618,-824,-621\n\
  \553,345,-567\n\
  \474,580,667\n\
  \-447,-329,318\n\
  \-584,868,-557\n\
  \544,-627,-890\n\
  \564,392,-477\n\
  \455,729,728\n\
  \-892,524,684\n\
  \-689,845,-530\n\
  \423,-701,434\n\
  \7,-33,-71\n\
  \630,319,-379\n\
  \443,580,662\n\
  \-789,900,-551\n\
  \459,-707,401\n\
  \\n\
  \--- scanner 1 ---\n\
  \686,422,578\n\
  \605,423,415\n\
  \515,917,-361\n\
  \-336,658,858\n\
  \95,138,22\n\
  \-476,619,847\n\
  \-340,-569,-846\n\
  \567,-361,727\n\
  \-460,603,-452\n\
  \669,-402,600\n\
  \729,430,532\n\
  \-500,-761,534\n\
  \-322,571,750\n\
  \-466,-666,-811\n\
  \-429,-592,574\n\
  \-355,545,-477\n\
  \703,-491,-529\n\
  \-328,-685,520\n\
  \413,935,-424\n\
  \-391,539,-444\n\
  \586,-435,557\n\
  \-364,-763,-893\n\
  \807,-499,-711\n\
  \755,-354,-619\n\
  \553,889,-390\n\
  \\n\
  \--- scanner 2 ---\n\
  \649,640,665\n\
  \682,-795,504\n\
  \-784,533,-524\n\
  \-644,584,-595\n\
  \-588,-843,648\n\
  \-30,6,44\n\
  \-674,560,763\n\
  \500,723,-460\n\
  \609,671,-379\n\
  \-555,-800,653\n\
  \-675,-892,-343\n\
  \697,-426,-610\n\
  \578,704,681\n\
  \493,664,-388\n\
  \-671,-858,530\n\
  \-667,343,800\n\
  \571,-461,-707\n\
  \-138,-166,112\n\
  \-889,563,-600\n\
  \646,-828,498\n\
  \640,759,510\n\
  \-630,509,768\n\
  \-681,-892,-333\n\
  \673,-379,-804\n\
  \-742,-814,-386\n\
  \577,-820,562\n\
  \\n\
  \--- scanner 3 ---\n\
  \-589,542,597\n\
  \605,-692,669\n\
  \-500,565,-823\n\
  \-660,373,557\n\
  \-458,-679,-417\n\
  \-488,449,543\n\
  \-626,468,-788\n\
  \338,-750,-386\n\
  \528,-832,-391\n\
  \562,-778,733\n\
  \-938,-730,414\n\
  \543,643,-506\n\
  \-524,371,-870\n\
  \407,773,750\n\
  \-104,29,83\n\
  \378,-903,-323\n\
  \-778,-728,485\n\
  \426,699,580\n\
  \-438,-605,-362\n\
  \-469,-447,-387\n\
  \509,732,623\n\
  \647,635,-688\n\
  \-868,-804,481\n\
  \614,-800,639\n\
  \595,780,-596\n\
  \\n\
  \--- scanner 4 ---\n\
  \727,592,562\n\
  \-293,-554,779\n\
  \441,611,-461\n\
  \-714,465,-776\n\
  \-743,427,-804\n\
  \-660,-479,-426\n\
  \832,-632,460\n\
  \927,-485,-438\n\
  \408,393,-506\n\
  \466,436,-512\n\
  \110,16,151\n\
  \-258,-428,682\n\
  \-393,719,612\n\
  \-211,-452,876\n\
  \808,-476,-593\n\
  \-575,615,604\n\
  \-485,667,467\n\
  \-680,325,-822\n\
  \-627,-443,-432\n\
  \872,-547,-609\n\
  \833,512,582\n\
  \807,604,487\n\
  \839,-516,451\n\
  \891,-625,532\n\
  \-652,-548,-490\n\
  \30,-46,-14\n"
:}
79
3621

-}
module Main (main) where

import Advent (format, counts)
import Advent.Box (coverBoxes, Box(..))
import Advent.Coord3 (Coord3(..), origin)
import Advent.Nat (Nat(Z, S))
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.List (transpose)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 457
-- 13243
main :: IO ()
main =
 do inp <- [format|2021 19 (--- scanner %u ---%n(%d,%d,%d%n)*)&%n|]
    let coord (x,y,z) = C3 x y z
    let scanners = [map coord ps | (_,ps) <- inp]

    let (offsets, locations) = unzip (start scanners)
    print (Set.size (Set.unions locations))
    print (radius offsets)

-- | Starts the scanner reading correlation algorithm.
start ::
  [[Coord3]] {- ^ uncorrelated scanner readings -} ->
  [(Coord3, Set Coord3)] {- ^ correlated scanner locations and readings -}
start (x:xs) = assemble xs [(origin, Set.fromList x)]
start [] = []

-- | Worker for 'start'.
assemble ::
  [[Coord3]]             {- ^ uncorrelated scanner readings -} ->
  [(Coord3, Set Coord3)] {- ^ recently correlated scanners -} ->
  [(Coord3, Set Coord3)] {- ^ completed correlated locations and readings -}
assemble _ [] = []
assemble remains (c@(_,reference):cs) = c : assemble remain' (new ++ cs)
  where
    (new,remain') = partitionEithers
      [ maybe (Right remain) Left (match reference remain)
        | remain <- remains
      ]

-- | Try to match the uncorrelated offsets to a set of absolute coordinates.
match ::
  Set Coord3 {- ^ reference coordinates -} ->
  [Coord3]   {- ^ uncorrelated offsets -} ->
  Maybe (Coord3, Set Coord3) {- ^ sensor offset and absolute beacons -}
match xset ys = listToMaybe
 [(offset, yset')
   | yset <- Set.fromList <$> reorient ys
   , offset <- prefilter ((-) <$> Set.toList xset <*> Set.toList yset)
   , let yset' = Set.mapMonotonic (offset +) yset
   , 12 <= Set.size (Set.intersection xset yset')
 ]

-- | Only bother checking offsets that occur enough times that it's possible
-- to have an overlap.
prefilter :: [Coord3] -> [Coord3]
prefilter = Map.keys . Map.filter (>= 12) . counts

-- * Reorienting sensor readings

-- | Return all 24 possibilities of rotating the given list of coordinates.
reorient :: [Coord3] -> [[Coord3]]
reorient = transpose . map (rotations >=> faces)

faces :: Coord3 -> [Coord3]
faces (C3 x y z) =
  [
    C3 x y z,
    C3 y (-x) z,
    C3 (-x) (-y) z,
    C3 (-y) x z,
    C3 y z x,
    C3 y (-z) (-x)
  ]

-- | Return the 4 rotations of a point around the x-axis
rotations :: Coord3 -> [Coord3]
rotations (C3 x y z) =
  [
    C3 x y z,
    C3 x (-z) y,
    C3 x (-y) (-z),
    C3 x z (-y)
  ]

-- * Determining sensor radius

-- | Determines the maximum manhattan distance between any pair of points.
-- this is achieved by finding the bounding octahedroid for this set of points.
radius :: [Coord3] -> Int
radius = minCube . coverBoxes . map to4

-- | Find the side length of the smallest hypercube that can bound
-- the given hyperrectangle.
minCube :: Box n -> Int
minCube (Dim a b x) = max (b-a) (minCube x)
minCube Pt = 0

-- | Convert a 3D point into an octahedron coordinate.
to4 :: Coord3 -> Box ('S ('S ('S ('S 'Z))))
to4 (C3 x y z) = x+y+z # x+y-z # x-y+z # x-y-z # Pt
  where
    i # j = Dim i i j
    infixr 5 #
