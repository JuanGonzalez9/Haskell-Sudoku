module Main where

import Control.Applicative ((<|>))
import Data.Function ((&))
import qualified Control.Monad
import qualified Data.Char
import qualified Data.Function
import qualified Data.List.Split
import qualified Data.List
import qualified Data.Map.Strict as Map

fixM :: (Eq t, Monad m) => (t -> m t) -> t -> m t
fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Grid = [Row]

isPossible :: Cell -> Bool
isPossible (Possible _) = True
isPossible _            = False

readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 = traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just $ Possible [1..9]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing

showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x
    showCell _ = "."

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = show x ++ "          "
    showCell (Possible xs) =
      (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [1..9]

exclusivePossibilities :: [Cell] -> [[Int]]
exclusivePossibilities row =
  row
  & zip [1..9]
  & filter (isPossible . snd)
  & Data.List.foldl'
      (\acc ~(i, Possible xs) ->
        Data.List.foldl' (\acc' x -> Map.insertWith prepend x [i] acc') acc xs)
      Map.empty
  & Map.filter ((< 4) . length)
  & Map.foldlWithKey'(\acc x is -> Map.insertWith prepend is [x] acc) Map.empty
  & Map.filterWithKey (\is xs -> length is == length xs)
  & Map.elems
  where
    prepend ~[y] ys = y:ys

makeCell :: [Int] -> Maybe Cell
makeCell ys = case ys of
  []  -> Nothing
  [y] -> Just $ Fixed y
  _   -> Just $ Possible ys

pruneCellsByFixed :: [Cell] -> Maybe [Cell]
pruneCellsByFixed cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCell (Possible xs) = makeCell (xs Data.List.\\ fixeds)
    pruneCell x             = Just x

pruneCellsByExclusives :: [Cell] -> Maybe [Cell]
pruneCellsByExclusives cells = case exclusives of
  [] -> Just cells
  _  -> traverse pruneCell cells
  where
    exclusives    = exclusivePossibilities cells
    allExclusives = concat exclusives

    pruneCell cell@(Fixed _) = Just cell
    pruneCell cell@(Possible xs)
      | intersection `elem` exclusives = makeCell intersection
      | otherwise                      = Just cell
      where
        intersection = xs `Data.List.intersect` allExclusives

pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = fixM pruneCellsByFixed cells >>= fixM pruneCellsByExclusives

subGridsToRows :: Grid -> Grid
subGridsToRows =
  concatMap (\rows -> let [r1, r2, r3] = map (Data.List.Split.chunksOf 3) rows
                      in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3)
  . Data.List.Split.chunksOf 3

pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
  traverse pruneCells grid
  >>= fmap Data.List.transpose . traverse pruneCells . Data.List.transpose
  >>= fmap subGridsToRows . traverse pruneCells . subGridsToRows

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'

isGridFilled :: Grid -> Bool
isGridFilled grid = null [ () | Possible _ <- concat grid ]

isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid
  || any isInvalidRow (Data.List.transpose grid)
  || any isInvalidRow (subGridsToRows grid)
  where
    isInvalidRow row =
      let fixeds         = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)

    hasDups l = hasDups' l []

    hasDups' [] _ = False
    hasDups' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = hasDups' ys (y:xs)

nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        fixCell
        . Data.List.minimumBy (compare `Data.Function.on` (possibilityCount . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    possibilityCount (Possible xs) = length xs
    possibilityCount (Fixed _)     = 1

    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    fixCell _                    = error "Impossible case"

    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v = let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

solve :: Grid -> Maybe Grid
solve grid = pruneGrid grid >>= solve'
  where
    solve' g
      | isGridInvalid g = Nothing
      | isGridFilled g  = Just g
      | otherwise       =
          let (grid1, grid2) = nextGrids g
          in solve grid1 <|> solve grid2

main :: IO ()
main = do
  inputs <- lines <$> getContents
  Control.Monad.forM_ inputs $ \input ->
    case readGrid input of
      Nothing   -> putStrLn "Invalid input"
      Just grid -> case solve grid of
        Nothing    -> putStrLn "No solution found"
        Just grid' -> putStrLn $ showGrid grid'