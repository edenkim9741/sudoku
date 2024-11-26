-- | A Simple Sudoku Solver
--   27th September, 2007
--   In Chapter 05

-- 0. Basic data types

-- import Data.Char

type Matrix a = [Row a]

type Row a = [a]

type Grid = Matrix Digit

type Digit = Char

digits :: [Digit]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

-- Q1. 다음 스도쿠 예시를 Grid 타입의 값으로 작성하시오.
--     빈칸은 '0'으로 표시하고, '1'~'9'로 칸을 채운다.
--
--       _ _ 4 _ _ 5 7 _ _
--       _ _ _ _ _ 9 4 _ _
--       3 6 _ _ _ _ _ _ 8
--       7 2 _ _ 6 _ _ _ _
--       _ _ _ 4 _ 2 _ _ _
--       _ _ _ _ 8 _ _ 9 3
--       4 _ _ _ _ _ _ 5 6
--       _ _ 5 3 _ _ _ _ _
--       _ _ 6 1 _ _ 9 _ _


sudoku1 :: Grid
sudoku1 = [ ['0', '0', '4', '0', '0', '5', '7', '0', '0'],
            ['0', '0', '0', '0', '0', '9', '4', '0', '0'],
            ['3', '6', '0', '0', '0', '0', '0', '0', '8'],
            ['7', '2', '0', '0', '6', '0', '0', '0', '0'],
            ['0', '0', '0', '4', '0', '2', '0', '0', '0'],
            ['0', '0', '0', '0', '8', '0', '0', '9', '3'],
            ['4', '0', '0', '0', '0', '0', '0', '5', '6'],
            ['0', '0', '5', '3', '0', '0', '0', '0', '0'],
            ['0', '0', '6', '1', '0', '0', '9', '0', '0']
          ]

-- Q2. Grid 타입의 값을 입력받아 위의 스도쿠 예시와 같은 형태의 문자열을
--     출력하는 함수 display를 작성하시오.
--      (1) 빈칸은 밑줄(_)로 표시
--      (2) 각 칸 사이에 공백을 둔다.
--      (3) 한 줄에 9개의 빈칸 또는 숫자를 표시하고 줄바꿈 문자를 둔다.
--      (4) 총 9개 줄을 표시
--

display :: Grid -> String
display sudoku = unlines $ map (map replaceZero) sudoku
  where
      replaceZero '0' = '_'
      replaceZero x = x

-- | 1. Specification
solve1 :: Grid -> [Grid]
solve1 = filter valid . expand . choices

type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where
    choice d
      | blank d = digits
      | otherwise = [d]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]

valid :: Grid -> Bool
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = x `notElem` xs && nodups xs

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols [xs] = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> [Row a]
boxs =
  map ungroup
    . ungroup
    . map cols
    . group
    . map group

ungroup = concat

group [] = []
group (x : y : z : xs) = [x, y, z] : group xs


-- Q3.  solve1 함수가 아래와 같이 작성되었다.
--
--      solve1 :: Grid -> [Grid]
--      solve1 = filter valid . expand . choices
--
-- Q3-1. choices의 타입은 무엇이며, choices sudoku1의 결과를 작성하시오.
{-

  choices의 타입은 Grid -> Matrix Choice이므로 [[Char]]을 입력으로 받고 [[[Char]]]을 출력으로 가진다고 할 수 있다.
  choices sudoku1의 결과는 다음과 같다.
  [["123456789","123456789","4","123456789","123456789","5","7","123456789","123456789"],["123456789","123456789","123456789","123456789","123456789","9","4","123456789","123456789"],["3","6","123456789","123456789","123456789","123456789","123456789","123456789","8"],["7","2","123456789","123456789","6","123456789","123456789","123456789","123456789"],["123456789","123456789","123456789","4","123456789","2","123456789","123456789","123456789"],["123456789","123456789","123456789","123456789","8","123456789","123456789","9","3"],["4","123456789","123456789","123456789","123456789","123456789","123456789","5","6"],["123456789","123456789","5","3","123456789","123456789","123456789","123456789","123456789"],["123456789","123456789","6","1","123456789","123456789","9","123456789","123456789"]]

  choices는 각 빈칸마다 (0마다) 모든 숫자(digits)를 원소로 가지는 배열을 반환한다.

-}

-- Q3-2. expand의 타입은 무엇이며, head . expand . choices $ sudoku1의 결과를 작성하시오.
{-
  expand의 타입은 Matrix Choice -> [Grid] 이므로 [[[Char]]]을 입력으로 받고 [[[Char]]]을 출력으로 가진다.
  head . expand . choices $ sudoku1의 결과는 다음과 같다.
  ["114115711","111119411","361111118","721161111","111412111","111181193","411111156","115311111","116111911"]

  expand는 가능한 모든 조합에 대해 배열을 생성한다. sudoku가 성립하는지 아닌지는 알 수 없다.
-}

-- Q3-3. valid의 타입은 무엇이며, valid . head . expand . choices $ sudoku1의 결과를 작성하시오.
{-

  valid의 타입은 Grid -> Bool이고 [[Char]]을 입력으로 받고 Bool을 출력으로 가진다.
  valid . head . expand . choices $ sudoku1의 출력은 False이다.

  valid는 각 행, 열, 박스(3x3)에 대해서 sudoku 규칙을 만족하는지를 나타낸다.

-}

-- Q4. solve1은 수도쿠를 직관적으로 푸는 방법에 대한 명세로써
--     작성한 함수이다.
--
--     이 함수의 수도쿠 풀이 과정을 1단락 이내로 설명하시오.

{-

   solve1 함수는 choices 함수와 expand를 통해 가능한 모든 상태의 sudoku 보드를 만든 다음 valid를 통해 sudoku 규칙을 만족하는지를 계산한다. filter 함수를 통해서 valid를 통과한 sudoku 보드들만 남긴다고 볼 수 있다.

-}

-- | 2. Pruning
prune :: Matrix Choices -> Matrix Choices
prune =
  pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map pruneRow . f

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove ones) row
  where
    ones = [d | [d] <- row]

remove :: Choices -> Choices -> Choices
remove xs [d] = [d]
remove xs ds = filter (`notElem` xs) ds

-- | 3. Single-cell expansion
expand1 :: Matrix Choices -> [Matrix Choices]
expand1 rows =
  [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (any smallest) rows
    (row1, cs : row2) = break smallest row
    smallest cs = length cs == n
    n = minimum (counts rows)

counts = filter (/= 1) . map length . concat

-- | 4. Final algorithm
solve2 :: Grid -> [Grid]
solve2 = search . choices

search :: Matrix Choices -> [Grid]
search cm
  | not (safe pm) = []
  | complete pm = [map (map head) pm]
  | otherwise = (concat . map search . expand1) pm
  where
    pm = prune cm

complete :: Matrix Choices -> Bool
complete = all (all single)

single [_] = True
single _ = False

safe :: Matrix Choices -> Bool
safe cm =
  all ok (rows cm)
    && all ok (cols cm)
    && all ok (boxs cm)

ok row = nodups [d | [d] <- row]

-- Q5. solve2는 수도쿠를 빠르게 풀도록 최적화한 함수이다.
--
--     solve1 대비 최적화한 방법을 한가지를 1단락 이내로 설명하시오.
--     (solve2에서 사용하는 함수를 모두 이해하지 않아도 됩니다.)
{-

  solve1에서는 가능한 모든 상태의 sudoku 보드를 만드는 expand . search를 사용했지만 solve2에서는 prune을 통해서 가능한 경우에 대해서만 계산하도록하고 가능하지 않는 경우에 대해서는 더 이상 계산하지 않도록 하였다. pruneRow 함수가 경우의 수를 줄이는 부분이라고 볼 수 있다. (겹치는 수가 있다면 계산에서 제외) 경우의 수를 계속해서 줄여가며 계산하기 때문에 계산 시간이 굉장히 줄어든다.

-}