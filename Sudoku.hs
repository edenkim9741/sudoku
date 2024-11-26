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

{-
모든 원소마다 chr 함수를 적용하여 Char 타입으로 변하도록 구성함.
-}

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
x :: [a] -> [a]
x [] = []
x a = head a : x a


display :: Grid -> String
display sudoku = if length lines == 1 then head lines
                 else head lines ++ "\n" ++ display lines
   where lines = x sudoku


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

   A3-1.

-}

-- Q3-2. expand의 타입은 무엇이며, head . expand . choices $ sudoku1의 결과를 작성하시오.
{-

   A3-2.

-}

-- Q3-3. valide의 타입은 무엇이며, valid . head . expand . choices $ sudoku1의 결과를 작성하시오.
{-

   A3-3.

-}

-- Q4. solve1은 수도쿠를 직관적으로 푸는 방법에 대한 명세로써
--     작성한 함수이다.
--
--     이 함수의 수도쿠 풀이 과정을 1단락 이내로 설명하시오.

{-

   A4.

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

   A5.

-}