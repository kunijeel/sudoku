-- | A Simple Sudoku Solver
--   27th September, 2007
--   In Chapter 05

-- 0. Basic data types

type Matrix a = [Row a]
type Row a    = [a]

type Grid     = Matrix Digit
type Digit    = Char

digits  :: [Digit]
digits  =  ['1'..'9']

blank   :: Digit -> Bool
blank   =  (== '0')

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

   A1. 

-}

sudoku1 :: Grid
sudoku1 = [
   "004005700",
   "000009400",
   "360000008",
   "720060000",
   "000402000",
   "000080093",
   "400000056",
   "005300000",
   "006100900"]

-- Q2. Grid 타입의 값을 입력받아 위의 스도쿠 예시와 같은 형태의 문자열을
--     출력하는 함수 display를 작성하시오.
--      (1) 빈칸은 밑줄(_)로 표시
--      (2) 각 칸 사이에 공백을 둔다.
--      (3) 한 줄에 9개의 빈칸 또는 숫자를 표시하고 줄바꿈 문자를 둔다.
--      (4) 총 9개 줄을 표시
--

{-

   A2. 

-}

display :: Grid -> String
display = unlines . map (unwords . map displayDigit)

displayDigit :: Digit -> String
displayDigit d = if blank d then "_" else [d]

-- | 1. Specification

solve1 :: Grid -> [Grid]
solve1 = filter valid . expand . choices

type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices = map (map choice)
 where choice d | blank d   = digits
                | otherwise = [d]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

valid  :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

nodups       :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = x `notElem` xs && nodups xs

rows :: Matrix a -> [Row a]
rows = id

cols          :: Matrix a -> [Row a]
cols [xs]     = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> [Row a]
boxs = map ungroup . ungroup . map cols .
       group . map group

ungroup          = concat
group []         = []
group (x:y:z:xs) = [x,y,z]:group xs

-- Q3.  solve1 함수가 아래와 같이 작성되었다.
-- 
--      solve1 :: Grid -> [Grid]
--      solve1 = filter valid . expand . choices
--
-- Q3-1. choices의 타입은 무엇이며, choices sudoku1의 결과를 작성하시오.
{-

   A3-1. choices의 타입은 'Grid -> Matrix Choices'이고 'choices sudoku1'의 결과는 [["123456789","123456789",
   "4","123456789","123456789","5","7","123456789","123456789"],["123456789","123456789","123456789",
   "123456789","123456789","9","4","123456789","123456789"],["3","6","123456789","123456789","123456789",
   "123456789","123456789","123456789","8"],["7","2","123456789","123456789","6","123456789","123456789",
   "123456789","123456789"],["123456789","123456789","123456789","4","123456789","2","123456789",
   "123456789","123456789"],["123456789","123456789","123456789","123456789","8","123456789","123456789",
   "9","3"],["4","123456789","123456789","123456789","123456789","123456789","123456789","5","6"],["123456789",
   "123456789","5","3","123456789","123456789","123456789","123456789","123456789"],["123456789","123456789","6",
   "1","123456789","123456789","9","123456789","123456789"]] 이다.

   스도쿠 그리드의 셀이 비어있다면 입력할 수 있는 모든 수인 "123456789"를 나타내고 비어있지 않다면 셀에 작성된 숫자를 나타내는 것이다.

-}

-- Q3-2. expand의 타입은 무엇이며, head . expand . choices $ sudoku1의 결과를 작성하시오.
{-

   A3-2. expand의 타입은 'Matrix Choices -> [Grid]'이다. 'head . expand . choices $ sudoku1'의 결과는
   ["114115711","111119411","361111118","721161111","111412111","111181193","411111156","115311111","116111911"]이다.

   expand 함수는 'Matrix Choices'를 입력받아 가능한 모든 스도쿠 그리드를 배열로 반환한다. head 함수는 스도쿠 그리드 배열의 첫 번째 인자를 반환한다.

-}

-- Q3-3. valide의 타입은 무엇이며, valid . head . expand . choices $ sudoku1의 결과를 작성하시오.
{-

   A3-3. valid의 타입은 'Grid -> Bool'이다. 'valid . head . expand . choices $ sudoku1'의 결과는 'False'이다.

   valid 함수는 주어진 스도쿠 그리드가 유효한지 검사한다. 모든 행, 열, 3x3박스에 대해 'nodups' 함수를 통해 중복 검사를 하고 중복 요소가 있으면
   'False'를 반환하고 없으면 'True'를 반환한다.

-}


-- Q4. solve1은 수도쿠를 직관적으로 푸는 방법에 대한 명세로써
--     작성한 함수이다.
-- 
--     이 함수의 수도쿠 풀이 과정을 1단락 이내로 설명하시오.

{-

   A4. 'solve1'은 스도쿠 퍼즐을 해결하기 위한 직관적인 접근 방식을 구현한다. 'choices' 함수를 활용해 스도쿠 그리드의 각 셀에 작성할 수 있는 모든 숫자를 
   'Matrix Choices' 형태로 반환한다. 이후 'expand' 함수를 사용해 가능한 모든 스도쿠 그리드 조합을 반환한다. 마지막으로 'valid' 함수를 통해
   스도쿠 그리드가 유효한지 검증한다. 만약 스도쿠 그리드가 유효하다면 'True'를 반환하고 유효하지 않다면 'False'를 반환한다. 'filter' 함수를 사용해
   최종적으로 유효한 스도쿠 그리드를 반환한다.

-}

-- | 2. Pruning

prune :: Matrix Choices -> Matrix Choices
prune =
 pruneBy boxs . pruneBy cols . pruneBy rows
 where pruneBy f = f . map pruneRow . f

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove ones) row
 where ones = [d | [d] <- row]

remove :: Choices -> Choices -> Choices
remove xs [d] = [d]
remove xs ds  = filter (`notElem` xs) ds

-- | 3. Single-cell expansion

expand1   :: Matrix Choices -> [Matrix Choices]
expand1 rows =
 [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
 where
 (rows1,row:rows2) = break (any smallest) rows
 (row1,cs:row2)    = break smallest row
 smallest cs       = length cs == n
 n                 = minimum (counts rows)

counts = filter (/=1) . map length . concat

-- | 4. Final algorithm

solve2 :: Grid -> [Grid]
solve2 =  search . choices

search :: Matrix Choices -> [Grid]
search cm
 |not (safe pm)  = []
 |complete pm    = [map (map head) pm]
 |otherwise      = (concat . map search . expand1) pm
 where pm = prune cm

complete :: Matrix Choices -> Bool
complete = all (all single)

single [_] = True
single _   = False

safe :: Matrix Choices -> Bool
safe cm = all ok (rows cm) &&
          all ok (cols cm) &&
          all ok (boxs cm)

ok row = nodups [d | [d] <- row]

-- Q5. solve2는 수도쿠를 빠르게 풀도록 최적화한 함수이다.
-- 
--     solve1 대비 최적화한 방법을 한가지를 1단락 이내로 설명하시오.
--     (solve2에서 사용하는 함수를 모두 이해하지 않아도 됩니다.)
{-

   A5. 'solve2' 함수는 'prune' 함수를 사용하여 최적화를 달성한다. 'prune' 함수는 스도쿠 그리드의 각 셀에서 불가능한 숫자들을 제거한다.
   스도쿠 그리드의 각 행, 열, 3x3박스에서 이미 확정된 숫자를 바탕으로 나머지 셀들의 가능한 숫자를 줄일 수 있고 이러한 방식은 스도쿠 해결 과정이
   더 빠르고 효율적으로 수행될 수 있도록 한다. 'solve1'은 모든 가능한 조합을 생성한 뒤 유효성을 검사하지만 'solve2'는 불필요한 조합을 미리 제거하여
   탐색 범위를 좁힌다.

-}