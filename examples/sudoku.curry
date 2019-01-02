-----------------------------------------------------------------------------
--- Solving Su Doku puzzles in Curry with a web-based interface
---
--- Note that this example requires the CLPFD solver provided with PAKCS.
---
--- @author Michael Hanus
--- @version January 2019
-----------------------------------------------------------------------------

import CLPFD
import HTML.Base
import List         ( transpose )
import WUI

import Control.AllSolutions ( getOneSolution ) -- requires package `searchtree`

-----------------------------------------------------------------------------
-- Solving a Su Doku puzzle represented as a matrix of numbers (possibly free
-- variables):
sudoku :: [[Int]] -> Bool
sudoku m =
 domain (concat m) 1 9 &             -- define domain of all digits
 all allDifferent m  &               -- all rows contain different digits
 all allDifferent (transpose m)  &   -- all columns have different digits
 all allDifferent (squares m) &      -- all 3x3 squares are different
 labeling [FirstFailConstrained] (concat m)
 where
  -- translate a matrix into a list of small 3x3 squares
  squares :: Eq a => [[a]] -> [[a]]
  squares [] = []
  squares (l1:l2:l3:ls) = group3Rows [l1,l2,l3] ++ squares ls
  
  group3Rows l123 = if head l123 == [] then [] else
   concatMap (take 3) l123 : group3Rows (map (drop 3) l123)

-- Compute one solution to a SuDoKu puzzle by encapsulated search:
solveSudoku :: [[Int]] -> IO (Maybe [[Int]])
solveSudoku s = getOneSolution (\m -> m=:=map (map transDigit) s &> sudoku m)
 where transDigit i = if i==0 then _ else i

-----------------------------------------------------------------------------
-- the main form to input SuDoKu puzzles:
initForm :: [[Int]] -> IO HtmlForm
initForm s = let (hexp,handler) = wui2html wSudoku s solveForm in
  return $ standardForm "SuDoku" [hexp, wuiHandler2button "Solve" handler]

-- the specification of the SuDoku WUI:
wSudoku :: WuiSpec [[Int]]
wSudoku = wMatrix (wSelect (\i->if i==0 then " " else show i) [0..9])

solveForm :: [[Int]] -> IO HtmlForm
solveForm m = do
  mbsol <- solveSudoku m
  return $ standardForm "SuDoku" $
    maybe [h1 [htxt "No solution"]]
          (\sol -> [fst (wui2html (wMatrix (wConstant (\d->HtmlText ("&nbsp;"++show d++"&nbsp;"))))
                        sol initForm)])
          mbsol

-- An empty initial form:
formEmpty :: IO HtmlForm
formEmpty = initForm (map (const (take 9 (repeat 0))) [1..9])

formExample :: IO HtmlForm
formExample =
  initForm [ [9,0,0,2,0,0,5,0,0]
           , [0,4,0,0,6,0,0,3,0]
           , [0,0,3,0,0,0,0,0,6]
           , [0,0,0,9,0,0,2,0,0]
           , [0,0,0,0,5,0,0,8,0]
           , [0,0,7,0,0,4,0,0,3]
           , [7,0,0,0,0,0,1,0,0]
           , [0,5,0,0,2,0,0,4,0]
           , [0,0,1,0,0,6,0,0,9] ]

-- Generate cgi program with:
-- curry makecgi -cpm -m formExample -o ~/public_html/sudoku.cgi sudoku
