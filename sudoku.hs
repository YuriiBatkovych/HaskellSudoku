import System.IO;
import System.Environment;

-- w rozwiązaniu korzystam z pomysłu backtracking algorithm, dokładny opis którego można znaleść na następujących stronach:
-- https://en.wikipedia.org/wiki/Sudoku_solving_algorithms
-- https://www.geeksforgeeks.org/sudoku-backtracking-7/

-- W projekcie przyjmuje założenie, że treść sudoku do rozwiązania znajduje się w pliku tektowym , ścieżka do którego
-- będzie podana jako argument do programu. Format zapisu sudoku w pliku jest następujący : 
-- kolejne rzędy są podawane w odrębnych liniach 
-- liczby w rzędach są oddzielone conajmniej jedną spacją
-- pusta komórka jest reprezentowana przez liczbę 0

-- Rozwiązanie sudoku wyświetlone zostanie na wyjściu standardowym

type Puzzle = [[Int]]

row :: Int -> Puzzle -> [Int]
row = flip (!!)

column :: Int -> Puzzle -> [Int]
column = map.flip (!!)

-- zwaraca indeksy dla utworzenia listy elementów małego kwadratu w którym znajduje się
-- krotka o rzedzie x i columnie y
blockIndexes :: Int -> Int -> [Int]
blockIndexes x y = [dx*9 + dy |  dx <- [row_start..(row_start+2)], dy <- [col_start..(col_start+2)]]
    where row_start = (x `div` 3) * 3
          col_start = (y `div` 3) * 3

-- zwraca liste elementów małego kwadratu w którym znajduje się
-- krotka o rzedzie r i columnie c
subSquare :: Int -> Int -> Puzzle -> [Int]
subSquare r c = flip map (blockIndexes r c).((!!).concat)

--funkcja wykorzystana do szukania indeksów wierszy w Sudoku i indeksów elementów w wierszu
indexOf :: Eq a => a -> [a] -> Int
indexOf _ [] = -10
indexOf el (x:xs)
        | x == el = 0
        | otherwise = 1 + indexOf el xs

-- funkcja zwraca pare (row, column) - współrzędne nieuzupełnionej krotki sudoku
-- lub (-1, -1), jeżeli sudoku jest kompletnie uzupełnione
findNextEmpty :: Puzzle -> (Int, Int)
findNextEmpty puzzle
    | null r = (-1,-1)
    | otherwise = (indexOf r puzzle, indexOf 0 r)
    where rowWithEmpty [] = []
          rowWithEmpty (x:xs) = if 0 `elem` x then x else rowWithEmpty xs
          r = rowWithEmpty puzzle

-- sprawdzamy czy zgadnięty element x nie znajduje się już w wierszu r lub kolumnie c
-- lub subkwadracie 3x3
isValid :: Int -> Int -> Int -> Puzzle -> Bool
isValid x r c puzzle = x `notElem` set
    where set = row r puzzle ++ column c puzzle ++ subSquare r c puzzle

-- wstawiamy element x w krotkę o wierszu r i kolumnie c
add :: Int -> Int -> Puzzle -> Int -> Puzzle
add r c puzzle x = take r puzzle ++ [take c middle ++ [x] ++ drop (c+1) middle] ++ drop (r+1) puzzle
        where middle = puzzle !! r

--funkcja dopasowująca prawidłową liczbę na puste miejsce
guess :: Int -> Int -> Int -> Puzzle -> (Puzzle, Bool)
guess _ _ 10 puzzle = (puzzle , False)
guess r c g puzzle
        | isValid g r c puzzle && snd nextTry = nextTry
        | otherwise = guess r c (g+1) puzzle
            where nextTry = solveSudoku $ add r c puzzle g

-- funkcja przyjmuje sudoku do rozwiązania (Puzzle) oraz zwraca 
-- 1 - (rozwiazane sudoku, True) w przypadku istnienia rozwiązania
-- 2 - (wejściowe sudoku, False)  w przypadku nieistnienia rozwiązania 
solveSudoku :: Puzzle -> (Puzzle, Bool)
solveSudoku puzzle
  | row == -1 && col == -1 = (puzzle, True)
  | snd nextTry = nextTry
  | otherwise = (puzzle, False)
  where
      (row, col) = findNextEmpty puzzle
      nextTry = guess row col 1 puzzle


stringToList :: String -> [Int]
stringToList = map read.words

-- wczytywanie sudoku do rozwiązania
readTask :: Handle -> IO Puzzle
readTask handle = do
    eof<-hIsEOF handle
    if eof then return []
    else do
        line <- hGetLine handle
        let row = stringToList line
        puzzle <- readTask handle
        return (row:puzzle)

showPuzzle :: Puzzle -> IO()
showPuzzle [] = return ()
showPuzzle (x:xs) = do
                print x
                showPuzzle xs

main :: IO ()
main = do
        (fileName:_) <- getArgs
        fileHandle <- openFile fileName ReadMode
        puzzle <- readTask fileHandle
        hClose fileHandle
        putStrLn "----------------------"
        putStrLn "Zadanie : "
        showPuzzle puzzle
        putStrLn "---------------------"
        putStrLn "Rozwiazanie : "
        let solvedPuzzle = solveSudoku puzzle
        if snd solvedPuzzle then showPuzzle (fst solvedPuzzle)
        else putStrLn "Rozwiazanie nie istnieje"
        
