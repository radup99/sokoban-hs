import System.Console.ANSI
import Foreign.C.Types
import Data.Char


area1 = [ "  ###    ",
          "  #x#    ",
          "  #.#### ",
          "###...x# ",
          "#x...### ",
          "####.#   ",
          "   #x#   ",
          "   ###   ",
          "         " ]

area2 = [ "#######  ",
          "#.....#  ",
          "#...x.###",
          "#.......#",
          "#.....x.#",
          "#.....x.#",
          "##.######",
          " #x#     ",
          " ###     " ]

area3 = [ " ####    ",
          " #..###  ",
          " #....#  ",
          "###.#.## ",
          "#x#.#..# ",
          "#x...#.# ",
          "#x.....# ",
          "######## ",
          "         " ]
          
area4 = [ "######   ",
          "#....#   ",
          "#....##  ",
          "#..#xx###",
          "##..x...#",
          " #......#",
          " ########",
          "         ",
          "         " ]
          
area5 = [ "  ###### ",
          "  #....# ",
          "###....# ",
          "#...xx.# ",
          "#..xxx## ",
          "####..#  ",
          "  #####  ",
          "         ",
          "         " ]
          
area6 = [ "#####    ",
          "#...#    ",
          "#...# ###",
          "#...# #x#",
          "###.###x#",
          " ##....x#",
          " #...#..#",
          " #...####",
          " #####   " ]
          
          
area7 = [ "  ####   ",
          "  #xx#   ",
          " ##.x##  ",
          " #...x#  ",
          "##....## ",
          "#..#...# ",
          "#......# ",
          "######## ",
          "         " ]
          

-- are scopul de a "regla" ecranul inainte de primul clearScreen.          
fixScreen :: IO()
fixScreen = putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

-- functie care returneaza linia nr. x dintr-o matrice
areaLine :: [[Char]] -> Int -> Int -> [Char]
areaLine area x currentIndex = if x == currentIndex then (head area) else (areaLine (tail area) x (currentIndex+1) )

-- functie care returneaza caracterul nr. y dintr-o linie a matricei
element' :: [Char] -> Int -> Int -> Char
element' lista y currentIndex = if y == currentIndex then (head lista) else (element' (tail lista) y (currentIndex+1) )

-- functie care returneaza caracterul de pe randul x si coloana y dintr-o matrice
-- elementele dinafara matricei sunt considerate spatii goale (necesar pentru functia wall care creaza conturul nivelului)
element :: [[Char]] -> Int -> Int -> Char
element area 0  y = ' '
element area 10 y = ' '
element area x 0  = ' '
element area x 10 = ' '
element area x y = element' (areaLine area x 1) y 1


-- tip de date folosit pentru a retine pozitia curenta a jucatorului sau a unei cutii
data Point = P Int Int deriving Eq 

row :: Point -> Int
row (P x y) = x

col :: Point -> Int
col (P x y) = y


-- returneaza cutia nr. x dintr-o lista de cutii
boxAt' :: Int -> Int -> [Point] -> Point
boxAt' currentIndex x boxList = if x == currentIndex then (head boxList) else (boxAt' (currentIndex+1) x (tail boxList))

boxAt :: Int -> [Point] -> Point
boxAt x boxList = boxAt' 1 x boxList

-- verifica daca cutia primita ca parametru se afla in boxList 
findBox' :: Int -> Point -> [Point] -> Bool
findBox' currentIndex box boxList = if (boxList == []) then False
                              else if ( row box == row (head(boxList)) && col box == col(head(boxList)) ) then True
                              else findBox' (currentIndex+1) box (tail boxList)
                                 
findBox :: Point -> [Point] -> Bool
findBox box boxList = findBox' 1 box boxList


-- verifica daca fiecare 'x' din area a fost acoperit de o cutie (conditia de castigare a nivelului)
isLevelSolved :: [[Char]] -> [Point] -> Bool
isLevelSolved area boxList = if (boxList == []) then True
                             else if ( element area (row (head boxList)) (col (head boxList)) == 'x') then isLevelSolved area (tail boxList)
                             else False


-- primeste lista de cutii si returneaza o lista noua, in care cutia impinsa 
updateBoxList' :: Int -> [Point] -> [Point] -> Int -> Int -> Int -> Int -> [Point]
updateBoxList' currentIndex boxList currentList x y dx dy = 
            --if (boxList == [] || (findBox (P x y) boxList) == False) then boxList
            if ( row (head currentList) == x && col (head currentList) == y ) then
            
                if (currentIndex == 1) then let
                    box = P (x+dx) (y+dy)
                    in ([box] ++ (tail boxList))
                 
                else if (currentIndex == (length boxList)) then let
                    box = P (x+dx) (y+dy)
                    (list1, list2) = splitAt ((length boxList)-1) boxList
                    in (list1 ++ [box])
                
                else let
                   box = P (x+dx) (y+dy)
                   (list1, list2) = splitAt (currentIndex-1) boxList
                   in (list1 ++ [box] ++ (tail list2))
            else
                updateBoxList' (currentIndex+1) boxList (tail currentList) x y dx dy
                
updateBoxList :: [Point] -> [Point] -> Int -> Int -> Int -> Int -> [Point]
updateBoxList boxList currentList x y dx dy = updateBoxList' 1 boxList currentList x y dx dy


-- area:     matricea nivelului
-- lvl:      numarul nivelului
-- m:        nr. de miscari efectuate
-- x, y:     coordonatele caracterului curent la care a ajuns functia showArea'
-- c:        punctul in care se afla jucatorul
-- boxList: lista coordonatelor cutiilor din nivel
showArea' :: [[Char]] -> Int -> Int -> Point -> [Point] -> IO ()
showArea' area x y c boxList = if x == 10 then (putChar('\n')) 
                               else if y == 10 then (putChar('\n')) >> (showArea' area (x+1) 1 c boxList)
                               else if (x == (row c) && y == (col c) ) then (character) >> (showArea' area x (y+1) c boxList)
                               else if ( findBox (P x y) boxList  ) then (showBox) >> (showArea' area x (y+1) c boxList)
                               else (mapItem area x y) >> (showArea' area x (y+1) c boxList)

showArea :: [[Char]] -> Point -> [Point] -> IO()
showArea area c boxList = showArea' area 1 1 c boxList


character :: IO()
character = (setSGR [SetColor Foreground Vivid Cyan]) >> (putChar('@'))

showBox :: IO()
showBox = (setSGR [SetColor Foreground Dull Yellow]) >> (putChar('O'))

mapItem :: [[Char]] -> Int -> Int -> IO()
mapItem area x y = case element area x y of
                    '#' -> (setSGR [SetColor Foreground Vivid White]) >> (wall area x y) -- perete
                    ' ' -> (setSGR [SetColor Foreground Vivid White]) >> (putChar(' '))  -- spatiul dinafara conturului
                    '.' -> (setSGR [SetColor Foreground Vivid White]) >> (putChar('.'))  -- spatiul dinauntrul conturului
                    'x' -> (setSGR [SetColor Foreground Vivid Blue])  >> (putChar('x'))  -- locul in care trebuie pusa o cutie
                    
                    
--algoritm care "deseneaza" conturul nivelului
wall :: [[Char]] -> Int -> Int -> IO()
wall area x y = if (element area x (y-1) /= '#' && element area x (y+1) /= '#') then
                    putChar('│')

                else if element area (x-1) y /= '#' then
                    if element area (x+1) y /= '#'  then
                        putChar('─')
                    else
                        if element area x (y-1) /= '#' then putChar('┌')
                        else if element area x (y+1) /= '#' then putChar('┐')
                        else putChar('┬')
                    
                else if element area (x+1) y /= '#' then
                        if element area (x-1) y /= '#' then
                            putChar('─')
                        else
                            if element area x (y-1) /= '#' then putChar('└')
                            else if element area x (y+1) /= '#' then putChar('┘')
                            else putChar('┴')
                        
                else if element area x (y-1) /= '#' then
                        if element area x (y+1) /= '#' then
                            putChar('│')
                        else
                            if element area (x-1) y == '#' then putChar('├')
                            else putChar('┌')
                        
                else if element area x (y+1) /= '#' then
                         if element area x (y-1) /= '#' then
                            putChar('│')
                        else
                            if element area (x+1) y == '#' then putChar('┤')
                            else putChar('┐')
                        
                else
                    putChar('┼')


-- functie care returneaza tasta care a fost apasata
-- sursa: https://stackoverflow.com/questions/2983974/haskell-read-input-character-from-console-immediately-not-after-newline
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt


movePlayer :: [[Char]] -> Int -> Int -> Point -> [Point] -> IO()
movePlayer area lvl m c boxList = do
                    if (isLevelSolved area boxList) then
                        endScreen area lvl m c boxList
                    else do
                    clearScreen
                    putStr "\nLevel "
                    putStr(show lvl)
                    putStr "\nMoves: "
                    putStr(show m)
                    putStr "\n\n"
                    showArea area c boxList
                    putStr "Reset: R"
                    putStr "\nMenu:  M"
                    putStr "\nExit:  E"
                    putStr "\nMove:  W/S/A/D"
                    setSGR [SetColor Foreground Vivid White]
                    x <- getHiddenChar
                    
                    case x of
                        'w'  -> check area lvl m c boxList (-1) 0
                        's'  -> check area lvl m c boxList 1 0
                        'a'  -> check area lvl m c boxList 0 (-1)
                        'd'  -> check area lvl m c boxList 0 1
                        'm'  -> menu
                        'r'  -> loadLevel lvl
                        'e'  -> putStrLn "\n" --incheie executia programului
                        _    -> (clearScreen) >> (movePlayer area lvl m c boxList)
  
  
-- dx si dy reprezinta directia in care se efectueaza miscarea jucatorului
-- de exemplu, pentru stanga: (dx,dy) = (0,-1) (scade nr. coloanei pe care se afla jucatorului cu 1 dar randul ramane aceeasi)
check :: [[Char]] -> Int -> Int -> Point -> [Point] -> Int -> Int -> IO()
check area lvl m c boxList dx dy = 

                      if ( findBox (P ((row c) + dx) ((col c) + dy)) boxList == True ) then -- se incearca mutarea unei cutii
                        if (element area ((row c) + 2*dx) ((col c) + 2*dy) /= '#') then     -- verifica sa nu fie un perete in patratul in care ar fi mutata cutia
                            if ( findBox (P ((row c) + 2*dx) ((col c) + 2*dy)) boxList == False ) then -- verifica sa nu fie o cutie in patratul in care ar fi mutata cutia
                                movePlayer area lvl (m+1) (P ((row c)+dx) ((col c)+dy)) ( updateBoxList boxList boxList ((row c) + dx) ((col c) + dy) dx dy) -- se realizeaza mutarea jucatorului si a cutiei impinse
                            else
                                movePlayer area lvl m c boxList -- nu este realizata mutarea jucatorului sau a vreunei cutii
                        else
                            movePlayer area lvl m c boxList
                        
                      else if element area ((row c)+dx) ((col c)+dy) /= '#' then
                        movePlayer area lvl (m+1) (P ((row c)+dx) ((col c)+dy)) boxList 
                      else
                        movePlayer area lvl m c boxList


endScreen :: [[Char]] -> Int -> Int -> Point -> [Point] -> IO()
endScreen area lvl m c boxList = do
    clearScreen
    putStr "\nLevel "
    putStr(show lvl)
    putStr "\nMoves: "
    putStr(show m)
    putStr "\n\n"
    showArea area c boxList
    setSGR [SetColor Foreground Vivid White]
    putStr "Reset: R"
    putStr "\nMenu:  M"
    putStr "\nExit:  E"
    putStr "\nLevel solved! Press any key to continue: "
    x <- getHiddenChar
    case x of
      'm' -> menu
      'e' -> putStrLn "\n" --incheie executia programului
      'r' -> loadLevel lvl
      _   -> loadLevel (lvl+1)
    

-- fiecare nivel necesita urmatorii parametri (prezenti si in functia showArea, care afiseaza nivelul): 
-- numarul nivelului (necesar pentru ca jocul sa poata treaca de la un nivel la urmatorul);
-- numarul miscarilor (initializat cu 0);
-- un Point ce reprezinta pozitia initiala a jucatorului;
-- o lista de Points ce reprezinta pozitiile initiale ale cutiilor. 
loadLevel:: Int -> IO()
loadLevel lvl = case lvl of
                     1 -> movePlayer area1 1 0 (P 5 5) [(P 4 4), (P 5 3), (P 6 5), (P 4 6)]
                     2 -> movePlayer area2 2 0 (P 4 4) [(P 5 6), (P 5 3), (P 5 4), (P 5 5)]
                     3 -> movePlayer area3 3 0 (P 2 3) [(P 3 4), (P 6 4), (P 7 6)]
                     4 -> movePlayer area4 4 0 (P 2 3) [(P 3 3), (P 3 4), (P 5 7)]
                     5 -> movePlayer area5 5 0 (P 5 2) [(P 5 3), (P 4 4), (P 3 4), (P 3 5), (P 3 6)]
                     6 -> movePlayer area6 6 0 (P 3 4) [(P 3 3), (P 4 3), (P 4 4)]
                     7 -> movePlayer area7 7 0 (P 5 3) [(P 5 4), (P 6 5), (P 6 6), (P 4 5)]
                     _ -> menu
    
    
menu :: IO()
menu = do
       clearScreen
       putStr "\nSelect level (1-7): "
       x <- getLine
       case x of
            "1"    -> loadLevel 1
            "2"    -> loadLevel 2
            "3"    -> loadLevel 3
            "4"    -> loadLevel 4
            "5"    -> loadLevel 5
            "6"    -> loadLevel 6
            "7"    -> loadLevel 7
            "exit" -> putStrLn "\n" --incheie executia programului
            _      -> menu


main :: IO()
main = do
       fixScreen
       menu