{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Cell = H | T | O | G | Space deriving (Eq, Ord)

instance Show Cell
    where
        show H = "!"
        show T = "*"
        show O = "@"
        show G = "#"
        show Space = " "

data Game = Game
    { list :: [Cell]
    , hunter :: Position
    , targets :: [Target] 
    , gateways :: [(Position, Position)]
    , rows :: Int
    , columns :: Int
    } deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString (Game {list = l, rows = r, columns = c}) = intercalate "\n"
    [(concatMap show $ drop (x * c) $ take (c * (x + 1)) l)| x <- [0..(r - 1)]]


instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame new_rows new_columns = 
    (Game {list = newlist, hunter = (1, 1), rows = new_rows, columns = new_columns, gateways = [], targets = []})
    where
        newlist = (replicate new_columns O) ++ 
                  ([O] ++ [H] ++ (replicate (new_columns - 3) Space) ++ [O]) ++ 
                  (foldl (\acc x -> x ++ acc) [] (replicate (new_rows - 3) ([O] ++ (replicate (new_columns - 2) Space) ++ [O]))) ++ 
                  (replicate new_columns O)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
isInGateways :: Position -> [(Position, Position)] -> Maybe (Position, Position)
isInGateways (x, y) gateways_list = iter gateways_list 
                    where
                        iter [] = Nothing
                        iter g_list = if ((x,y) == fst (head g_list) || (x, y) == snd (head g_list)) then Just (head g_list) else iter (tail g_list)

addHunter :: Position -> Game -> Game
addHunter (x, y) game@(Game {list = l, hunter = (xh, yh), rows = r, columns = c, gateways = gateways_list, targets = t}) =
    if x <= r && x >= 1 && y <= c && y >= 1 && ((l !! n) == Space || (l !! n) == G) then
        (Game {list = newlist2, hunter = (x, y), rows = r, columns = c, targets = t, gateways = gateways_list})
    else
        game
    where
        n = x * c + y
        n2 = xh * c + yh
        newlist1 = take n2 l ++ [element] ++ drop (n2 + 1) l
            where
                element = if (isInGateways (xh, yh) gateways_list) /= Nothing  then G else Space
        newlist2 = take n newlist1 ++ [H] ++ drop (n + 1) newlist1

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget new_target_behave pos@(x, y) game@(Game {list = l, rows = r, targets = target_list, hunter = hnt, columns = c, gateways = glist}) =
        if x <= r && x >= 1 && y <= c && y >= 1 && ((l !! n) == Space || (l !! n) == G) then
            (Game {list = newlist, targets = new_targets, hunter = hnt, rows = r, columns = c, gateways = glist})
        else
            game
        where
            n = x * c + y
            newlist = take n l ++ [T] ++ drop (n + 1) l
            new_targets = target_list ++ [(Target pos new_target_behave)]


{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway ((x1, y1), (x2, y2)) game@(Game {list = l, rows = r, columns = c, gateways = gateway_list, hunter = hnt, targets = t}) = 
    if x1 <= r && x1 >= 1 && y1 <= c && y1 >= 1 && x2 <= r && x2 >= 1 && y2 <= c && y2 >= 1 && (l !! n1) /= O && (l !! n2) /= O then
        (Game {list = newlist2, gateways = new_gateways, rows = r, columns = c, hunter = hnt, targets = t})
    else
        game
    where
        n1 = x1 * c + y1
        n2 = x2 * c + y2
        newlist1 = take n1 l ++ [G] ++ drop (n1 + 1) l
        newlist2 = take n2 newlist1 ++ [G] ++ drop (n2 + 1) newlist1
        new_gateways = gateway_list ++ [((x1, y1), (x2, y2))]
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle (x, y) game@(Game {list = l, rows = r, columns = c, hunter = hnt, gateways = glist, targets = t}) =
    if x <= r && x >= 1 && y <= c && y >= 1 && (l !! n) == Space then
        (Game {list = newlist, rows = r, columns = c, hunter = hnt, gateways = glist, targets = t})
    else
        game
    where
        n = x * c + y
        newlist = take n l ++ [O] ++ drop (n + 1) l

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos@(x, y) (Game {list = l, columns = c, gateways = list_gateway})
    | (l !! n) == Space || (l !! n) == T = Just pos
    | (l !! n) == G =  if (x,y) == (snd apelare_functie) then Just (fst apelare_functie) else Just (snd apelare_functie)
    | otherwise = Nothing
    where
        n = x * c + y
        apelare_functie = fromJust (isInGateways (x, y) list_gateway)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

getPosTarget :: Target -> Position
getPosTarget (Target {position = p}) = p

getBehavTarget :: Target -> Behavior
getBehavTarget (Target {behavior = b}) = b

goEast :: Behavior
goEast (x, y) game@(Game {gateways = list_gateway, targets = list_targets})
    | (attemptMove (x, y + 1) game) /= Nothing = (Target {position = fromJust (attemptMove (x, y + 1) game), behavior = getBehavTarget found_target})
    | otherwise = if ((isInGateways (x, y) list_gateway) /= Nothing) 
                    then (Target {position = pos_other_gateway, behavior = getBehavTarget found_target})
                    else found_target
    where
        found_target = (head (foldl (\acc t -> (if (getPosTarget t) == (x, y) then [t] ++ acc else acc)) [] list_targets))
        pos_other_gateway = if (x,y) == (snd apelare_functie) then (fst apelare_functie) else (snd apelare_functie)
        apelare_functie = fromJust (isInGateways (x, y) list_gateway)


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x, y) game@(Game {gateways = list_gateway, targets = list_targets})
    | (attemptMove (x, y - 1) game) /= Nothing = (Target {position = fromJust (attemptMove (x, y - 1) game), behavior = getBehavTarget found_target})
    | otherwise = if ((isInGateways (x, y) list_gateway) /= Nothing) 
                    then (Target {position = pos_other_gateway, behavior = getBehavTarget found_target})
                    else found_target
    where
        found_target = (head (foldl (\acc t -> (if (getPosTarget t) == (x, y) then [t] ++ acc else acc)) [] list_targets))
        pos_other_gateway = if (x,y) == (snd apelare_functie) then (fst apelare_functie) else (snd apelare_functie)
        apelare_functie = fromJust (isInGateways (x, y) list_gateway)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) game@(Game {gateways = list_gateway, targets = list_targets})
    | (attemptMove (x - 1, y) game) /= Nothing = (Target {position = fromJust (attemptMove (x - 1, y) game), behavior = getBehavTarget found_target})
    | otherwise = if ((isInGateways (x, y) list_gateway) /= Nothing) 
                    then (Target {position = pos_other_gateway, behavior = getBehavTarget found_target})
                    else found_target
    where
        found_target = (head (foldl (\acc t -> (if (getPosTarget t) == (x, y) then [t] ++ acc else acc)) [] list_targets))
        pos_other_gateway = if (x,y) == (snd apelare_functie) then (fst apelare_functie) else (snd apelare_functie)
        apelare_functie = fromJust (isInGateways (x, y) list_gateway)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x, y) game@(Game {gateways = list_gateway, targets = list_targets})
    | (attemptMove (x + 1, y) game) /= Nothing = (Target {position = fromJust (attemptMove (x + 1, y) game), behavior = getBehavTarget found_target})
    | otherwise = if ((isInGateways (x, y) list_gateway) /= Nothing)  
                    then (Target {position = pos_other_gateway, behavior = getBehavTarget found_target})
                    else found_target
    where
        found_target = (head (foldl (\acc t -> (if (getPosTarget t) == (x, y) then [t] ++ acc else acc)) [] list_targets))
        pos_other_gateway = if (x,y) == (snd apelare_functie) then (fst apelare_functie) else (snd apelare_functie)
        apelare_functie = fromJust (isInGateways (x, y) list_gateway)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior
bounce i (x, y) game@(Game {list = l, columns = c}) = new_target
    where
        new_target
            | i == 1 = if ((l !! ((x + 1) * c + y)) == O) 
                        then (Target {position = fromJust (attemptMove (x - 1, y) game), behavior = bounce (-1)}) 
                        else (Target {position = fromJust (attemptMove (x + 1, y) game), behavior = bounce 1})
            | otherwise = if ((l !! ((x - 1) * c + y)) == O)
                            then (Target {position = fromJust (attemptMove (x + 1, y) game), behavior = bounce 1}) 
                            else (Target {position = fromJust (attemptMove (x - 1, y) game), behavior = bounce (-1)})

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

moveJustOneTarget:: Target -> Game -> [Cell]
moveJustOneTarget (Target {position = pos@(x, y), behavior = b}) game@(Game {list = l, columns = c, gateways = gateways_list}) = new_list
    where
        n = x * c + y
        newlist_aux = take n l ++ [element] ++ drop (n + 1) l
            where
                element = if (isInGateways (x, y) gateways_list) /= Nothing  then G else Space
        (x2, y2) = (getPosTarget (b pos game))
        n2 = x2 * c + y2
        new_list = take n2 newlist_aux ++ [T] ++ drop (n2 + 1) newlist_aux


moveTargets :: Game -> Game
moveTargets game@(Game {list = l, rows = r, columns = c, hunter = hnt, targets = list_targets, gateways = gateways_list}) = (Game {list = new_list, rows = r, hunter = hnt, columns = c,gateways = gateways_list, targets = new_targets})
    where
        new_targets = (foldl (\acc t -> [(getBehavTarget t) (getPosTarget t) game] ++ acc) [] list_targets)
        new_list = (foldl (\acc t -> moveJustOneTarget t (Game {list = acc, rows = r, hunter = hnt, columns = c, targets = list_targets, gateways = gateways_list})) l list_targets)

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (xh, yh) (Target {position = pos_target})
    | xh == (fst pos_target) && (yh == ((snd pos_target) - 1) || yh == ((snd pos_target) + 1)) = True
    | yh == (snd pos_target) && (xh == ((fst pos_target) - 1) || xh == ((fst pos_target) + 1)) = True
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

killOneTarget :: Target -> Game -> [Cell]
killOneTarget target@(Target {position = (x, y)}) (Game {list = l, columns = c, hunter = (xh, yh), gateways = gateways_list}) =
        if (isTargetKilled (xh, yh) target) == True then newlist else l
            where
                n = x * c + y
                newlist = take n l ++ [element] ++ drop (n + 1) l
                    where
                        element = if (isInGateways (x, y) gateways_list) /= Nothing  then G else Space


killTargets :: Game -> Game
killTargets (Game {list = l, rows = r, columns = c, hunter = hnt@(xh, yh), gateways = list_gateway, targets = list_targets}) =
    (Game {list = newlist, rows = r, columns = c, hunter = (xh, yh), gateways = list_gateway, targets = new_list_targets})
    where
        new_list_targets = (foldl (\acc t -> if (isTargetKilled (xh, yh) t) == True then acc else [t] ++ acc) [] list_targets)
        newlist =(foldl (\acc t -> killOneTarget t (Game {list = acc, rows = r, columns = c, hunter = hnt, targets = list_targets, gateways = list_gateway})) l list_targets)

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir canEliminate game@(Game {list = l, rows = r, columns = c, hunter = hnt@(xh, yh), gateways = list_gateway, targets = list_targets}) =
    if canEliminate == False
        then if dir == North 
            then if (l !! ((xh-1) * c + yh) == O)
                then (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets})
                else (Game {list = new_list1, rows = r, columns = c, hunter = newHunter1, gateways = list_gateway, targets = list_targets}) 
            else if dir == South
                then if (l !! ((xh+1) * c + yh) == O)
                        then (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets})
                        else (Game {list = new_list2, rows = r, columns = c, hunter = newHunter2, gateways = list_gateway, targets = list_targets})
                    else if dir == East
                            then if (l !! (xh * c + yh + 1) == O)
                                    then (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets})
                                    else (Game {list = new_list3, rows = r, columns = c, hunter = newHunter3, gateways = list_gateway, targets = list_targets})
                            else if (l !! (xh * c + yh - 1) == O)
                                    then (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets})
                                    else (Game {list = new_list4, rows = r, columns = c, hunter = newHunter4, gateways = list_gateway, targets = list_targets})
    else if dir == North
            then if (l !! ((xh-1) * c + yh) == O)
                    then (killTargets (moveTargets (killTargets (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets}))))
                    else (killTargets (moveTargets (killTargets (Game {list = new_list1, rows = r, columns = c, hunter = newHunter1, gateways = list_gateway, targets = list_targets}))))
            else if dir == South
                    then if (l !! ((xh+1) * c + yh) == O)
                            then (killTargets (moveTargets (killTargets (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets}))))
                            else (killTargets (moveTargets (killTargets (Game {list = new_list2, rows = r, columns = c, hunter = newHunter2, gateways = list_gateway, targets = list_targets}))))
                    else if dir == East
                            then if (l !! (xh * c + yh + 1) == O)
                                    then (killTargets (moveTargets (killTargets (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets}))))
                                    else (killTargets (moveTargets (killTargets (Game {list = new_list3, rows = r, columns = c, hunter = newHunter3, gateways = list_gateway, targets = list_targets}))))
                            else if (l !! (xh * c + yh - 1) == O)
                                    then (killTargets (moveTargets (killTargets (Game {list = l, rows = r, columns = c, hunter = hnt, gateways = list_gateway, targets = list_targets}))))
                                    else (killTargets (moveTargets (killTargets (Game {list = new_list4, rows = r, columns = c, hunter = newHunter4, gateways = list_gateway, targets = list_targets}))))
    where
        n = xh * c + yh
        list_aux = take n l ++ [element] ++ drop (n + 1) l
            where
                element = if (isInGateways (xh, yh) list_gateway) /= Nothing then G else Space
        n1 = if (attemptMove (xh - 1, yh) game) /= Nothing 
                then (fst (fromJust (attemptMove (xh - 1, yh) game))) * c + (snd (fromJust (attemptMove (xh - 1, yh) game)))
                else n
        new_list1 = take n1 list_aux ++ [H] ++ drop (n1 + 1) list_aux
        n2 = if (attemptMove (xh + 1, yh) game) /= Nothing 
                then (fst (fromJust (attemptMove (xh + 1, yh) game))) * c + (snd (fromJust (attemptMove (xh + 1, yh) game)))
                else n
        new_list2 = take n2 list_aux ++ [H] ++ drop (n2 + 1) list_aux
        n3 = if (attemptMove (xh, yh + 1) game) /= Nothing 
                then (fst (fromJust (attemptMove (xh, yh + 1) game))) * c + (snd (fromJust (attemptMove (xh, yh + 1) game)))
                else n
        new_list3 = take n3 list_aux ++ [H] ++ drop (n3 + 1) list_aux
        n4 = if (attemptMove (xh, yh - 1) game) /= Nothing 
                then (fst (fromJust (attemptMove (xh, yh - 1) game))) * c + (snd (fromJust (attemptMove (xh, yh - 1) game)))
                else n
        new_list4 = take n4 list_aux ++ [H] ++ drop (n4 + 1) list_aux
        newHunter1 = if (attemptMove (xh - 1, yh) game) /= Nothing 
                        then (fromJust (attemptMove (xh - 1, yh) game))
                        else hnt
        newHunter2 = if (attemptMove (xh + 1, yh) game) /= Nothing 
                        then (fromJust (attemptMove (xh + 1, yh) game))
                        else hnt
        newHunter3 = if (attemptMove (xh, yh + 1) game) /= Nothing 
                        then (fromJust (attemptMove (xh, yh + 1) game))
                        else hnt
        newHunter4 = if (attemptMove (xh, yh - 1) game) /= Nothing 
                        then (fromJust (attemptMove (xh, yh - 1) game))
                        else hnt
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game {targets = list_targets})
    | list_targets == [] = False
    | otherwise = True

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    -- Trebuie modificat --
    successors game =  [(dir, (advanceGameState dir False game)) | dir <- [North, South, East, West]]
    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game {hunter = (xh, yh), targets = list_targets}) = iter list_targets
        where
            iter [] = False
            iter targets_list = if (isTargetKilled (xh, yh) (head targets_list)) == True then True else iter (tail targets_list)

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h (Game {hunter = (xh, yh), targets = list_targets}) = iter list_targets 100
        where
            iter [] acc = acc
            iter targets_list acc = if (hEuclidean (getPosTarget (head targets_list)) (xh, yh)) < acc 
                                    then iter (tail targets_list) (hEuclidean (getPosTarget (head targets_list)) (xh, yh))
                                    else iter (tail targets_list) acc
{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
