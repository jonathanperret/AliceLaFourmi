{-# LANGUAGE ViewPatterns #-}
module Fourmis where
import Data.List
import Data.Ord
import System.Random
import System.Posix

data Fourmi = Fourmi { directionFourmi :: Direction, positionFourmi :: Int, couleurFourmi :: Int }
  deriving (Eq, Show)

dirPosFourmi f = (directionFourmi f, positionFourmi f)

data Direction = Droite | Gauche deriving (Eq, Show, Enum)

avance f@(directionFourmi -> Droite) = f { positionFourmi = positionFourmi f + 1 }
avance f@(directionFourmi -> Gauche) = f { positionFourmi = positionFourmi f - 1 }

procède = filter surLeBâton . collisionne . demitours
  where
    surLeBâton (positionFourmi -> x) = x >= 0 && x < 100

retourne f@(directionFourmi -> Droite) = f { directionFourmi = Gauche }
retourne f@(directionFourmi -> Gauche) = f { directionFourmi = Droite }

demitours (f1@(directionFourmi -> Droite) : f2@(directionFourmi -> Gauche) : rest)
  | positionFourmi f1 == positionFourmi f2 =
    retourne f1 : retourne f2 : demitours rest
demitours (f:fs) = f:demitours fs
demitours [] = []

collisionne  (f1@(directionFourmi -> Droite) : f2@(directionFourmi -> Gauche) : rest )
  | positionFourmi f2 == positionFourmi f1 + 1 =
    retourne f1 : retourne f2 : collisionne rest

collisionne (f:fs) = avance f : collisionne fs
collisionne [] = []

dessine = dessineAvecColorisateur couleurXterm

couleurXterm coul txt = "\x1b[38;5;" ++ show coul ++ "m" ++ txt ++ "\x1b[39m"

dessineAvecColorisateur colorie = dessine' 0
  where
    dessine' _ [] = ""
    dessine' x (Fourmi Droite fx1 coul1:Fourmi Gauche fx2 coul2:rest)
      | x == fx1 && x == fx2 = colorie (coul1+coul2) "X" ++ dessine' (x+1) rest
    dessine' x (Fourmi Gauche fx1 coul1:Fourmi Droite fx2 coul2:rest)
      | x == fx1 && x == fx2 = colorie (coul1+coul2) "♢" ++ dessine' (x+1) rest
    dessine' x (Fourmi dir fx coul:rest)
      | x == fx = (colorie coul $ dessineDir dir) ++ dessine' (x+1) rest
    dessine' x ((positionFourmi -> fx):rest)
      | x > fx = dessine' (x+1) rest
    dessine' x fs = ' ':dessine' (x+1) fs
    dessineDir Gauche = "<"
    dessineDir Droite = ">"

anime fs = mapM_ (putStrLn . dessine) étapes >> print (length étapes)
  where étapes = simule fs

animeLigne fs = do
    cacheCurseur
    sequence_ $ map ((>>usleep 50000) . putStr . remplaceLigne . dessine) étapes
    montreCurseur
  where
    étapes = simule fs
    remplaceLigne txt = '\x0d' : txt ++ "\x1b[K"
    cacheCurseur = putStr "\x1b[?25l"
    montreCurseur = putStrLn $ remplaceLigne "\x1b[?25h"

simule fs = takeWhile (not.null) $ iterate procède $ triFourmis fs

triFourmis = sortBy (comparing positionFourmi)

créeBâton n graine = triFourmis $ take n $ zipWith3 (\d p coul->Fourmi {directionFourmi = d, positionFourmi = p, couleurFourmi = coul}) directions positions [0..]
  where
    directions = map toEnum $ randomRs (0, 1) gen1
    positions = nub $ randomRs (0, 99) gen2
    (gen1, gen2) = split $ mkStdGen graine

chercheBâtons n = mapM_ print
  $ nubBy (\a b->snd a == snd b)
  $ scanl (\(_,(mn,mx)) (i,n) -> (i,(min mn n,max mx n))) (0,(100,0))
  $ [(i, length . takeWhile (not.null) . iterate procède . créeBâton n $ i) | i<-[0..] ]

