module Tests where
import Test.HUnit
import Fourmis

fourmi0 = Fourmi Gauche 0 0

mkFourmi dir pos = fourmi0 { directionFourmi = dir, positionFourmi = pos }

main = runTestTT $ test [
    "Une fourmi, ça avance" ~: [
      avance (mkFourmi Droite 0) ~?= mkFourmi Droite 1,
      avance (mkFourmi Gauche 0) ~?= mkFourmi Gauche (-1)
    ],
    "Une procession, ça procède" ~: [
      procède [mkFourmi Droite 0, mkFourmi Droite 1] ~?=
        [mkFourmi Droite 1, mkFourmi Droite 2]
    ],
    "Une fourmi, ça chute au bord gauche du bâton" ~: [
      procède [mkFourmi Gauche 0] ~?= [],
      procède [mkFourmi Droite 99] ~?= []
    ],
    "Une fourmi, quand ça se cogne à une copine, bin ça fait demi tour" ~: [
      procède [mkFourmi Droite 1, mkFourmi Gauche 2] ~?=
              [mkFourmi Gauche 1, mkFourmi Droite 2],
      procède [mkFourmi Droite 1, mkFourmi Gauche 2, mkFourmi Droite 5] ~?=
              [mkFourmi Gauche 1, mkFourmi Droite 2, mkFourmi Droite 6],
      procède [mkFourmi Droite 1, mkFourmi Droite 5, mkFourmi Gauche 6] ~?=
              [mkFourmi Droite 2, mkFourmi Gauche 5, mkFourmi Droite 6],
      procède [mkFourmi Droite 1, mkFourmi Gauche 1] ~?=
              [mkFourmi Gauche 0, mkFourmi Droite 2],
      procède [mkFourmi Droite 1, mkFourmi Gauche 2, mkFourmi Gauche 3] ~?=
              [mkFourmi Gauche 1, mkFourmi Droite 2, mkFourmi Gauche 2],
      procède [mkFourmi Droite 1, mkFourmi Droite 2, mkFourmi Gauche 2] ~?=
              [mkFourmi Gauche 1, mkFourmi Droite 2, mkFourmi Droite 3]
    ],
    "Une procession, ça se dessine" ~:
      let dessine = dessineAvecColorisateur (flip const) in [
      dessine [mkFourmi Gauche 0] ~?= "<",
      dessine [mkFourmi Droite 0] ~?= ">",
      dessine [mkFourmi Gauche 1] ~?= " <",
      dessine [mkFourmi Gauche 2] ~?= "  <",
      dessine [mkFourmi Droite 2] ~?= "  >",
      dessine [mkFourmi Droite 2, mkFourmi Droite 2] ~?= "  >",
      dessine [mkFourmi Gauche 1, mkFourmi Droite 2] ~?= " <>",
      dessine [mkFourmi Droite 1, mkFourmi Gauche 1] ~?= " X",
      dessine [mkFourmi Gauche 1, mkFourmi Droite 1] ~?= " ♢"
    ]
  ]
