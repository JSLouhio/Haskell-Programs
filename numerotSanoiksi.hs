-- luvutsanoiksi.hs

import Data.List.Utils

-- nimet numeroille 0-9
nimi n
      |n == 0 = ""
      |n == 1 = "yksi"
      |n == 2 = "kaksi"
      |n == 3 = "kolme"
      |n == 4 = "neljä"
      |n == 5 = "viisi"
      |n == 6 = "kuusi"
      |n == 7 = "seitsemän"
      |n == 8 = "kahdeksan"
      |n == 9 = "yhdeksän"
      |otherwise = "blablabla"


-- korvaa muodon "kymmmenen"+"numero" muotoon "numero"+"toista"
teinit n = replace "kymmenenyksi" "yksitoista" (replace "kymmenenkaksi" "kaksitoista" (replace "kymmenenkolme" "kolmetoista" 
                                               (replace "kymmenenneljä" "neljätoista" (replace "kymmenenviisi" "viisitoista" 
                                               (replace "kymmenenkuusi" "kuusitoista" (replace "kymmenenseitsemän" "seitsemäntoista" 
                                               (replace "kymmenenkahdeksan" "kahdeksantoista" (replace "kymmenenyhdeksän" "yhdeksäntoista" n))))))))
 
-- muuttaa numeron listaksi numeroita          
digi n
      | n == 0 = []
      | otherwise = digi (div n 10) ++ [mod n 10]

-- lisää numerolistan alkuun nollia, kunnes listan pituus on 9.     
nolliaalkuun d
              |length d == 9 = d
              |otherwise = nolliaalkuun ([0] ++ d)
       
-- jos luvussa esiintyy Miljoonia, nimeää ne             
miljoonat d
           |((d !! 0) /= 0) || ((d !! 1) /= 0) || ((d !! 2) /= 0) = (if ((d !! 0) == 1) then "sata" else ((nimi (d !! 0)) ++ (if ((d !! 0) /= 0) then "sataa" else ""))) ++ 
                                                                    (if ((d !! 1) == 1) then "kymmenen" else ((nimi (d !! 1)) ++ (if ((d !! 1) /= 0) then "kymmentä" else ""))) ++
                                                                    (if ((d !! 2) == 1 && (d !! 1  == 0) && (d !! 0) == 0) then "miljoona" else ((nimi (d !! 2)) ++  "miljoonaa"))
           | otherwise = ""

-- jos luvussa esiintyy tuhansia, nimeää ne
tuhannet d
          |((d !! 3) /= 0) || ((d !! 4) /= 0) || ((d !! 5) /= 0) = (if ((d !! 3) == 1) then "sata" else ((nimi (d !! 3)) ++ (if ((d !! 3) /= 0) then "sataa" else ""))) ++ 
                                                                   (if ((d !! 4) == 1) then "kymmenen" else ((nimi (d !! 4)) ++ (if ((d !! 4) /= 0) then "kymmentä" else ""))) ++
                                                                   (if ((d !! 5) == 1 && (d !! 4  == 0) && (d !! 3) == 0) then "tuhat" else ((nimi (d !! 5)) ++  "tuhatta"))
          | otherwise = ""

-- nimeää loput          
loput d = (if ((d !! 6) == 1) then "sata" else ((nimi (d !! 6)) ++ (if ((d !! 6) /= 0) then "sataa" else ""))) ++
          (if ((d !! 7) == 1) then "kymmenen" else ((nimi (d !! 7)) ++ (if ((d !! 7) /= 0) then "kymmentä" else ""))) ++
          (nimi (d !! 8))  

-- antaa nimen max 9 numeroiselle luvulle       
numeronimi n = (miljoonat num) ++ (tuhannet num) ++ (loput num) 
                where num = nolliaalkuun (digi ((read n::Int)))

muutokset n = teinit (numeronimi n)

--pääohjelma kysyy numeroita, kunnes vastaan tulee tyhjä rivi.
main = do   
        numero <- getLine  
        if null numero  
            then return ()  
            else do
                putStrLn (muutokset numero)
                main  
