--käänteinenSanalista.hs

--Halve, seqmerge ja mergeSort listan uudelleenjärjestämistä varten 
halve seqx = let a = div (length seqx) 2
             in splitAt a seqx
             
seqmerge acc seqa seqb 
                        |null seqa && null seqb = reverse acc
                        |null seqa = seqmerge (head seqb : acc) seqa (tail seqb)
                        |null seqb = seqmerge (head seqa : acc) (tail seqa) seqb
                        |head seqa <= head seqb = seqmerge (head seqa : acc) (tail seqa) seqb
                        |head seqa >= head seqb = seqmerge (head seqb : acc) seqa (tail seqb)
mergeSort seqx
              |null seqx = seqx
              |length seqx == 1 = seqx
              |otherwise = seqmerge [] (mergeSort (fst (halve seqx))) (((mergeSort (snd (halve seqx)))))
              
--laskee listan, joka kertoo uudet indeksit kirjaimille.
-- i = 1, lista = [1].              
pylvasindeksit i lista kantti
                              | i == (kantti * kantti) = reverse lista
                              | mod (length lista) kantti == 0 =  pylvasindeksit (succ i) (((head lista) - (((kantti - 1) * kantti) -1)) :lista) kantti           
                              | otherwise = pylvasindeksit (succ i) (((head lista) + kantti) :lista) kantti           

--laskee kantti x kantti -pituisen Stringin, joka toistaa annettua sanaa.
sanalista sana kantti = take (kantti * kantti) (cycle sana)

--Tuplelista kirjaimista ja niiden indekseistä taulukossa
sanatJaIndeksit sana kantti = (zip (sanalista sana kantti) (pylvasindeksit 1 [1] kantti)) 

--muuttaa listan Char:it Stringeiksi käsittelyn helpottamiseksi
charToString :: Char -> String
charToString c = [c]
muutaStringeiksi sana kantti = [(charToString (fst x), snd x) | x <- sanatJaIndeksit sana kantti]

--laskee tuplelistalle uuden järjestyksen indeksien perusteella
flipTup (eka,toka) = (toka,eka)
flipMergeFlip sana kantti = map flipTup (mergeSort (map flipTup (muutaStringeiksi sana kantti)))

--laskee indeksit, joilla tulee lisätä rivivaihto
jokaKanttis kantti = [x | x <- [1..(kantti * kantti)], mod x kantti == 0]

--lisää rivivaihdon tuplen ensimmäiseen jäseneen
lisaarivitys (sana,indeksi) = (" \n" ++ sana, indeksi)

--lisää rivivaihdot tuplelistaan
rivitListaan sana kantti = [if elem ((snd x) - 1)  (jokaKanttis kantti) then lisaarivitys x else x | x <- (flipMergeFlip sana kantti)]

--purkaa tuplelistan kirjainlistaksi
sanalistaRivityksella sana kantti = fst (unzip [x | x <- rivitListaan sana kantti])

-- muuttaa kirjainlistan Stringiksi
sanaStringina sana kantti = concat (sanalistaRivityksella sana kantti)

-- tulostaa sanan laatikkoon
printlaatikko sana kantti = putStrLn (sanaStringina sana kantti)

main = do
         let sana = "SAIPPUAKAUPPIAS"
             kantti = 47
             in printlaatikko sana kantti  
         
         
