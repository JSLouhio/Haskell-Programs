--saippuakauppa.hs

import Data.List

--- laskee kaikki eripituiset kombinaatiot sanalle, ja kertoo niiden määrän.

--tarkasteltavien kombinaatioiden maximipituus    
pituus s = length s - 1

-- Laskee n pituiset kombinaatiot sanalle. n=0 palauttaa listan vaihtoehdoista.   
combi 0 sana = [[]]
combi n sana = [x:rest | (x:xs) <- tails sana, rest <- combi (n-1) xs]
    
--tuottaa listan kaikista eripituisita kombinaatioista sanalle    
kaikkinaatiot i sana acc
                        | i == 0 = acc
                        | otherwise = kaikkinaatiot (pred i) sana (acc ++ combi i sana)
                
-- poistaa tupla-kappaleet vaihtoehdoista
poistaDuplikaatit l = poista l []
  where
    poista [] _  = [] 
    poista (x:xs) ls                              
                    | elem x ls = poista xs ls
                    | otherwise = x : poista xs (x:ls)

-- tuottaa kaikkien kombinaatioiden listan, poistaa tuplat ja kertoo listan pituuden.                    
final sana = length $ poistaDuplikaatit $ kaikkinaatiot (pituus sana) sana []
    
main = do
         print $ final "SAIPPUAKAUPPIAS"
      

