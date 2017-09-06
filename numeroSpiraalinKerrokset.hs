--spiraaliValinta.hs

-- 1  4  5  16
-- 2  3  6  15
-- 9  8  7  14
-- 10 11 12 13

-- Algoritmi tarkastelee yhtä spiraalin kierrettä, rivistä ja sarakkeesta suurempi kertoo, monennettako kierrettä tarkastellaan.

--kierteen ensimmäinen luku
ensimmainen n = (n-1)^2 + 1

--kierteen lukujen määrä yhteensä
lukuja n = 2 * (n-1) + 1

--kierteen viimeinen luku
viimeinen n = ensimmainen n + lukuja n - 1 

--joka toinen kierre on nouseva ja joka toinen laskeva. funktio kertoo rivinron perusteella kumpi.  
nouseva x = mod x 2 == 0

--funktio laskee halutun luvun.
--nousevissa kierteissä rivin ollessa saraketta suurempi lisää ensimmäiseen lukuun sarakkeen nron-1. 
--Sarakkeen ollessa suurempi vähentää kierteen suurimmasta luvusta rivin nron-1      
--laskevissa kierteissä päinvastoin.
ota r s
        | nouseva x = if r >= s then eka + (s - 1) else vika - (r - 1)
        | otherwise = if r >= s then vika - (s - 1) else eka + (r - 1) 
        where x = max r s
              eka = ensimmainen x
              vika = viimeinen x
              
--vaihtoehtoinen, tilaa säästävä versio edellisestä, missä kaikki tungettu kolmelle riville.      
ota' r s
        | mod (max r s) 2 == 0 = if r >= s then ((max r s)-1)^2 + 1 + (s - 1) else ((max r s)-1)^2 + 1 + 2 * ((max r s)-1) - (r - 1)
        | otherwise = if r >= s then ((max r s)-1)^2 + 1 + 2 * ((max r s)-1) - (s - 1) else ((max r s)-1)^2 + 1 + (r - 1) 

main = do
         print $ ota' 123456789 987654321
