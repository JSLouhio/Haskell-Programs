--abc.hs


-- [A..Z] -> [1..26]

--funktio tarkastaa saamansa listan käyden läpi jokaisen elementin. 
--jos huomaa, etteivät kaksi elementtiä ole peräkkäisiä, palauttaa False. muutoin True.
porras xs         
         |tail xs == [] = True
         |not $ seuraava (head xs) (head $ tail xs) = False
         |otherwise = porras (tail xs)

--funktio, joka tarkastaa, onko b a:sta edeltävä tai seuraava luku         
seuraava a b = a == (b+1) || a == (b-1)

--funktio yhdistää eri vaihtoehdot x-pitusista kirjainyhdistelmistä A..Z
yhdista x = foldr vaihtoehdot (return []) (replicate x [1..26])

--luo listan erilaisista permutaatioista, siten, että jokainen a:n elementti on lisätty jokaiseen b:n elementtiin.
--ei ota mukaan vaihtoehtoja, jotka eivät toteuta 'porrasta'
vaihtoehdot a b = [x:xs | x <- a, xs <- b, porras (x:xs)]

main = do
          print $ length $ yhdista 15
                
