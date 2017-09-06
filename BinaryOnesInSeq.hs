--bitit.hs
                           
--laskee ykköset binaariksi muutetusta 10-järjestelmän luvusta 
binaari acc x 
             | x == 0 = acc
             | x == 1 = acc + 1
             | mod x 2 == 0 = binaari acc (div x 2)  
             | otherwise = binaari (acc + 1) (div x 2) 

--laskee ykköset välillä [alku..n]             
osasumma alku n acc
                   |n == alku = acc
                   |otherwise = osasumma alku (pred n) (acc + (binaari 0 n))             
             
--ykkösten määrä välillä [0..2^n]      
nollasta2potiin n = div ((2^n) * n + 2) 2

-- laskee n:ää edeltävän 2-potenssin.
ed2pot n = potenssifunktio n 0 0                                                                        
potenssifunktio n i acc
                       |n < acc = i - 2
                       |otherwise = potenssifunktio n (succ i) (2^i)

--laskee ykköset ensiksi nollasta n:ää lähinnä olevaan 2-potenssiin ja lisää ykköset siitä n:ään.
--periaatteessa itsessään toimiva funktio ykkösten laskemiseen, mutta vie liikaa aikaa ja muistia eron 2-potensseihin kasvaessa.      
binpot n = (nollasta2potiin (ed2pot n)) + osasumma (2^(ed2pot n)) n 0     
                        

--loppukoodi hyödyntää havaintoa, että mihinkä tahansa lukuun asti ykkösten määrän voi laskea alla olevien esimerkkien kaavoilla summaamalla 
-- 2-potensseja ja ykkösten määriä 2-potensseihin. ykkösten määrät 2-potensseissa puolestaan on helppo laskea aiemmilla funktioilla.
-- esim. 
--                  768 =  512 + 256 
--        ykkösiä: 3586 =  2305 + (1*256 + 1025)     

--                  772 = 512 + 256 + 4
--      ykkösiä:   3599 = 2305 + (1*256 + 1025) + (2*4 + 5)                  
   
--              100000 = 2^16 (65536) + 2^15 (32768) + 2^10 (1024) + 2^9 (512) + 2^7 (128) + 2^5 (32)    
--     ykkösiä: 815030 = 524289 + (1*32768 + 245761) + (2*1024 + 5121) + (3*512 + 2305) + (4*128 + 449) + (5*32 + 81) 

-- jne..
                                      
-- jakaa luvun n 2-potensseihin                                                                        
jaa2poteihin n lista
                    |n==0 = lista 
                    |otherwise = jaa2poteihin (n-(2^(ed2pot n))) (lista ++ [(2^(ed2pot n))])

--kerroin kasvaa yhdellä jokaista käytettyä 2-potenssia kohden ensimmäisen jälkeen.                    
--kertoo listan (ensimmäistä lukuunottamatta) luvuilla [1..listan pituus-1]                    
kerroinlista lista = zipWith (*) (drop 1 lista) (map toInteger [1..(length lista - 1)])   

--laskee listojen summat
kerroinsumma lista = sum $ kerroinlista lista   
summaa2potit lista = sum $ map binpot lista

--laskee molemmat listat yhteen.
ykkoset n = kerroinsumma (map fromIntegral lista) + summaa2potit lista where lista = jaa2poteihin n [] 

main = do
          print $ ykkoset (10^16)
