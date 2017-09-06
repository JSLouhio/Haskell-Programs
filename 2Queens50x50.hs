--queen.hs
--kaksiKuningatarta50x50Laudalla.hs

import Data.List

-- tyhjä n*n taulukko. joka ruudussa [x,y,z,q] sijaintikoordinaatit x ja y, sekä diagonaalisia rivejä kuvaavat z ja q.  
tyhja n = replicate n (replicate n [0,0,0,0])

--Täyttää shakkilaudan ruudun              
ruutuaction nro i max [x,y,z,q] = [nro, i ,(max -1) + nro - i, nro +i] 

--täyttää yksittäisen rivin                   
riviaction rivi rivinro vuoronro newrivi
                                        |vuoronro == length rivi = reverse newrivi
                                        |otherwise = riviaction rivi rivinro (succ vuoronro) (ruutuaction rivinro vuoronro (length rivi) (rivi !! vuoronro) :newrivi) 
--Täyttää talukon kaikki rivit                
tauluaction taulu vuoronro newtaulu
                                   |vuoronro == length taulu = concat (reverse  newtaulu) 
                                   |otherwise = tauluaction taulu (succ vuoronro) ((riviaction (taulu !! vuoronro) vuoronro 0 []) :newtaulu)
                                   
--laskee kaikki kombinaatiot kahdelle taulukon ruudulle                                   
vaihtoehdot taulu = [[x,y] | (x:rest) <- tails taulu , y <- rest]

-- kolme funktiota, jotka kertovat ovatko kaksi ruutua samalla rivillä, pylväässä, tai diago -riveillä.
samarivi [a,_,_,_] [b,_,_,_] = a == b
samapylvas [_,a,_,_] [_,b,_,_] = a == b
samadiago [_,_,a,c] [_,_,b,d] = a == b || c == d   

-- kolmen ylläolevan ollessa tosia uhkaavat kahden ruudun kuningattaret toisiaan
uhka [a, b] = samarivi a b || samapylvas a b || samadiago a b 

-- käänteinen versio edellisestä
notUhka [a, b] = not (uhka [a, b])

--laskee montako tapaa kaksi kuningatarta voi sijoittaa n*n shakkilaudalla niiden uhkaamatta toisiaan 
master n = filter notUhka (vaihtoehdot (tauluaction (tyhja n) 0 []))
blaster n = length $ filter uhka (vaihtoehdot (tauluaction (tyhja n) 0 []))

main = do
          putStrLn (show ((master 5000)))
          
