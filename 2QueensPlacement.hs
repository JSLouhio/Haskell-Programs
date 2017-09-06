--nextQueen.hs

-- allaoleva algoritmi laskee, montako ruutua jokaiseen ruutuun asetettu Q uhkaa, ja vähentää sen kaikkin ruutujen määrästä.
-- algoritmi käsittelee erikseen nurkat, reunat ja keskipalat (esimerkki alla)

-- N R R R N
-- R 1 1 1 R
-- R 1 2 1 R
-- R 1 1 1 R
-- N R R R N

-- N = nurkkapala, R = reunapala, 1 = 1. sisäkehä, 2 = 2. sisäkehä, jne...

--reunapalojen määrä n*n laudalla
reunoja n = ((2*n + 2 * (n-2)) - 4)

--keskipalojen määrä n*n -laudalla
keskipaloja n = (n-2) * (n-2)

--reuna ja nurkkapaloihin asetetun Qn uhkaamien ruutujen määrän laskeva algoritmi
algo n = ((n-2) + (2 * n))

--reunapaloihin asetetun Qn uhkaamien ruutujen määrä
reunojenpoistamat n =  (reunoja n) * (algo n)

--nurkkapaloihin asetetun Qn uhkaamien ruutujen määrä
nurkkienpoistamat n =  4 * (algo n) 

--keskipaloille asetetun Qn uhkaamien ruutujen määrä
keskienpoistamat n = rinkikierros 0 (rinkejayht n) n

--uloin kehä uhkaa 3n ruutua, sisemmät ((rinkejä yht - ringin numero) * 2)
--laske, montako ruutua n*n laudalla sisä-kehän ruutuihin sijoitettu Q uhkaa
--eri määrä parillisilla ja parittomilla laudoilla.
ringinottamat n i
                 |mod n 2 == 0 = evenringinottamat n i
                 |otherwise = oddringinottamat n i

evenringinottamat n i = ((3 * n) + (((rinkejayht n) - i) *  2)) * ((8 * i) - 4)
oddringinottamat n i = ((3 * n) + (((rinkejayht n) - i) *  2)) * (if i == 1 then 1 else (8 * (i-1)))

--laskee, montako sisä-kehää n*n -laudalla on.
--kehien määrä vaihtelee parillisilla ja parittomilla laudoilla.
rinkejayht n
            |mod n 2 == 0 = evenrinkejayht n
            |otherwise = oddrinkejayht n

evenrinkejayht n = div (n-2) 2 
oddrinkejayht n = div (n-1) 2
 
-- käy läpi sisäkehät ja niiden ruutuihin asetetun Q:n uhkaamien ruutujen määrän.  
-- alussa i = (rinkejayht n), acc=0, n=max
rinkikierros acc i n
                     |i == 0 = acc
                     |otherwise = rinkikierros (acc + (ringinottamat n i)) (pred i) n
  
--laskee kaikkien ruutujen ja uhattujen ruutujen erotuksen 
nurkat n = (4*n*n) - nurkkienpoistamat n 
reunat n = ((reunoja n) * n * n) - reunojenpoistamat n
keskit n = ((keskipaloja n) * n * n) - keskienpoistamat n

--selvittää montako kahden kuningattaren yhdistelmää mahtuu n * n -shakkilaudalle.
--laskee ensiksi, montako vaihtoehtoa jää vapaaksi, kun Q sijoitetaan laudan nurkiin, reunoille, ja keskipaloille
--jakaa lopuksi kahdella (ottaen siten huomioon duplikaatit)
kelpaavia n = div ((nurkat n) + (reunat n) + (keskit n)) 2

main = do
         print (kelpaavia 12345)
