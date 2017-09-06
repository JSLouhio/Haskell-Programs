--bitbit.hs

--muuttaa x:n listaksi ykkösiä ja nollia 
binaari x
         | x == 0 = [0]
         | x == 1 = [1]
         | mod x 2 == 0 = binaari (div x 2) ++ [0]
         | otherwise = binaari (div x 2) ++ [1]

--listaa numerot 1..n         
listaa n = [1..n]

--muuttaa listan binäärilistoiksi
binaariMap n = map binaari (listaa n)

--yhdistää binäärilistat yhdeksi listaksi ykkösiä ja nollia
pelkatNum n = concat (binaariMap n)

--poistaa nollat
eiNollia n = filter (> 0) (pelkatNum n)

--laskee montako ykköstä jäljellä
montako n = length (eiNollia n)

--vaihtoehtoisesti edellinen ilman apufunktioita
kaikki n = length (filter (> 0) (concat (map binaari [1..n])))

main = do
         putStrLn (show (kaikki 12345))
