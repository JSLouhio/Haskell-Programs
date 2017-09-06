--alkusummat.hs

-- selvitettävä luku
n = 25

--lista numeroista 2..x
luvut = [2..n]

-- kertoo onko i alkuluku
onko i = not $ elem True [mod i x == 0 | x <- [2..(i-1)]]

--poistaa lukujonolta kaikki, jotka eivät ole alkulukuja
tarkasteltavat = filter onko $ luvut

-- kertoo pisimmän mahdollisen alkuluku -listan, jonka summa on x (eli 7 = [2,2,3], 10 = [2,2,2,2,2], jne..)
maxPit x | mod x 2 == 0 = div x 2
         |otherwise = div (x - 1) 2

-- laskee kaikki x-pit permutaatiot alkuluvuille n asti, lukuunottamatta niitä, joiden summa on yli 25
yhdista pit n = foldr vaihtoehdot (return []) (replicate pit tarkasteltavat)
vaihtoehdot a b = [x:xs | x <- a, xs <- b, sumN (x:xs)]

--tarkastaa, onko lukujonon summa yli n
sumN xs         
         |sum xs <= n = True
         |otherwise = False

-- laskee vaihtoehtoisten summajonojen pituudet acc:n
kaikki i n acc
              |i == 0 = acc 
              |otherwise = kaikki (pred i) n (acc + length (filter (\x -> sum x == n) (yhdista i n)))

-- toteuttaa ylläolevat n:lle ja palauttaa summavaihtoehtojen määrän.              
summavaihtoehtoja = kaikki (maxPit n) n 0

main = do
         print summavaihtoehtoja
