--permutaatioPrinter.hs

-- tekee annetun lukujonon pohjalta listan tupleista, joissa jokaisessa yksi numeroista ja lista muista jonon luvuista.
-- esim. 123 => [(1,[2,3]),(2,[1,3]),(3,[1,2])]
paatJaHannat []     = []
paatJaHannat (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- paatJaHannat xs]

--käy läpi edellisen funktion luoman listan siten, että jokaisen tuplen ensimmäiseen jäseneen ("päähän") lisätään sen toisen jäsenen ("hännän") permutaatiot
--toistaen saman jokaiselle "hännän" osalle, kunnes vastaan tulee tyhjä lista.
permutaatiot [] = [[]]
permutaatiot xs = [ y:zs | (y,ys) <- paatJaHannat xs, 
                               zs <- permutaatiot ys]

--muuttaa numeroista koostuvan listan numeromuotoon. [1,2,3] -> 123 
listanumeroksi xs i n acc
                         |n == length xs = acc
                         |otherwise = listanumeroksi xs (i*10) (succ n) (acc + (((reverse xs) !! n) * i))

-- muuttaa edellistä käyttäen numerolistoista koostuvan listan kaikki jäsenet oikeiksi numeroiksi.                         
listalistaNumeroiksi xs = [listanumeroksi x 1 0 0| x <- xs]
                
main = do 
          mapM print $ listalistaNumeroiksi $ permutaatiot [1..6]
   
