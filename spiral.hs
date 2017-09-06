--spiral.hs

--listaa luvut 1 -> n*n
list n = [1..(n * n)]   

--jakaa lukulistan laatikoihin (joka laatikossa kaksi lukua enemmän kuin edellisessä)     
jaalaatikot jaljella palautelista x
                                   | jaljella == [] = palautelista
                                   | x == 1 = jaalaatikot (drop x jaljella) (palautelista ++ ([take x jaljella])) (succ x)
                                   | x == 2 = jaalaatikot (drop (x + 1)  jaljella) (palautelista ++ ([take (x + 1) jaljella])) (succ x)
                                   | otherwise = jaalaatikot (drop (x + 2) jaljella) (palautelista ++ ([take (x + 2) jaljella])) (x + 2)

-- palauttaa listan, jonka jokatoinen lista on käänteinen.
kaannajokatoinen listalistoista i palaute
                                         | listalistoista == [] = palaute
                                         | mod i 2 == 0 = kaannajokatoinen (tail listalistoista) (succ i) (palaute ++ [(head listalistoista)])
                                         | otherwise = kaannajokatoinen (tail listalistoista) (succ i) (palaute ++ [(reverse (head listalistoista))])

--apufunktio, joka käy läpi edellämainitut                                         
master n = map reverse (reverse (kaannajokatoinen (jaalaatikot (list n) [] 1) 0 []))

-- Käy lävitse edellisen funktion tuottaman listan ja ottaa i kappalletta (i:n ollessa alussa spiraalin koko) jokaisesta "laatikosta"         
kaylapi kaannetty i palauta
                           |kaannetty == [] = reverse palauta 
                           |otherwise = kaylapi (tail kaannetty) (pred i) (palauta ++ [take i (head kaannetty)]) 

-- palauttaa numerot, joita edellinen funktio ei ottanut talteen. yhdestavikaan = [1..spiraalinKoko]            
jaljella lista yhdestavikaan i palaute
                                     |i == length lista = reverse palaute
                                     |otherwise = jaljella lista yhdestavikaan (succ i) (palaute ++ [drop (yhdestavikaan !! i) (lista !! i)])             
            
--lisää jäljellä olevat numerot aiemmin kerättyihin laatikoihin.
lisaaloput jaljellaolevat lapikaydyt i final
                                            | i == length lapikaydyt = final 
                                            | otherwise = lisaaloput (init jaljellaolevat) lapikaydyt (succ i) (final ++ [(lapikaydyt !! i) ++ (reverse (map (!! i) (init jaljellaolevat)))])
        
-- apufunktioita aiempien kahden funktion käyttöön        
superlapi n = (kaylapi (master n) n [])
megajaljella n = map reverse (jaljella (reverse (master n)) [1..n] 0 [])     
final n = lisaaloput (megajaljella n) (superlapi n) 0 []

-- muuttaa listan numerot Int:stä String:ksi
kirjaimiksi n = [map show x | x <- (final n)]

--Lisää välilyöntejä numeron pituuden mukaisesti
lisaaspaceja lista uusi
                       | lista == [] = uusi ++ "\n"
                       | (read (head lista) ::Int) < 10 = lisaaspaceja (tail lista) (uusi ++ ("   " ++ (head lista)))
                       | ((read (head lista) ::Int) >= 10 && (read (head lista)::Int) < 100) = lisaaspaceja (tail lista) (uusi ++ ("  " ++ (head lista)))
                       | (read (head lista) ::Int) >= 100 = lisaaspaceja (tail lista) (uusi ++ (" " ++ (head lista)))   
                       | otherwise = (head lista)
                         
--apufunktio, joka tekee kaikki edellä mainitut                         
valit n = [lisaaspaceja x [] | x <- (kirjaimiksi n)]                          
         
main = do
          mapM_ putStrLn (valit 30)
           
            
           
