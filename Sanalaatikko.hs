--sanalaatikko.hs
--Tulostaa sanan toistuvasti halutun kokoiseen laatikkoon

sanalista sana kantti = take (kantti * kantti) (cycle sana)
sanatJaIndeksit sana kantti = (zip (sanalista sana kantti) [0..])
charToString :: Char -> String
charToString c = [c]
muutaStringeiksi sana kantti = [(charToString (fst x), snd x) | x <- sanatJaIndeksit sana kantti]
jokaYydes (a,x) y
    | x <= 0       = False
    | mod x y <= 0 = True 
    | otherwise    = False
lisaarivitys (sana,indeksi) = (" \n" ++ sana, indeksi)
sanakarsinta sana kantti = [if jokaYydes x kantti then lisaarivitys x else x | x <- muutaStringeiksi sana kantti]
sanalistaRivityksella sana kantti = fst (unzip [x | x <- sanakarsinta sana kantti])
sanaStringina sana kantti = concat (sanalistaRivityksella sana kantti)
printlaatikko sana kantti = putStrLn (sanaStringina sana kantti)

main = do
        let sana = "SAIPPUAKAUPPIAS"
        let kantti = 47
        printlaatikko sana kantti
