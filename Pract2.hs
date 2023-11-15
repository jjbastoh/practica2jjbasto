import Data.Char
--hacer que en caso de que haya una X en la cadena que se introduce, se convierta en 10 y convertir los digitos a Int
convertirX :: Char -> Int
convertirX  a = 
    if a =='X' 
        then 10
    else digitToInt a

--funcion que permite verificar si el caracter es un digito o una X
esDigitoOX :: Char -> Bool
esDigitoOX a = isDigit a || a == 'X' 


suma :: String -> Int
suma = sum . zipWith (*) [10,9..1] . map convertirX . filter esDigitoOX

--funcion que permite verificar si el numero es valido
verificar :: String -> Bool
verificar x = suma x `mod` 11 == 0

