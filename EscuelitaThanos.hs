-- Punto 1
data Guantelete = Guantelete{
    material :: String,
    gemas :: [Gema] 
}

type Gema = Personaje -> Personaje

data Personaje = Personaje{
    edad :: Int,
    energia :: Int,
    planeta :: String,
    habilidades :: [String]
    
}

type Universo = [Personaje]

chasquido :: Universo -> Guantelete -> Universo
chasquido unUniverso unGuantelete
    | esDeUruGuantelete unGuantelete && tiene6GemasGuantelete unGuantelete = take (div (length unUniverso) 2) unUniverso 
    | otherwise = unUniverso

esDeUruGuantelete :: Guantelete -> Bool
esDeUruGuantelete unGuantelete = material unGuantelete == "uru"

tiene6GemasGuantelete :: Guantelete -> Bool
tiene6GemasGuantelete = (== 6) . length . gemas  

-- Punto 2
esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any (<45) . map edad 

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia 
