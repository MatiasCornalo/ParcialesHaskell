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

-- Segunda parte
laMente :: Int -> Gema
laMente numero unPersonaje = sacarEnergia numero unPersonaje

sacarEnergia :: Int -> Personaje -> Personaje
sacarEnergia numero unPersonaje = unPersonaje {energia = energia unPersonaje - numero}

elAlma :: String -> Gema
elAlma habilidad = sacarEnergia 10 . sacarHabilidad habilidad  

sacarHabilidad :: String -> Personaje -> Personaje
sacarHabilidad habilidad unPersonaje = unPersonaje {habilidades = filter (/= habilidad) (habilidades unPersonaje)}

elEspacio :: String -> Gema
elEspacio planeta = enviarAlplaneta planeta . sacarEnergia 10

enviarAlplaneta :: String -> Personaje -> Personaje
enviarAlplaneta planetaNuevo unPersonaje = unPersonaje {planeta = planetaNuevo}

elPoder :: Gema
elPoder unPersonaje
    | tienePocasHabilidades unPersonaje         = (sacarEnergia (energia unPersonaje) . vaciarHabilidades) unPersonaje
    | not (tienePocasHabilidades unPersonaje)   = sacarEnergia (energia unPersonaje) unPersonaje

vaciarHabilidades :: Personaje -> Personaje
vaciarHabilidades unPersonaje = unPersonaje {habilidades = []}

tienePocasHabilidades :: Personaje -> Bool
tienePocasHabilidades  = (<=2) . length . habilidades

elTiempo :: Gema
elTiempo unPersonaje = unPersonaje {edad = max (div (edad unPersonaje) 2) 18}

laGemaLoca :: Gema -> Gema
laGemaLoca unaGema unPersonaje = unaGema unPersonaje

guanteleteTrucho :: Guantelete
guanteleteTrucho = Guantelete "Goma" [elTiempo,elAlma "Usar Mjolnir",laGemaLoca (elAlma "Programacion en Haskell")]

utilizar :: Personaje -> [Gema] -> Personaje
utilizar unPersonaje gemasNuevas = foldl (flip ($)) unPersonaje gemasNuevas

--gemaMasPoderosa :: Guantelete -> Personaje -> Gema
--gemaMasPoderosa unGuantelete unPersonaje = max (energiaPerdida unPersonaje ((head . gemas) unGuantelete)) (gemaMasPoderosa (unGuantelete{gemas = drop 1 (gemas unGuantelete)}) unPersonaje) 

energiaPerdida :: Personaje -> Gema -> Int
energiaPerdida unPersonaje unaGema = abs (energia unPersonaje - (energia . unaGema) unPersonaje) 

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete unPersonaje = (utilizar unPersonaje . take 3 . gemas) guantelete

-- gemaMasPoderosa punisher guanteleteDeLocos. Este comando es incalculable, ya que la lista es infinita e infinitamente estamos aplicando la gema del tiempo a Punisher, por lo tanto nunca nos va a devolver el valor 
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher.  Este si va a funcionar porque la ventaja de "Lazy evaluation" es que no utiliza los valores, o no calcula los valores, hasta que realmente los necesite.
-- En este caso se ejecuta perfectamente porque solo necesita las primeras 3 gemas

punisher :: Personaje
punisher = Personaje 10 10 "Tierra" ["Disparar"]
