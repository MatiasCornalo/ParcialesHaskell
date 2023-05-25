data Persona = UnaPersona String Float Int [(String,Int)] deriving(Show)

nico :: Persona
nico = (UnaPersona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu :: Persona
maiu = (UnaPersona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)])

suerteTotal :: Persona -> Int
suerteTotal (UnaPersona _ _ suerte [(objeto,valor)])
    | esAmuleto objeto = max (valor * suerte) suerte
    | otherwise        = suerte

esAmuleto :: String -> Bool
esAmuleto objeto = objeto == "amuleto"

data Juego = Juego{
    nombre:: String,
    cuantoGana :: (Float->Float),
    criterios :: [(Persona -> Bool)]
}

ruleta :: Juego
ruleta = Juego "Ruleta" (*37) [tieneSuerteMayorA 80]

tieneSuerteMayorA :: Int -> Persona -> Bool
tieneSuerteMayorA  num unaPersona = suerteTotal unaPersona > num 

maquinita :: Float -> Juego
maquinita numero = Juego "Maquinita" (jackPotMasDinero numero) [tieneSuerteMayorA 95, tiene "paciencia"]

jackPotMasDinero :: Float -> Float -> Float
jackPotMasDinero dinero jackpot = dinero + jackpot

tiene :: String -> Persona -> Bool
tiene palabra (UnaPersona _ _  _ objetos) = (elem "paciencia" . map fst) objetos

ganaElJuego :: Persona -> Juego -> Bool
ganaElJuego unaPersona (Juego _ _ [])= True  
ganaElJuego unaPersona unJuego = head (criterios unJuego) unaPersona && ganaElJuego unaPersona (sacarPrimero unJuego)

sacarPrimero :: Juego -> Juego
sacarPrimero unJuego = unJuego { criterios = drop 1 (criterios unJuego)}

totalDinero1 :: Persona -> Float -> [Juego] -> Float
totalDinero1 unaPersona dinero juegos = foldl (flip ($)) dinero ((map cuantoGana . filtraLosGanadores unaPersona) juegos)

filtraLosGanadores :: Persona -> [Juego] -> [Juego]
filtraLosGanadores unaPersona juegos = filter (ganaElJuego unaPersona) juegos  


totalDinero2 :: Persona -> Float -> [Juego] -> Float
totalDinero2 unaPersona dinero [] = dinero
totalDinero2 unaPersona dinero (juego1:juegos) 
    | ganaElJuego unaPersona juego1 = totalDinero2 unaPersona (cuantoGana juego1 dinero) juegos
    | not (ganaElJuego unaPersona juego1) = totalDinero2 unaPersona dinero juegos 

noGananlosJugadores :: [Persona] -> [Juego] -> [String]
noGananlosJugadores unasPersonas unosJuegos = map nombrePersona (filter (noGanaNingunJuego unosJuegos) unasPersonas)

nombrePersona :: Persona -> String
nombrePersona (UnaPersona nombrep _ _ _) = nombrep

noGanaNingunJuego :: [Juego] -> Persona -> Bool
noGanaNingunJuego unosJuegos unaPersona = (not . any(ganaElJuego unaPersona)) unosJuegos 