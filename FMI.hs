data Pais = Pais{
    ingresoPerCapita :: Float,
    poblacionPublica :: Float,
    poblacionPrivada :: Float,
    recursosNaturales :: [String],
    deuda :: Float
} deriving (Eq,Show)

type Estrategia = Pais -> Pais

prestarDinero :: Float -> Estrategia
prestarDinero millones unPais = cambiarDeuda (1.50*deuda unPais) unPais
cambiarDeuda :: Float -> Pais -> Pais
cambiarDeuda num unPais = unPais {deuda = deuda unPais + num } 

reducirPuestosTrabajo :: Float -> Estrategia
reducirPuestosTrabajo x unPais
    | ((>100) . ingresoPerCapita) unPais   = (disminuirIPC 0.10 . disminuirPuestos x) unPais
    | otherwise                            = (disminuirIPC 0.15 . disminuirPuestos x) unPais 

disminuirIPC :: Float -> Pais -> Pais
disminuirIPC num unPais = unPais {ingresoPerCapita = ingresoPerCapita unPais - num}

disminuirPuestos :: Float -> Pais -> Pais
disminuirPuestos num unPais = unPais {poblacionPublica = poblacionPublica unPais - num}

explotarRecursoNatural :: String -> Estrategia
explotarRecursoNatural recurso = cambiarDeuda (-2) . sacarRecurso recurso 

sacarRecurso :: String -> Pais -> Pais
sacarRecurso recurso unPais = unPais {recursosNaturales = filter (/=recurso) (recursosNaturales unPais) }

establecerBlindaje :: Estrategia
establecerBlindaje unPais = disminuirPuestos 500 . cambiarDeuda (pbi unPais) $ unPais

pbi :: Pais -> Float
pbi unPais = ingresoPerCapita unPais * (poblacionPrivada unPais + poblacionPrivada unPais) 

namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria","Ecoturismo"] 50

receta :: Estrategia
receta  = cambiarDeuda 200 . explotarRecursoNatural "Mineria" -- Composicion de funciones y aplicacion parcial. A todas le falta el parameetro "Pais"

-- ghci> receta namibia
-- Pais {ingresoPerCapita = 4140.0, poblacionPublica = 400000.0, poblacionPrivada = 650000.0, recursosNaturales = ["Ecoturismo"], deuda = 248.0}

zafanlosPaises :: [Pais] -> [Pais]
zafanlosPaises = filter (elem "Petroleo" . recursosNaturales)   -- Composicion y orden superior

totalDeuda :: [Pais] -> Float
totalDeuda = sum . map deuda -- Composicion y orden superior 

-- 5 en proceso
-- esMayor :: Pais -> [Estrategia] -> Bool
-- esMayor unPais [] = True
-- esMayor unPais (x:xs) = (pbi . x) unPais &&  esMayor unPais xs

--6
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

paisConRiquezaExtrema :: Pais
paisConRiquezaExtrema = Pais 100000 100000 100000 recursosNaturalesInfinitos 0

-- si evaluamos puedenZafarlosPais paisConRiquezaExtrema nunca nos va a devolver un valor porque siempre estara buscando el "Petroleo" pero nunca lo va a encontrar
-- Gracias a la lazy evaluation podemos ejecutar la funcion totalDeuda incluso cuando tiene una lista infinita