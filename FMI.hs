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