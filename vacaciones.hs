data Turista = Turista{
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
}

type Excursion = Turista -> Turista
irAPlaya :: Excursion
irAPlaya unTurista
    | viajaSolo unTurista         = unTurista {cansancio = cansancio unTurista - 5}
    | not (viajaSolo unTurista)     = reducirStress 1 unTurista

apreciarPaisaje :: String -> Excursion 
apreciarPaisaje elemento  = reducirStress (length elemento)

reducirStress :: Int -> Turista -> Turista
reducirStress numero unTurista =  unTurista {stress = stress unTurista - numero}

salirHablarIdioma :: String -> Excursion
salirHablarIdioma idioma unTurista = unTurista {viajaSolo = False, idiomas = idioma : idiomas unTurista}

caminar :: Int -> Excursion
caminar tiempo = reducirStress (div tiempo 4) . aumentarCansancio (div tiempo 4) 

aumentarCansancio :: Int -> Turista -> Turista
aumentarCansancio numero unTurista = unTurista {cansancio = cansancio unTurista + numero}
