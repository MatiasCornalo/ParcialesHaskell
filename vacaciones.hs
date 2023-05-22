data Turista = Turista{
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Eq,Show)

type Excursion = Turista -> Turista
irAPlaya :: Excursion
irAPlaya unTurista
    | viajaSolo unTurista         = unTurista {cansancio = cansancio unTurista - 5}
    | not (viajaSolo unTurista)     = cambiarStress (-1) unTurista

apreciarPaisaje :: String -> Excursion 
apreciarPaisaje elemento  = cambiarStress (-length elemento)

cambiarStress :: Int -> Turista -> Turista
cambiarStress numero unTurista =  unTurista {stress = stress unTurista + numero}

salirHablarIdioma :: String -> Excursion
salirHablarIdioma idioma unTurista = unTurista {viajaSolo = False, idiomas = idioma : idiomas unTurista}

caminar :: Int -> Excursion
caminar tiempo = cambiarStress (-(div tiempo 4)) . aumentarCansancio (div tiempo 4) 

aumentarCansancio :: Int -> Turista -> Turista
aumentarCansancio numero unTurista = unTurista {cansancio = cansancio unTurista + numero}

data Marea = Fuerte | Tranquila | Moderada deriving (Eq,Show)
paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea unTurista 
    | marea == Fuerte       =  (aumentarCansancio 10 . cambiarStress 6) unTurista
    | marea == Tranquila    =  (salirHablarIdioma "Aleman".  apreciarPaisaje "Mar" . caminar 10) unTurista
    | marea == Moderada     =  unTurista

ana :: Turista
ana = Turista 0 21 False ["Espaniol"]

beto :: Turista
beto = Turista 15 15 True ["Aleman"]

cathi :: Turista
cathi= Turista 15 15 True ["Aleman","Catalan"]
