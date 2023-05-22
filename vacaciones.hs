data Turista = Turista {
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

reducirStress :: Turista -> Excursion -> Turista
reducirStress unTurista unaExcursion = (cambiarStress (-(div (stress (unaExcursion unTurista)) 10)) . unaExcursion) unTurista

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = (Turista->Int)
deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun unIndice unTurista unaExcursion = unIndice (unaExcursion unTurista) - unIndice unTurista 

esEducativa :: Turista -> Excursion -> Bool
esEducativa unTurista unaExcursion = deltaExcursionSegun numIdiomas unTurista unaExcursion > 0

numIdiomas :: Turista -> Int
numIdiomas unTurista = length (idiomas unTurista)

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes unTurista  = filter ((>3) . deltaExcursionSegun stress unTurista) 

type Tour = [Excursion]
completo :: Tour    
completo  = [salirHablarIdioma "melmacquiano",  apreciarPaisaje "Cascada",caminar 20]

ladoB :: Excursion -> Tour
ladoB excursion = [caminar 120 , excursion , paseoEnBarco Tranquila]

islaVecina :: Marea -> Tour
islaVecina unaMarea
    | unaMarea == Fuerte = [paseoEnBarco unaMarea , apreciarPaisaje "Lago" , paseoEnBarco unaMarea]
    | otherwise          = [paseoEnBarco unaMarea, irAPlaya, paseoEnBarco unaMarea]

hacerUnTour :: Turista -> Tour -> Turista
hacerUnTour unTurista unTour =  foldl (flip ($)) (cambiarStress (length unTour) unTurista) unTour

toursConvincentes :: Tour -> Turista -> Tour
toursConvincentes unTour unTurista  = filter (not . teDejaSolo unTurista) (excursionesDesestresantes unTurista unTour)

esConvincente :: Tour -> Turista -> Bool
esConvincente unTour unTurista = any (not . teDejaSolo unTurista) . excursionesDesestresantes unTurista  $ unTour


teDejaSolo :: Turista -> Excursion -> Bool
teDejaSolo unTurista unaExcursion = viajaSolo (unaExcursion unTurista) 

efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidadAportada tour) . filter (esConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista = deltaSegun nivelDeCansancioEstres (hacerUnTour turista tour) turista  

nivelDeCansancioEstres :: Turista -> Int
nivelDeCansancioEstres unTurista = cansancio unTurista + stress unTurista

-- ghci> replicate (1/0) irAPlaya
-- Para ana es convincente porque viaja acompañada siempre, pero para beto y cathi no
-- en Haskell es imposible saber porque tiene "Lazy evaluation" y no va a ejecutar la funcion hasta que realmente lo necesite