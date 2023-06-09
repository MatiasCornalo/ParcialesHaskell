data Heroe = Heroe{
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}

data Artefacto = Artefacto{
    nombre :: String,
    rareza :: Int
}

data Bestia = Bestia{
    nombreBestia :: String,
    debilidad :: (Heroe->Bool)
}

paseAlaHistoria :: Heroe -> Heroe
paseAlaHistoria unHeroe
    | reconocimiento unHeroe > 1000 = cambiarEpiteto "El Mitico" unHeroe
    | reconocimiento unHeroe >= 500 = (darArtefacto lanzaDelOlimpo . cambiarEpiteto "El Magnifico") unHeroe
    | reconocimiento unHeroe > 100  = (darArtefacto xiphos . cambiarEpiteto "Hoplita") unHeroe
    | otherwise = unHeroe



cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto epitetoNuevo unHeroe = unHeroe {epiteto = epitetoNuevo}

darArtefacto :: Artefacto -> Heroe -> Heroe
darArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : artefactos unHeroe}

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

type Tarea = Heroe -> Heroe

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto = darArtefacto unArtefacto . ganarReconocimiento (rareza unArtefacto)

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento num unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + num}

escalarElOlimpo :: Tarea
escalarElOlimpo unHeroe = (darArtefacto relampagoDeZeus . desecharLosComunesTriplicados .  ganarReconocimiento 500) unHeroe

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "Relampago De Zeus" 500

desecharLosComunesTriplicados :: Heroe -> Heroe
desecharLosComunesTriplicados unHeroe = unHeroe {artefactos = filter ((>1000) . (*3) . rareza) (artefactos unHeroe)}

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras unHeroe = cambiarEpiteto ("gros" ++ (agregarO cuadras)) unHeroe

agregarO :: Int -> String
agregarO cuadras = concat (replicate cuadras "o")

matarBestia :: Bestia -> Tarea
matarBestia unaBestia unHeroe
    | puedeMatar unHeroe unaBestia = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) unHeroe
    | otherwise = (perderArtefacto . cambiarEpiteto "El cobarde") unHeroe

puedeMatar :: Heroe -> Bestia -> Bool
puedeMatar unHeroe unaBestia = debilidad unaBestia unHeroe

perderArtefacto :: Heroe -> Heroe
perderArtefacto unHeroe = unHeroe {artefactos = drop 1 (artefactos unHeroe)} 

heracles :: Heroe
heracles = Heroe "El Guardian del Olimpo" 700 [pistola,relampagoDeZeus] [matarBestia leonDeNemea]

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" longitudEpiteto20

longitudEpiteto20 :: Heroe->Bool
longitudEpiteto20 unHeroe = ((>20) . length . epiteto) unHeroe

hacerTarea :: Heroe -> Tarea -> Heroe
hacerTarea unHeroe unaTarea = (unaTarea unHeroe) {tareas = unaTarea : tareas unHeroe}

presumir :: Heroe -> Heroe -> (Heroe,Heroe)
presumir heroe1 heroe2
    | tieneMayorReconocimiento heroe1 heroe2 = ganaPrimero heroe1 heroe2
    | tieneMayorReconocimiento heroe2 heroe1 = ganaPrimero heroe2 heroe1
    | sumRarezaArtefactos heroe1 > sumRarezaArtefactos heroe2 = ganaPrimero heroe1 heroe2
    | sumRarezaArtefactos heroe2 < sumRarezaArtefactos heroe1 = ganaPrimero heroe2 heroe1
    | otherwise = presumir (realizarTareasDelOtro heroe1 heroe2) (realizarTareasDelOtro heroe2 heroe1)

-- Si los heroes tienen el mismo reconocimiento y no hicieron tareas, ambos van a estar presumiendo entre si hasta el infinito 

tieneMayorReconocimiento :: Heroe -> Heroe -> Bool
tieneMayorReconocimiento heroe1 heroe2 = reconocimiento heroe2 > reconocimiento heroe2

ganaPrimero :: Heroe -> Heroe -> (Heroe,Heroe)
ganaPrimero x y = (x,y)

sumRarezaArtefactos :: Heroe -> Int
sumRarezaArtefactos  = sum . map rareza . artefactos 

realizarTareasDelOtro :: Heroe -> Heroe -> Heroe
realizarTareasDelOtro heroe1 heroe2 = foldl (flip ($)) heroe1 (tareas heroe2)

type Labor = [Tarea]
hacerLabor :: Labor -> Heroe -> Heroe
hacerLabor labores heroe = foldl (flip ($)) heroe labores 

-- NO se va a poder saber el estado final del heroe porque el mismo va a estar realizando tareas constantemente sin terminar

hacerLaborInfinita = escalarElOlimpo : hacerLaborInfinita