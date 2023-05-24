data Heroe = Heroe{
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto]
}

data Artefacto = Artefacto{
    nombre :: String,
    rareza :: Int
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