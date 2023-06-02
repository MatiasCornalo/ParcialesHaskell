data Investigador = Investigador{
    rango :: Rango,
    nombre :: String,
    experiencia :: Float,
    companiero :: Pokemon,
    mochila :: [Item],
    pokemonesCapturados :: [Pokemon]
}
type Item = Investigador -> Investigador

data Pokemon = Pokemon{
    mote :: String,
    descripcion :: String,
    puntos :: Int,
    nivelCompaniero :: Int
}

data Rango = Cielo | Galaxia | Constelacion | Estrella

akari :: Investigador
akari = Investigador Constelacion "Akari" 1500 oshawott [] []

oshawoot :: Pokemon
oshawoot = Pokemon "Oshawott" "una nutria que pelea con el caparazón de su pecho" 3 5 

esAlfa :: Pokemon -> Bool
esAlfa unPokemon = tieneTodaslasVocales (mote unPokemon) || ((=="Alfa") . take 4) (mote unPokemon)

tieneTodaslasVocales :: String -> Bool
tieneTodaslasVocales mote = all (flip elem mote) "aeiou"
--2
rangoInvestigador :: Investigador -> Investigador
rangoInvestigador unInvestigador
    | expMayorA 2000 unInvestigador = cambiarRango Galaxia unInvestigador
    | expMayorA 500 unInvestigador  = cambiarRango Constelacion unInvestigador
    | expMayorA 100 unInvestigador  = cambiarRango Estrella unInvestigador
    | otherwise =  cambiarRango Cielo unInvestigador

expMayorA :: Float -> Investigador -> Bool
expMayorA num unInvestigador = experiencia unInvestigador > num

cambiarRango :: Rango -> Investigador -> Investigador
cambiarRango unRango unInvestigador = unInvestigador{rango = unRango}

--3
type Actividad = Investigador -> Investigador
obtenerItem :: Item -> Actividad
obtenerItem unItem unInvestigador = unInvestigador{mochila= unItem : mochila unInvestigador}

bayas :: Item
bayas = (^2) . cambiarExp 1

cambiarExp :: Float -> Investigador -> Investigador
cambiarExp num unInvestigador = unInvestigador{experiencia=experiencia unInvestigador + num}

apricorns :: Item
apricorns unInvestigador = cambiarExp (experiencia unInvestigador/2) unInvestigador

guijarros :: Item
guijarros = cambiarExp 2

fragmentosDeHierro :: Float -> Item
fragmentosDeHierro num unInvestigador = unInvestigador{experiencia = experiencia unInvestigador / num}

admirarPaisaje :: Actividad
admirarPaisaje unInvestigador = perderItems 3 . cambiarExp (0.05*(experiencia unInvestigador)) $ unInvestigador

perderItems :: Int -> Investigador -> Investigador
perderItems num unInvestigador = unInvestigador {mochila = drop num $ mochila unInvestigador}

capturarPokemon :: 