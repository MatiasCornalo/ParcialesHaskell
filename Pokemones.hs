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
    puntos :: Float,
    nivelCompaniero :: Int
}

data Rango = Cielo | Galaxia | Constelacion | Estrella deriving (Eq,Show)

akari :: Investigador
akari = Investigador Constelacion "Akari" 1500 oshawoot [] []

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
bayas = cuadradoExp . cambiarExp 1 

cuadradoExp :: Investigador -> Investigador
cuadradoExp unInvestigador = unInvestigador {experiencia = experiencia unInvestigador ^ 2}

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

capturarPokemon :: Pokemon -> Actividad
capturarPokemon unPokemon unInvestigador
    |   puntos unPokemon > 20   = cambiarExp (puntos unPokemon) . cambiarCompaniero unPokemon $ unInvestigador
    |   otherwise               = cambiarExp (puntos unPokemon) unInvestigador

cambiarCompaniero :: Pokemon ->  Investigador -> Investigador
cambiarCompaniero unPokemon unInvestigador = unInvestigador{companiero = unPokemon}

combatirPokemon :: Pokemon -> Actividad
combatirPokemon unPokemon unInvestigador
    | leGana (companiero unInvestigador) unPokemon = cambiarExp (puntos unPokemon/2) unInvestigador
    | otherwise = unInvestigador

leGana :: Pokemon -> Pokemon -> Bool
leGana pokemon1 pokemon2 = nivelCompaniero pokemon1 > nivelCompaniero pokemon2

type Expedicion = [Actividad]

investigadoresConpokemonesAlfa :: [Investigador] -> Expedicion -> [String]
investigadoresConpokemonesAlfa investigadores expedicion = map nombre . filter ((==3) . length . filter (=="Alfa"). map mote . pokemonesCapturados) . investigadoresPostExpedicion expedicion $ investigadores

investigadoresGalaxia :: Expedicion -> [Investigador] -> [Float]
investigadoresGalaxia expedicion = map experiencia . filter ((==Galaxia) . rango) . investigadoresPostExpedicion expedicion 

investigadoresPostExpedicion :: Expedicion -> [Investigador] -> [Investigador]
investigadoresPostExpedicion expedicion investigadores = map (realizarExpedicion expedicion) investigadores 

pokemonesDeNivel10DeInvestigadores :: Expedicion -> [Investigador] -> [Pokemon]
pokemonesDeNivel10DeInvestigadores expedicion investigadores = map companiero . filter((>10) . nivelCompaniero . companiero) . investigadoresPostExpedicion expedicion $ investigadores

realizarExpedicion :: Expedicion -> Investigador -> Investigador
realizarExpedicion expedicion investigador = foldl (flip ($))  investigador expedicion

ultimos3PokemonesParesPostExp ::  Expedicion -> [Investigador] -> [[Pokemon]]
ultimos3PokemonesParesPostExp expedicion investigadores = map pokemonesCapturados . filter(all even . map nivelCompaniero . pokemonesCapturados) . investigadoresPostExpedicion expedicion $ investigadores

