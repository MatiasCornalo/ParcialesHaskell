data Serie = Serie{
    nombreSerie :: String,
    actores :: [Actor],
    presupuestoAnual :: Float,
    temporadas :: Int,
    rating :: Float,
    estaCancelada :: Bool
}

data Actor = Actor{
    nombreActor :: String,
    sueldo :: Float,
    reestricciones :: [Bool]
}
--1 a
estaEnRojo :: Serie -> Bool
estaEnRojo  unaSerie = presupuestoAnual unaSerie > (sum . map sueldo . actores) unaSerie

--1b
esProblematica :: Serie -> Bool
esProblematica unaSerie = ((>3) . length . filter (>1) . map length . map reestricciones . actores) unaSerie

--2
conFavoritismos :: [Actor] -> Serie -> Serie
conFavoritismos unosActores unaSerie = agregarActoresFav unosActores . sacarLosPrimeros2Actores $ unaSerie

agregarActoresFav :: [Actor] -> Serie -> Serie
agregarActoresFav unosActores unaSerie =  unaSerie {actores = unosActores ++ actores unaSerie}

sacarLosPrimeros2Actores :: Serie -> Serie
sacarLosPrimeros2Actores unaSerie = unaSerie {actores = drop 2  . actores $ unaSerie }

timBurton = conFavoritismos [jhonnyDepp,helenaBonhamCarter]

jhonnyDepp :: Actor
jhonnyDepp = Actor "Johnny Depp" 20000000 []

helenaBonhamCarter :: Actor
helenaBonhamCarter = Actor "Helena Bonham Carter" 15000000 []

gatoPardeitor :: Serie -> Serie
gatoPardeitor unaSerie = unaSerie 

estireitor :: Serie -> Serie 
estireitor unaSerie = unaSerie {temporadas = temporadas unaSerie  *2}

desespereitor :: Serie -> Serie
desespereitor = gatoPardeitor .  agregarActoresFav [tobeyMaguire]. estireitor

tobeyMaguire :: Actor
tobeyMaguire = Actor "Tobey Maguire" 10000000 []

canceleitor :: Float -> Serie -> Serie
canceleitor ratingMin unaSerie
    | estaEnRojo unaSerie || esMenorARating ratingMin unaSerie = cancelarSerie unaSerie
    | otherwise = unaSerie

cancelarSerie :: Serie -> Serie
cancelarSerie unaSerie = unaSerie {estaCancelada = True}

esMenorARating :: Float -> Serie -> Bool
esMenorARating ratingMin unaSerie = ratingMin > rating unaSerie

