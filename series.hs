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
agregarActoresFav unosActores unaSerie =  