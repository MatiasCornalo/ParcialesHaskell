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
type Productor = Serie -> Serie
conFavoritismos :: [Actor] -> Productor
conFavoritismos unosActores unaSerie = agregarActoresFav unosActores . sacarLosPrimeros2Actores $ unaSerie

agregarActoresFav :: [Actor] -> Serie -> Serie
agregarActoresFav unosActores unaSerie =  unaSerie {actores = unosActores ++ actores unaSerie}

sacarLosPrimeros2Actores :: Serie -> Serie
sacarLosPrimeros2Actores unaSerie = unaSerie {actores = drop 2  . actores $ unaSerie }

timBurton :: Productor
timBurton = conFavoritismos [jhonnyDepp,helenaBonhamCarter]

jhonnyDepp :: Actor
jhonnyDepp = Actor "Johnny Depp" 20000000 []

helenaBonhamCarter :: Actor
helenaBonhamCarter = Actor "Helena Bonham Carter" 15000000 []

gatoPardeitor :: Productor
gatoPardeitor unaSerie = unaSerie 

estireitor :: Productor
estireitor unaSerie = unaSerie {temporadas = temporadas unaSerie  *2}

desespereitor :: Productor
desespereitor = gatoPardeitor .  agregarActoresFav [tobeyMaguire]. estireitor

tobeyMaguire :: Actor
tobeyMaguire = Actor "Tobey Maguire" 10000000 []

canceleitor :: Float -> Productor
canceleitor ratingMin unaSerie
    | estaEnRojo unaSerie || esMenorARating ratingMin unaSerie = cancelarSerie unaSerie
    | otherwise = unaSerie

cancelarSerie :: Serie -> Serie
cancelarSerie unaSerie = unaSerie {estaCancelada = True}

esMenorARating :: Float -> Serie -> Bool
esMenorARating ratingMin unaSerie = ratingMin > rating unaSerie

--3
bienestar :: Serie -> Int
bienestar unaSerie
    |   estaCancelada unaSerie              = 0
    |   temporadas unaSerie > 4             = 5
    |   (<10) . length . actores $ unaSerie = 3 
    |   otherwise                           = max 2 (max (10 - actoresConReestricciones unaSerie) ((10 - temporadas unaSerie) * 2))

actoresConReestricciones :: Serie -> Int
actoresConReestricciones = length . filter ((>0). length) . map reestricciones . actores 

--Ejercicio 4 
aplicarSegunEfectividad :: [Productor] -> [Serie] -> [Serie]
aplicarSegunEfectividad productores series =  undefined

--5a) gatopardeitor si se puede aplicar por el concepto de lazy evaluation porque no necesita saber la cantidad de actores
--5b) En este si, por lo tanto siempre estara calculando la cantidad de actores y por lo tanto la funcion nunca nos devolvera una serie nueva. Depende de la cantidad de actores de la lista de tipo Actor de la serie
-- Ejercicio 6
esControvertida :: Serie -> Bool
esControvertida (Serie _ [] _ _ _ _) = True
esControvertida unaSerie = (sueldo . head . actores) unaSerie > (sueldo . head .drop 1 . actores) unaSerie && esControvertida (retirarActor unaSerie)

retirarActor :: Serie -> Serie
retirarActor unaSerie = unaSerie {actores = (drop 1 . actores) unaSerie}

--Ejercicio 7
funcionLoca :: (Integral b, Foldable t) => (Int -> b) -> (a1 -> t a2) -> [a1] -> [Int]
funcionLoca x y z = filter (even.x) . map (length.y) $ z
-- Funcion que recibe una lista de cualquier cosa, se le aplica la funcion y (que recibe una lista y devuelve otra) y luego filtra en base si son pares luego de aplicarle x (que recibe una lista y devuelve un entero)
-- Obteniendo asi una lista de numeros pares