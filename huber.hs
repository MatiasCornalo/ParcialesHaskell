data Chofer = Chofer{
    nombreChofer :: String,
    km :: Int,
    viajes :: [Viaje],
    condicion :: Condicion
}

data Viaje = Viaje{
    fecha :: (Int,Int,Int),
    cliente :: Cliente,
    costo :: Int
}

data Cliente = Cliente{
    domicilio :: String,
    nombre :: String
}

type Condicion = Viaje -> Bool
cualquierViaje :: Condicion
cualquierViaje unViaje = True

esCostoDeViajeMayorA200 :: Condicion
esCostoDeViajeMayorA200 unViaje = costo unViaje > 200

esNombreClienteMayorA :: Int -> Condicion
esNombreClienteMayorA n = (>=n) . length . nombre . cliente 

noViveEn :: String -> Condicion
noViveEn zona = (/=zona) . domicilio . cliente

lucas :: Cliente
lucas = Cliente "Victoria" "Lucas"

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [Viaje (20,04,2017) lucas 150] (noViveEn "Olivos")

alejandra :: Chofer
alejandra = Chofer "Alejandra" 180000 [] cualquierViaje

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer = condicion chofer $ viaje  

liquidacion :: Chofer -> Int
liquidacion = sum . map costo . viajes

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje = agregarViaje viaje . head . filter (flip condicion viaje)

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer {viajes = viaje : viajes chofer}

nitoInfy :: Chofer
nitoInfy = Chofer "Nito Infy" 70000 (repetirViaje (Viaje (11,03,2017) lucas 50)) (esNombreClienteMayorA 3)

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

-- No se puede calcular debido a que va a estar infinitamente esperando a calcular la longitud de la lista
-- Si, se puede saber  
