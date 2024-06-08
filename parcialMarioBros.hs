
import Text.Show.Functions
import Data.Char(isUpper)
import Data.List(genericLength)


data Plomero = Plomero {
nombrePlomero :: String,
cajaDeHerramientas :: [Herramienta],
historialDeReparaciones :: [Reparacion],
dinero :: Float 
}



data Herramienta = Herramienta{
    nombreDeHerramienta :: String,
    precio :: Float,
    materialEmpunadura :: Material  
} deriving (Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Eq)

--PUNTO 1 --
mario = Plomero {
nombrePlomero = "Mario",
cajaDeHerramientas = [llaveInglesa, martillo],
historialDeReparaciones = [],
dinero = 1200
}

martillo = Herramienta {
nombreDeHerramienta = "martillo",
precio = 20,
materialEmpunadura = Madera
}

llaveInglesa = Herramienta {
nombreDeHerramienta = "llave inglesa",
precio = 200,
materialEmpunadura = Hierro 
}

-- PUNTO 1 B 

wario = Plomero {
nombrePlomero = "Wario " ,
cajaDeHerramientas = infinitasLLavesFrancesas,
historialDeReparaciones = [],
dinero = 50
}

infinitasLLavesFrancesas :: [Herramienta]
infinitasLLavesFrancesas = repeat llaveFrancesa

llaveFrancesa = Herramienta {
nombreDeHerramienta = "llave francesa",
precio = 1,
materialEmpunadura = Hierro
}

-- PUNTO 2 --
tieneLaHerramienta :: Herramienta -> Plomero -> Bool 
tieneLaHerramienta unaHerramienta unPlomero = elem unaHerramienta (cajaDeHerramientas unPlomero)

esMalvado :: Plomero -> Bool 
esMalvado unPlomero = (take 2. nombrePlomero) unPlomero == "wa"

puedePagar :: Herramienta -> Plomero -> Bool 
puedePagar unaHerramienta unPlomero = dinero unPlomero >= precio unaHerramienta

--PUNTO 3--
esBuena :: Herramienta -> Bool 
esBuena unaHerramineta = materialEmpunadura unaHerramineta == Hierro && precio unaHerramineta >= 10000 || unaHerramineta == martillo && materialEmpunadura unaHerramineta == Goma || materialEmpunadura unaHerramineta == Madera 

--PUNTO 4 --
comprarHerramienta :: Herramienta -> Plomero -> Plomero 
comprarHerramienta unaHerramienta unPlomero 
    | puedePagar unaHerramienta unPlomero =(mapCajaDeHerramienta (unaHerramienta :).mapDinero (subtract (precio unaHerramienta))) unPlomero
    | otherwise = unPlomero 

mapDinero :: (Float -> Float) -> Plomero -> Plomero 
mapDinero f unPlomero = unPlomero {dinero = f $ dinero unPlomero}

mapCajaDeHerramienta :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero 
mapCajaDeHerramienta f unPlomero = unPlomero {cajaDeHerramientas = f $ cajaDeHerramientas unPlomero}

-- PUNTO 5 -- 

data Reparacion = Reparacion {
descripcion :: String,
requerimiento :: Requerimiento 
}

type Requerimiento = Plomero -> Bool  

filtracionDeAgua = Reparacion {
descripcion = "tapar agujero de la pared",
requerimiento = tieneLaHerramienta llaveInglesa
}

-- PUNTO 5 B --

esDificil :: Reparacion -> Bool 
esDificil unaReparacion = (length.descripcion) unaReparacion > 100 && esUnGrito unaReparacion

esUnGrito :: Reparacion -> Bool 
esUnGrito = all isUpper . descripcion

-- PUNTO 5 C --

presupuesto :: Reparacion -> Float
presupuesto unaReparacion = tipoDeDescripcion unaReparacion

    where tipoDeDescripcion :: Reparacion -> Float
          tipoDeDescripcion unaReparacion = 300 * genericLength (descripcion unaReparacion)  / 100  

realizarReparacion :: Plomero -> Reparacion -> Plomero 
realizarReparacion unPlomero unaReparacion 
    | cumpleRequerimientoDe unaReparacion unPlomero  = (adicionalVisita unaReparacion.mapReparaciones (unaReparacion:).mapDinero (+ presupuesto unaReparacion)) unPlomero
    | otherwise = mapDinero (+ 100) unPlomero

cumpleRequerimientoDe :: Reparacion -> Plomero -> Bool 
cumpleRequerimientoDe unaReparacion unPlomero = requerimiento unaReparacion unPlomero || esMalvado unPlomero && tieneLaHerramienta martillo unPlomero


adicionalVisita :: Reparacion -> Plomero -> Plomero 
adicionalVisita unaReparacion unPlomero 
    | esMalvado unPlomero = mapCajaDeHerramienta (destornillador :) unPlomero
    | (not.esMalvado) unPlomero && esDificil unaReparacion =  mapCajaDeHerramienta (filter (not.esBuena)) unPlomero 
    | otherwise = mapCajaDeHerramienta (drop 1) unPlomero

destornillador = Herramienta {
nombreDeHerramienta = "destornillador",
precio = 0,
materialEmpunadura = Plastico 
}


mapReparaciones :: ([Reparacion]  -> [Reparacion]) -> Plomero -> Plomero 
mapReparaciones f unPlomero = unPlomero {historialDeReparaciones = f $ historialDeReparaciones unPlomero}

-- PUNTO 7 --
type JornadaDeTrabajo = [Reparacion] 

realizarJornadaDeTrabajo :: JornadaDeTrabajo -> Plomero -> Plomero 
realizarJornadaDeTrabajo unaJornadaDeTrabajo unPlomero = foldl realizarReparacion unPlomero unaJornadaDeTrabajo



