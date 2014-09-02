module PepitaFuncional where
import HaskellObjetoso
import Prelude hiding ((.))

------------------------------ Golondrina --------------------------------------
-- Atributos
data Golondrina = 
	Golondrina{
	energia :: Float,
	ciudad :: Ciudad} deriving (Show, Eq)
-- Interfaz
-- Getters/Setters
getEnergia = energia·dataObject
setEnergia cant = settearEnergia cant·dataObject
getCiudad = ciudad·dataObject
setCiudad (Instance ciudad) = settearCiudad ciudad·dataObject
-- Métodos
come grms golondrina = golondrina.aumentarEnergia (10 * grms)
aumentarEnergia cant golondrina = golondrina.setEnergia (dataObject (golondrina.getEnergia!(+ cant)))
-- volaHacia ciudadNueva = setCiudad ciudadNueva·disminuirEnergiaHasta ciudadNueva
-- disminuirEnergiaHasta ciudadNueva = (disminuirEnergia·(*10)) << (\g -> ciudadNueva.(distanciaHasta·getCiudad) g)



settearEnergia cant (Golondrina _ ciudad) = Golondrina cant ciudad
settearCiudad ciudad (Golondrina energia _) = Golondrina energia ciudad
disminuirEnergia cant golondrina@(Golondrina energia ciudad)
	|cant > energia = error $ show golondrina ++ " no posee energía suficiente."
	|otherwise = Golondrina (energia - cant) ciudad

------------------------------- Ciudad ---------------------------------------
-- Atributos
data Ciudad = 
	Ciudad{
	nombre :: String,
	km :: Float} deriving (Show, Eq)

-- Interfaz
getNombre = Instance·nombre
getKm = Instance·km
distanciaHasta (Instance ciudad) = abs·(-) (km ciudad)·km

------------------------------------------------------------------------------

dataPepita = Golondrina 10000 dataBsAs
pepita = Instance dataPepita

dataBsAs = Ciudad "BsAs" 0
bsAs = Instance dataBsAs

dataGesell = Ciudad "V. Gesell" 376
villaGesell = Instance dataGesell
