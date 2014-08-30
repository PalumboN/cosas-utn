module PepitaFuncional where
import HaskellObjetoso
import Prelude hiding ((.))

------------------------------ Golondrina --------------------------------------
data Golondrina = 
	Golondrina{
	energia :: Float,
	ciudad :: Ciudad} deriving (Show, Eq)

getEnergia = New·energia
setEnergia cant = New·settearEnergia cant
getCiudad = New·ciudad
setCiudad (New ciudad) = New·settearCiudad ciudad
come grms = New·aumentarEnergia (10 * grms)
volaHacia ciudadNueva = setCiudad ciudadNueva·disminuirEnergiaHasta ciudadNueva
disminuirEnergiaHasta ciudadNueva = (disminuirEnergia·(*10)) << (\g -> ciudadNueva.(distanciaHasta·getCiudad) g)


settearEnergia cant (Golondrina _ ciudad) = Golondrina cant ciudad
settearCiudad ciudad (Golondrina energia _) = Golondrina energia ciudad
aumentarEnergia cant (Golondrina energia ciudad) = Golondrina (energia + cant) ciudad
disminuirEnergia cant golondrina@(Golondrina energia ciudad)
	|cant > energia = error $ show golondrina ++ " no posee energía suficiente."
	|otherwise = Golondrina (energia - cant) ciudad

------------------------------- Ciudad ---------------------------------------
data Ciudad = 
	Ciudad{
	nombre :: String,
	km :: Float} deriving (Show, Eq)

getNombre = New·nombre
getKm = New·km
distanciaHasta (New ciudad) = abs·(-) (km ciudad)·km

------------------------------------------------------------------------------

dataPepita = Golondrina 10000 dataBsAs
pepita = New dataPepita

dataBsAs = Ciudad "BsAs" 0
bsAs = New dataBsAs

dataGesell = Ciudad "V. Gesell" 376
villaGesell = New dataGesell
