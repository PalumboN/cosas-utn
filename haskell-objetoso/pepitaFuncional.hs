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
-- Setters
setEnergia cant golondrina = golondrina { energia = cant }
setCiudad (Instance ciudad) golondrina = golondrina { ciudad = ciudad }
-- 
come grms = Method (\g -> g.aumentarEnergia (10*grms))
volaHacia ciudadNueva = Method (\g -> g.disminuirEnergiaHasta ciudadNueva.setCiudad ciudadNueva)
disminuirEnergiaHasta ciudadNueva = Method (\g -> g.disminuirEnergia (10*g.ciudad.distanciaHasta ciudadNueva))


aumentarEnergia cant = setEnergia << ((+cant)·energia)
disminuirEnergia (Instance cant) golondrina
	|cant > (energia golondrina) = error $ show golondrina ++ " no posee energía suficiente = " ++ show cant
	|otherwise = golondrina { energia = ((energia golondrina) - cant) }

------------------------------- Ciudad ---------------------------------------
-- Atributos
data Ciudad = 
	Ciudad{
	nombre :: String,
	km :: Float} deriving (Show, Eq)

-- Interfaz
distanciaHasta otraCiudad = Method (\c -> abs·(-) (c.km) $ otraCiudad.km)

------------------------------------------------------------------------------

dataPepita = Golondrina 9999 dataBsAs
pepita = Instance dataPepita

dataBsAs = Ciudad "BsAs" 0
bsAs = Instance dataBsAs

dataGesell = Ciudad "V. Gesell" 376
villaGesell = Instance dataGesell
