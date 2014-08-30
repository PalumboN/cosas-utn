module PepitaFuncional where
import HaskellObjetoso
import Prelude hiding ((.))

------------------------------ Ciudad --------------------------------------
data Ciudad = 
	Ciudad{
	nombre :: String,
	km :: Float} deriving (Show, Eq)

dataBsAs = Ciudad "BsAs" 0
bsAs = New dataBsAs

dataGesell = Ciudad "V. Gesell" 376
villaGesell = New dataGesell

-- distanciaEntre ciudad1 ciudad2 = ciudad1.km.(-).(ciudad2.km).abs

------------------------------ Golondrina --------------------------------------
data Golondrina = 
	Golondrina{
	energia :: Float,
	ciudad :: Ciudad} deriving (Show, Eq)

class GolondrinaClass golondrina where
	getEnergia :: Object golondrina -> Float
	getCiudad :: Object golondrina -> Ciudad
instance GolondrinaClass Golondrina where
	getEnergia (New golondrina) = energia golondrina
	getCiudad (New golondrina) = ciudad golondrina
	

dataPepita = Golondrina 10000 dataBsAs
pepita = New dataPepita

-- come grms golondrina = golondrina.aumentarEnergia (10 * grms)
-- volaHacia ciudadNueva golondrina = (disminuirEnergia&(*10)) << ((distanciaEntre (ciudadNueva))&ciudad)

-- aumentarEnergia cant golondrina = Golondrina (golondrina.getEnergia.getData (+cant)) (golondrina.getCiudad.getData)
-- aumentarEnergia cant (Golondrina energia ciudad) = Golondrina (energia + cant) ciudad
disminuirEnergia cant golondrina@(Golondrina energia ciudad)
	|cant > energia = error (show golondrina ++ " no posee energía suficiente." )
	|otherwise = Golondrina (energia - cant) ciudad