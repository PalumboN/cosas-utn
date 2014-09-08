-- https://docs.google.com/document/d/12mjF0uM9f4Yf6TLs8f2t9vf27wen90PKp1FHjygder4/edit#
module HierbasYRatas where

(<<) f1 f2 = (\x -> f1 (f2 x) x)

tails [] = [""]
tails palabra@(_:xs) = [palabra] ++ tails xs

(%) porcentaje total = porcentaje * total / 100

data Raton = 
	CRaton{
	nombre :: String,
	edad :: Float,
	peso :: Float,
	enfermedades :: [String]} deriving (Show, Eq)

-- 1)
hierbaBuena = rejuvenecer << (sqrt.edad)
hierbaVerde terminacion = eliminarEnfermedades << (terminadasCon terminacion.enfermedades)
alcachofa raton	|((>2).peso) raton = perderPesoPorcentual 10 raton
				|otherwise = perderPesoPorcentual 5 raton
hierbaZort = pinky

-- 2)
medicamento hierbas raton = foldl (\raton hierba -> hierba raton) raton hierbas
pondsAntiAge = medicamento $ [alcachofa] ++ multiplicarHierbas 3 hierbaBuena
reduceFatFast potencia = medicamento $ [hierbaVerde "obesidad"] ++ multiplicarHierbas potencia alcachofa
pdepCilina = (medicamento.map hierbaVerde) ["sis", "itis", "emia", "cocos"]

-- 3)
cantidadIdeal condicion = (head.filter condicion) [1..]
estanMasLindosQueNunca medicamento = all estaMasLindosQueNunca.map medicamento
dosisDeJorgeHane ratones = cantidadIdeal (\potencia -> estanMasLindosQueNunca (reduceFatFast potencia) ratones)


quitar enfermedadesCuradas = filter (not.flip elem enfermedadesCuradas)

terminadasCon terminacion = filter (terminaCon terminacion)

terminaCon terminacion = elem terminacion.tails

perderPesoPorcentual porcentaje = perderPeso << ((porcentaje%).peso)

rejuvenecer cantAnios (CRaton nombre edad peso enfermedades) = CRaton nombre (edad - cantAnios) peso enfermedades

eliminarEnfermedades enfermedadesCuradas (CRaton nombre edad peso enfermedades) = CRaton nombre edad peso (quitar enfermedadesCuradas enfermedades)

perderPeso kilos (CRaton nombre edad peso enfermedades) = CRaton nombre edad (peso - kilos) enfermedades

pinky (CRaton nombre _ peso _) = CRaton nombre 0 peso []

multiplicarHierbas cantidad hierba
	|cantidad < 1 = error "La cantidad debe ser mayor que 1"
	|otherwise = foldl (\lista _ -> lista ++ [hierba]) [] [1..cantidad]

estaMasLindosQueNunca = (<1).peso