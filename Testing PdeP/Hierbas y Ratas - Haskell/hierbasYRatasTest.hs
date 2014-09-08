module HierbasYRatasTest where
import HierbasYRatas
import Test.Hspec
import Test.QuickCheck
import Control.Exception

(<<<) f1 fs = (\x ->(f1.map (\f -> f x)) fs)
iguales [_] = error "Se necesita al menos 2 elementos a comparar"
iguales (x:y:[]) = (x == y)
iguales (x:y:xs) = (x == y) && iguales (x:xs)

mickeyMouse = CRaton "Mickey Mouse" 81 5 ["disneymania", "hipotermia"]
conejillo = CRaton "Conejillo" 6 1 ["tuberculosis", "varicela", "endemia"]

instance Arbitrary Raton where
  arbitrary = elements [mickeyMouse, conejillo]

ratonConEdad anios = CRaton "" anios 0 []
ratonConPeso kilos = CRaton "" 0 kilos []
ratonConEnfermedades enfermedades = CRaton "" 0 0 enfermedades 

nada = \x -> x

main = hspec $ do
describe "Hierbas:" $ do
	describe "Hierba Buena" $ do
		it "rejuvenece la raiz cuadrada de su edad" $ do
			(edad.hierbaBuena.ratonConEdad) 9 `shouldBe` 6
	
	describe "Hierba Verde" $ do
		it "elimina las enfermedades que terminen de cierta forma" $ do
			(enfermedades.hierbaVerde "sis".ratonConEnfermedades) ["brucelosis", "sarampion", "tuberculosis"] `shouldBe` ["sarampion"]
	
	describe "Alcachofa" $ do
		it "en raton pesado reduce el 10% de su peso" $ do
			(peso.alcachofa.ratonConPeso) 10 `shouldBe` 9
	
		it "en raton no pesado reduce el 5% de su peso" $ do
			(peso.alcachofa.ratonConPeso) 1 `shouldBe` 0.95
	
	describe "Hierba Zort" $ do
		it "deja al raton con 0 anios de edad" $ do
			(edad.hierbaZort) mickeyMouse `shouldBe` 0
	
		it "elimina todas las enfermedades" $ do
			(enfermedades.hierbaZort) mickeyMouse `shouldBe` []
			
	describe "Medicamentos:" $ do
		describe "Medicamento" $ do
			it "administra varias hierbas" $ do
				medicamento [hierbaBuena, hierbaVerde "termia", alcachofa] mickeyMouse `shouldBe` CRaton "Mickey Mouse" 72 4.5 ["disneymania"]
			
		describe "PondsAntiAge" $ do
			it "administra 3 Hierbas Buenas" $ do
				(edad.pondsAntiAge.ratonConEdad) 16 `shouldBe` (edad.hierbaBuena.hierbaBuena.hierbaBuena.ratonConEdad) 16
			
			describe "administra una Alcachofa" $ do
				it "en raton pesado" $ do
					(peso.pondsAntiAge.ratonConPeso) 10 `shouldBe` 9
				it "en raton no pesado" $ do
					(peso.pondsAntiAge.ratonConPeso) 1 `shouldBe` 0.95
			
			it "administra una Alcachofa correctamente" $ property $
				iguales <<< [peso.pondsAntiAge, peso.alcachofa]
				-- \raton -> ((peso.pondsAntiAge) raton) == ((peso.alcachofa) raton)
		
		describe "ReduceFatFast" $ do
			it "elimina la 'obesidad'" $ do
				(reduceFatFast 1.ratonConEnfermedades) ["vagancia", "obesidad"] `shouldBe` ratonConEnfermedades ["vagancia"]
			
			it "administra tantas alcachofas como su potencia" $ do
				(reduceFatFast 3.ratonConPeso) 2.1 `shouldBe` (alcachofa.alcachofa.alcachofa.ratonConPeso) 2.1
			
			it "de potencia menor a 1 lanza error" $ do
				evaluate (reduceFatFast 0 mickeyMouse) `shouldThrow` errorCall "La cantidad debe ser mayor que 1"

		describe "PdePcilina" $ do
			it "elimina enfermedades terminadas en 'sis'" $ do
				(pdepCilina.ratonConEnfermedades) ["brucelosis", "sarampion", "tuberculosis"] `shouldBe` ratonConEnfermedades ["sarampion"]
		
			it "elimina enfermedades terminadas en 'itis'" $ do
				(pdepCilina.ratonConEnfermedades) ["otitis", "sarampion"] `shouldBe` ratonConEnfermedades ["sarampion"]
		
			it "elimina enfermedades terminadas en 'emia'" $ do
				(pdepCilina.ratonConEnfermedades) ["anemia"] `shouldBe` ratonConEnfermedades []
		
			it "elimina enfermedades terminadas en 'cocos'" $ do
				(pdepCilina.ratonConEnfermedades) ["diplococos", "varicela"] `shouldBe` ratonConEnfermedades ["varicela"]

	describe "Experimentos:" $ do
		describe "Cantidad ideal" $ do
			it "retorna el primer numero natural que cumple con una condicion" $ do
				 cantidadIdeal (>5) `shouldBe` 6
			
		describe "Despues de administrarles el medicamento, estan mas lindos que nunca" $ do
			it "se cumple cuando todos los ratones pesan menos que 1 kg" $ do
				estanMasLindosQueNunca alcachofa [conejillo, ratonConPeso 0.9, ratonConPeso 0.5] `shouldBe` True
			
			it "no se cumple si algun raton pesa al menos 1 kg" $ do
				estanMasLindosQueNunca nada [conejillo, ratonConPeso 0.5] `shouldBe` False
				
		describe "DosisDeJorgeHane" $ do
			it "retorna la potencia de ReduceFatFast necesaria para que todos los ratones queden mas lindos que nunca" $ do
				 dosisDeJorgeHane [conejillo, mickeyMouse] `shouldBe` 22
		