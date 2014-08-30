module PepitaFuncionalTest where
import PepitaFuncional
import Test.Hspec
import Test.QuickCheck
import Control.Exception

unaGolondrina = Golondrina 100

main = hspec $ do
describe "Golondrina:" $ do
	it "energia retorna la energia de una golondrina" $ do
		unaGolondrina.energia `shouldBe` 100

	it "Si vuela disminuye su energia correctamente" $ do
		unaGolondrina.vola `shouldBe` Golondrina 50
	
	it "Si come aumenta su energia correctamente" $ do
		unaGolondrina.come 10 `shouldBe` Golondrina 200