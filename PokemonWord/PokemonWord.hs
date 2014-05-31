module PokemonWord where

(%) numero porcentaje = numero * porcentaje `div` 100

mayorSegun f [x] = x
mayorSegun f (x1:x2:xs)
	|f x1 > f x2 = mayorSegun f (x1:xs)
	|otherwise = mayorSegun f (x2:xs)

--------------------------------------------------------------------------------------------------------
---------------------------------------- Tipos Pokemon
-- Tipo Nombre TiposContraLosQueGana
data PokeTipo = Tipo String [PokeTipo]

instance Ord PokeTipo where	
	(Tipo _ tiposDebiles) > otroTipo = elem otroTipo tiposDebiles
	unTipo < (Tipo _ tiposDebiles) = elem unTipo tiposDebiles

instance Eq PokeTipo where
	(Tipo unNombre _) == (Tipo otroNombre _) = unNombre == otroNombre
	
instance Show PokeTipo where
	show (Tipo nombre _) = show nombre
	
-- Devuelve el pokeTipado interesado en comparar los tipos despues de aplicarse el efecto del PokeTipo
contra unPokeTipado otroPokeTipado
	|tipo unPokeTipado > tipo otroPokeTipado = tipadoAFavor unPokeTipado
	|tipo unPokeTipado < tipo otroPokeTipado = tipadoEnContra unPokeTipado
	|otherwise = unPokeTipado

--------------------------------------------------------------------------------------------------------
-- Clase que debe implementar todo tipo con PokeTipo
class TipadoClass algoConPokeTipo where
	tipo :: algoConPokeTipo -> PokeTipo
	tipadoAFavor :: algoConPokeTipo -> algoConPokeTipo
	tipadoEnContra :: algoConPokeTipo -> algoConPokeTipo
	
--------------------------------------------------------------------------------------------------------
---------------------------------------- Ataque
data Ataque =
	AtaqueDañino {
	tipoA :: PokeTipo,
	daño :: Int
	} |
	AtaqueCurativo{
	tipoC :: PokeTipo,
	curacion :: Int
	} |
	AtaqueEspecial{
	tipoE :: PokeTipo,
	efecto :: Pokemon -> Pokemon
	}
	
puntosDeDaño (AtaqueCurativo _ _) = 0
puntosDeDaño ataque = daño ataque

puntosDeVida (AtaqueDañino _ _) = 0
puntosDeVida ataque = curacion ataque

aplicar _ atacado (AtaqueDañino _ daño) = reducirVida daño atacado
aplicar atacante _ (AtaqueCurativo _ curacion) = aumentarVida curacion atacante
aplicar atacante atacado (AtaqueEspecial _ efecto) = efecto atacado

debilitaA pokemonAtacado ataque = puntosDeDaño ataque >= vida pokemonAtacado

instance Show Ataque where
	show (AtaqueDañino _ daño) = "Dañino(" ++ show daño ++ ")"
	show (AtaqueCurativo _ ptosDeVida) = "Curativo(" ++ show ptosDeVida ++ ")"
	
instance TipadoClass Ataque where
	tipo ataque = tipoAtaque ataque
	tipadoAFavor ataque = ataqueAFavor ataque
	tipadoEnContra ataque = ataqueEnContra ataque

tipoAtaque (AtaqueDañino tipo _) = tipo
tipoAtaque (AtaqueCurativo tipo _) = tipo
tipoAtaque (AtaqueEspecial tipo _) = tipo

ataqueAFavor (AtaqueDañino tipo daño) = AtaqueDañino tipo (daño * 2) -- Saca el doble
ataqueAFavor (AtaqueCurativo tipo curacion) = AtaqueCurativo tipo (curacion % 120) -- Aumenta un 20%
ataqueAFavor (AtaqueEspecial tipo efecto) = AtaqueEspecial tipo (reducirVida 15.efecto) -- Le quita 15 de vida dsp de aplicarse el efecto

ataqueEnContra (AtaqueDañino tipo daño) = AtaqueDañino tipo (daño - 30) -- Reduce el daño en 30
ataqueEnContra (AtaqueCurativo tipo curacion) = AtaqueCurativo tipo (curacion % 120) -- Nada
ataqueEnContra (AtaqueEspecial tipo efecto) = AtaqueEspecial tipo (efectoDebil efecto) -- Solo se aplica si el rival esta medio muerto

efectoDebil efecto atacado 
	| estasMedioMuerto atacado = efecto atacado
	| otherwise = atacado


--------------------------------------------------------------------------------------------------------
---------------------------------------- Pokemon
data Pokemon = 
	Pokemon {
	nombre :: String,
	vida :: Int,
	vidaMax :: Int,
	tipoPokemon :: PokeTipo,
	ataques :: [Ataque],
	estrategia :: Estrategia
	} |
	Debilitado Pokemon

instance TipadoClass Pokemon where
	tipo pokemon = tipoPokemon pokemon
	tipadoAFavor pokemon = pokemon
	tipadoEnContra pokemon = pokemon

instance Show Pokemon where
	show (Pokemon nombre vida vidaMax tipo ataques _) = "nombre: " ++ show nombre ++ ", vida: " ++ show vida ++ "/" ++ show vidaMax ++ ", tipo: " ++ show tipo ++ ", ataques: " ++ show ataques
	show (Debilitado pokemon) = "DERROTADO --  " ++ show pokemon ++ "  --"

instance Eq Pokemon where
	(Pokemon nombre1 _ _ _ _ _) == (Pokemon nombre2 _ _ _ _ _) = nombre1 == nombre2
	unPokemon == (Debilitado otroPokemon) = unPokemon == otroPokemon
	(Debilitado otroPokemon) == unPokemon = unPokemon == otroPokemon

reducirVida puntosDeDaño pokemon
	|puntosDeDaño < vida pokemon = aplicarVida (flip (-) puntosDeDaño) pokemon 
	|otherwise = Debilitado $ aplicarVida (*0) pokemon
aumentarVida curacion pokemon = aplicarVida (+ curacion) pokemon 
aplicarVida f (Pokemon n v vm t a e)  = Pokemon n (f v) vm t a e

estasMedioMuerto pokemon = vida pokemon < (vidaMax pokemon `div` 2)

ataqueAUsar ataques = head ataques


----------------------------------------------------------------------------------------------------------
---------------------------------------- Estrategia
-- Las estrategias son las que determinan qué ataque debe usar un pokemon contra otro. Por ahora existen 4 estrategias, pero podrían agregarse más:
-- siemprePrimero: siempe elije el primer ataque.
-- siempreElMasDoloroso: siempre elije el ataque que más daño hará.
-- segura: si está medio muerto y posee algún ataque curativo usa el que más cura. Sino aquel que más daño hará.
-- experta: si hay un ataque que Debilite al contrincante escoge ese, sino lleva una estrategia segura.

type Estrategia = Pokemon -> Pokemon -> [Ataque] -> Ataque

siemprePrimero _ _ ataques = head ataques
siempreElMasDoloroso _ _ ataques = mayorSegun puntosDeDaño ataques
segura pokemonAtacante pokemonAtacado ataques 
	|estasMedioMuerto pokemonAtacante && any ((>0).puntosDeVida) ataques = mayorSegun puntosDeVida ataques
	|otherwise = siempreElMasDoloroso pokemonAtacante pokemonAtacado ataques
experta pokemonAtacante pokemonAtacado ataques 
	|any (debilitaA pokemonAtacado) ataques = (head.filter (debilitaA pokemonAtacado)) ataques
	|otherwise = segura pokemonAtacante pokemonAtacado ataques 

estrategiaLucha pokemonAtacante _ = (head.ataques) pokemonAtacante

----------------------------------------------------------------------------------------------------------
---------------------------------------- Batalla
data Resultado a = Pelea a a | Ganador a 

ganador (Pelea pokemonAtacante pokemonAtacado) = ganador (getResultado pokemonAtacante pokemonAtacado (atacar pokemonAtacante pokemonAtacado))
ganador (Ganador pokemon) = pokemon

getResultado pokemonAtacante pokemonAtacado pokemonAfectado
	|pokemonAtacante == pokemonAfectado = turnoSiguiente pokemonAfectado pokemonAtacado 
	|pokemonAtacado == pokemonAfectado = turnoSiguiente pokemonAtacante pokemonAfectado

turnoSiguiente (Debilitado _) pokemon = Ganador pokemon
turnoSiguiente pokemon (Debilitado _) = Ganador pokemon
turnoSiguiente pokemonAtacante pokemonAtacado = Pelea pokemonAtacado pokemonAtacante

----------------------------------------------------------------------------------------------------------
---------------------------------------- Casos de uso
-- Crear la funcion "elegirAtaqueContra" que devuelva el ataque elegido por un pokemon contra otro, teniendo en cuenta su estrategia.
elegirAtaqueContra pokemonAtacante pokemonAtacado = (estrategia pokemonAtacante) pokemonAtacante pokemonAtacado $ map (`contra` pokemonAtacado) (ataques pokemonAtacante)
	
	
-- Hacer que un pokemon ataque a otro con un ataque, esto devuelve el pokemon que fue afectado luego de sufrir el ataque elegido por el pokemon.
atacar pokemonAtacante pokemonAtacado = aplicar pokemonAtacante pokemonAtacado $ elegirAtaqueContra pokemonAtacante pokemonAtacado 

-- Saber el ganador al pelear entre dos pokemones, esto significa que se atacan, por turnos, hasta que uno queda debilitado. Un pokemon queda debilitado cuando su vida se reduce a 0.
ganadorAlPelear unPokemon otroPokemon = ganador (Pelea unPokemon otroPokemon)

