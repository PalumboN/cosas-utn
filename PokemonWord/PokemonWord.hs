module PokemonWord where

(%) numero porcentaje = numero * porcentaje `div` 100

mayorSegun f [x] = x
mayorSegun f (x1:x2:xs)
	|f x1 > f x2 = mayorSegun f (x1:xs)
	|otherwise = mayorSegun f (x2:xs)
	
(<<) f1 f2 = \a -> (f1 a.f2) a

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
	efecto :: Batalla Pokemon -> Batalla Pokemon
	}
	
puntosDeDaño (AtaqueCurativo _ _) = 0
puntosDeDaño ataque = daño ataque

puntosDeVida (AtaqueDañino _ _) = 0
puntosDeVida ataque = curacion ataque

aplicarAtaque (Pelea pokemonAtacante pokemonAtacado) (AtaqueDañino _ daño) = Pelea pokemonAtacante (reducirVida daño pokemonAtacado)
aplicarAtaque (Pelea pokemonAtacante pokemonAtacado) (AtaqueCurativo _ curacion) = Pelea (aumentarVida curacion pokemonAtacante) pokemonAtacado
aplicarAtaque batalla (AtaqueEspecial _ efecto) = efecto batalla

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

getAtaquesContra (Pelea pokemonAtacante pokemonAtacado) = map (`contra` pokemonAtacado) (ataques pokemonAtacante) 

getEstrategia batalla = ((estrategia.atacante)<<(\batalla -> batalla)) batalla

----------------------------------------------------------------------------------------------------------
---------------------------------------- Estrategia
-- Las estrategias son las que determinan qué ataque debe usar un pokemon contra otro. Por ahora existen 4 estrategias, pero podrían agregarse más:
-- siemprePrimero: siempe elije el primer ataque.
-- siempreElMasDoloroso: siempre elije el ataque que más daño hará.
-- segura: si está medio muerto y posee algún ataque curativo usa el que más cura. Sino aquel que más daño hará.
-- experta: si hay un ataque que Debilite al contrincante escoge ese, sino lleva una estrategia segura.

type Estrategia = Batalla Pokemon -> [Ataque] -> Ataque

estrategiaSiemprePrimero _ ataques = head ataques
estrategiaSiempreElMasDoloroso _ ataques = mayorSegun puntosDeDaño ataques
estrategiaSegura batalla ataques 
	|(estasMedioMuerto.atacante) batalla && any ((>0).puntosDeVida) ataques = mayorSegun puntosDeVida ataques
	|otherwise = estrategiaSiempreElMasDoloroso batalla ataques
estrategiEexperta batalla ataques 
	|any ((debilitaA.atacado) batalla) ataques = (head.filter ((debilitaA.atacado) batalla)) ataques
	|otherwise = estrategiaSegura batalla ataques 

estrategiaLucha = head.ataques.atacante

----------------------------------------------------------------------------------------------------------
---------------------------------------- Batalla
data Batalla a = Pelea a a | Ganador a

instance Show a => Show (Batalla a) where
	show (Pelea pokemonAtacante pokemonAtacado) = show pokemonAtacante ++ " >>>>VS<<<< " ++ show pokemonAtacado
	show (Ganador pokemon) = show pokemon
	
getGanador (Ganador pokemon) = pokemon
getGanador (Pelea pokemonAtacante pokemonAtacado) = (getGanador.getResultado.turnoSiguiente.atacar pokemonAtacante) pokemonAtacado

getResultado (Pelea (Debilitado _) pokemon) = Ganador pokemon
getResultado (Pelea pokemon (Debilitado _)) = Ganador pokemon
getResultado batalla = batalla

turnoSiguiente (Pelea pokemonAtacante pokemonAtacado) = Pelea pokemonAtacado pokemonAtacante
turnoSiguiente (Ganador pokemon) = errorGanador pokemon

atacante (Pelea pokemonAtacante _) = pokemonAtacante
atacante (Ganador pokemon) = errorGanador pokemon
atacado (Pelea _ pokemonAtacado) = pokemonAtacado
atacado (Ganador pokemon) = errorGanador pokemon

errorGanador pokemon = error ("Ganador no encontrado: " ++ show pokemon)

----------------------------------------------------------------------------------------------------------
---------------------------------------- Casos de uso
-- Hacer los ataques especiales.

-- Crear la funcion "elegirAtaqueContra" que devuelva el ataque elegido por un pokemon contra otro, teniendo en cuenta su estrategia.
elegirAtaqueContra pokemonAtacante pokemonAtacado = (getEstrategia<<getAtaquesContra) (Pelea pokemonAtacante pokemonAtacado)
	
-- Hacer que un pokemon ataque a otro con un ataque, esto devuelve el resultado del ataque.
atacar pokemonAtacante pokemonAtacado = aplicarAtaque (Pelea pokemonAtacante pokemonAtacado) $ elegirAtaqueContra pokemonAtacante pokemonAtacado 

-- Saber el ganador al pelear entre dos pokemones, esto significa que se atacan, por turnos, hasta que uno queda debilitado. Un pokemon queda debilitado cuando su vida se reduce a 0.
ganadorAlPelear unPokemon otroPokemon = getGanador (Pelea unPokemon otroPokemon)

-- Hacer que los pokemones tengan estado (paralizado, envenenado)

-------------------------------------------------------------------------------------------
---------------------------------------- Pokemon
newPokemon nombre vida tipo ataques = Pokemon nombre vida vida tipo ataques estrategiaSiemprePrimero

pikachu = newPokemon "Pikachu" 70 electrico [impactrueno,colaDeAcero,arañazo]
charmander = newPokemon "Charmander" 80 fuego [lanzallamas,arañazo]
psyduck = newPokemon "Psyduck" 50 agua [chorroDeAgua,restaurar]

-------------------------------------------------------------------------------------------
---------------------------------------- Ataques
newDañino tipo daño = AtaqueDañino tipo daño
newCurativo tipo vida = AtaqueCurativo tipo vida

lanzallamas = newDañino fuego 40
arañazo = newDañino normal 20
impactrueno = newDañino electrico 50
colaDeAcero = newDañino acero 70
chorroDeAgua = newDañino agua 40
restaurar = newCurativo psiquico 30

-------------------------------------------------------------------------------------------
---------------------------------------- Tipos
newTipo nombre ventajaContra = Tipo nombre ventajaContra

fuego = newTipo "Fuego" [planta,acero]
agua = newTipo "Agua" [fuego,piedra]
planta = newTipo "Planta" [piedra,agua]
acero = newTipo "Acero" [piedra]
piedra = newTipo "Piedra" [fuego, electrico]
electrico = newTipo "Electrico" [agua]
psiquico = newTipo "Psiquico" [psiquico]
normal = newTipo "Normal" []


