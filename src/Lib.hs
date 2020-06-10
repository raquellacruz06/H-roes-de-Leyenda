module Lib where
import Text.Show.Functions

laVerdad = True
{-Modelar a los héroes. Tip: lean todo el enunciado!
Hacer que un héroe pase a la historia. Esto varía según el índice de reconocimiento que tenga el héroe a la hora de 
su muerte:
Si su reconocimiento es mayor a 1000, su epíteto pasa a ser "El mítico", y no obtiene ningún artefacto. 
¿Qué artefacto podría desear tal espécimen?
Si tiene un reconocimiento de al menos 500, su epíteto pasa a ser "El magnífico" y añade a sus artefactos
 la lanza del Olimpo (100 de rareza). 
Si tiene menos de 500, pero más de 100, su epíteto pasa a ser "Hoplita" y añade a sus artefactos una Xiphos 
(50 de rareza).
En cualquier otro caso, no pasa a la historia, es decir, no gana ningún epíteto o artefacto.
-}
--1) Modelar Héroes

type Nombre = String
type Rareza = Int
type Artefacto = (Nombre, Rareza)

rareza (_, rareza)= rareza
nombre (nombre, _)= nombre --Primer intento con Tuplas

data Heroe = UnHeroe {epiteto :: String,
                      reconocimiento :: Int,
                      tareasRealizadas :: [Tarea],
                      artefactos :: [Artefacto]}

--data Artefacto = {nombreArtefacto :: String, rareza :: Int} deriving (Show, Eq) opcion con datas

type Tarea = String--Por ahora
pasarALaHsitoria :: Heroe -> Heroe
pasarALaHsitoria heroe 
    | reconocimiento heroe > 1000 = modificarEpiteto (\x-> "EL Mitico" ) heroe --heroe {epiteto = "El mítico"}
    | reconocimiento heroe >= 500 = (modificarEpiteto (\x-> "EL Magnifico" ) . modificarArtefactos (++[("lanza del olimpo", 100)])) heroe -- {epiteto= "El magnifico", artefactos = [("lanza del olimpo", 100)]}
    | reconocimiento heroe > 100 = (modificarEpiteto (\x-> "EL Hoplita" ). modificarArtefactos (++[("Xiphos", 50)])) heroe --{epiteto = "Hoplita", artefactos = [("Xiphos", 50)]} -- No consideramos menor a 500 ya que lo evaluamos en la guarda de arriba
    | otherwise = heroe

--Mejoramos repeticion de logica
modificarEpiteto :: (String-> String) -> Heroe -> Heroe
modificarEpiteto funcion heroe = heroe {epiteto = (funcion.epiteto) heroe}

modificarArtefactos :: ([Artefacto]-> [Artefacto]) -> Heroe -> Heroe --Opcion con tuplas
modificarArtefactos funcion heroe = heroe {artefactos = (funcion.artefactos) heroe} --opcion con tuplas
--modificarArtefactos :: Artefacto -> Heroe -> Heroe
--modificarArtefactos artefacto heroe = 


modificarReconocimiento :: (Int-> Int) -> Heroe -> Heroe
modificarReconocimiento funcion heroe = heroe {reconocimiento = (funcion.reconocimiento) heroe}

---Tareas
{-Encontrar un artefacto: el héroe gana tanto reconocimiento como rareza del artefacto, 
además de guardarlo entre los que lleva.-}

encontrarArtefacto :: Artefacto -> Heroe -> Heroe
encontrarArtefacto artefacto  = modificarReconocimiento (+ rareza artefacto ) . modificarArtefactos (++[artefacto])

{-Escalar el Olimpo: esta ardua tarea recompensa a quien la realice otorgándole 500 unidades de reconocimiento
 y triplica la rareza de todos sus artefactos, pero desecha todos aquellos que luego de triplicar su rareza no 
 tengan un mínimo de 1000 unidades.
 Además, obtiene "El relámpago de Zeus" (un artefacto de 500 unidades de rareza).-}

escalarOlimpo :: Heroe -> Heroe
escalarOlimpo  = modificarReconocimiento (+500) . modificarArtefactos desechar. modificarArtefactos (++[("lanza del olimpo", 100)])

desechar :: [Artefacto]-> [Artefacto]
desechar  = filter ((>= 1000).rareza) .rarezasTriplicadas 

rarezasTriplicadas :: [Artefacto] -> [Artefacto]
rarezasTriplicadas listaArtefactos = map triplicarRarezas listaArtefactos

triplicarRarezas (nombre, rareza)= (nombre, rareza *3)

{-Ayudar a cruzar la calle: incluso en la antigua Grecia los adultos mayores necesitan ayuda para ello. 
Los héroes que realicen esta tarea obtiene el epíteto "Groso", donde la última 'o' se repite tantas 
veces como cuadras haya ayudado a cruzar. Por ejemplo, 
ayudar a cruzar una cuadra es simplemente "Groso", pero ayudar a cruzar 5 cuadras es "Grosooooo".-}

{-ayudaCruzarLaCalle :: Int -> Heroe -> Heroe
ayudaCruzarLaCalle cantCuadras heroe = modificarEpiteto ()

grosoSegun :: Int -> String -> String
grosoSegun cantCuadras _ = "groso"++-}



