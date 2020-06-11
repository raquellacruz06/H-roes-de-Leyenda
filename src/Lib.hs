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
                      artefactos :: [Artefacto],
                      tareasRealizadas :: [Tarea]
                      } deriving Show --No utilizamos eq porque tiene una lista de funciones y las funciones no son comparables

--Heroe de ejemplo
pancho :: Heroe
pancho = UnHeroe "el mejor" 50 [("espadita", 300)] [] 

--data Artefacto = {nombreArtefacto :: String, rareza :: Int} deriving (Show, Eq) opcion con datas

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

type Tarea = Heroe -> Heroe
encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto artefacto  = modificarReconocimiento (+ rareza artefacto ) . modificarArtefactos (++[artefacto])

{-Escalar el Olimpo: esta ardua tarea recompensa a quien la realice otorgándole 500 unidades de reconocimiento
 y triplica la rareza de todos sus artefactos, pero desecha todos aquellos que luego de triplicar su rareza no 
 tengan un mínimo de 1000 unidades.
 Además, obtiene "El relámpago de Zeus" (un artefacto de 500 unidades de rareza).-}

escalarOlimpo :: Tarea
escalarOlimpo  = modificarReconocimiento (+500) . modificarArtefactos desechar. modificarArtefactos (++[("El relampago de zeus", 500)])

desechar :: [Artefacto]-> [Artefacto]
desechar  = filter ((>= 1000).rareza) .rarezasTriplicadas 

rarezasTriplicadas :: [Artefacto] -> [Artefacto]
rarezasTriplicadas listaArtefactos = map triplicarRarezas listaArtefactos

triplicarRarezas (nombre, rareza)= (nombre, rareza *3)

{-Ayudar a cruzar la calle: incluso en la antigua Grecia los adultos mayores necesitan ayuda para ello. 
Los héroes que realicen esta tarea obtiene el epíteto "Groso", donde la última 'o' se repite tantas 
veces como cuadras haya ayudado a cruzar. Por ejemplo, 
ayudar a cruzar una cuadra es simplemente "Groso", pero ayudar a cruzar 5 cuadras es "Grosooooo".-}

ayudaCruzarLaCalle :: Int -> Tarea
ayudaCruzarLaCalle cantCuadras  = modificarEpiteto (grosoSegun cantCuadras) 

grosoSegun :: Int -> String -> String
grosoSegun cantCuadras _ = "groso"++ replicate (cantCuadras +(-1))'o'

{-Matar una bestia: Cada bestia tiene una debilidad (por ejemplo: que el héroe tenga cierto artefacto, 
o que su reconocimiento sea al menos de tanto). Si el héroe puede aprovechar esta debilidad, entonces obtiene 
el epíteto de "El asesino de <la bestia>". Caso contrario, huye despavorido, 
perdiendo su primer artefacto. Además, tal cobardía es recompensada con el epíteto  "El cobarde".-}

data Bestia = UnaBestia {debilidadASuperar :: (Heroe -> Bool)} deriving Show

{-efectoSiLaSuperan :: (Heroe-> Heroe),
efectoSiNoLaSuperan :: (Heroe-> Heroe) Info del data bestia que podría servir luego, por ahora no la necesitamos-}

{-matarBestia :: (Heroe -> Bool)-> Tarea
matarBestia debilidadASuperar heroe
    | debilidadASuperar heroe = modificarEpiteto (\x-> "EL asesino de la bestia") heroe --Esta primera versión es practicamente igual
    | otherwise = (pierdePrimerArtefacto . modificarEpiteto (\x-> "EL cobarde")) heroe-}

    

---Reescribimos lo que ya teníamos usando el data:
matarBestia :: Bestia -> Heroe -> Heroe
matarBestia bestia heroe 
    | (debilidadASuperar bestia) heroe = modificarEpiteto (++ " EL asesino de la bestia") heroe
    | otherwise = (pierdePrimerArtefacto . modificarEpiteto (\x-> "EL cobarde")) heroe
    where pierdePrimerArtefacto = modificarArtefactos tail
    
{-Modelar a Heracles, cuyo epíteto es "Guardián del Olimpo" y tiene un reconocimiento de 700. 
Lleva una pistola de 1000 unidades de rareza (es un fierro en la antigua Grecia, obviamente que es raro)
 y el relámpago de Zeus. Este Heracles 
es el Heracles antes de realizar sus doce tareas, hasta ahora sabemos que solo hizo una tarea...-}

heracles :: Heroe
heracles = UnHeroe "Guardian del olimpo, El asesino de la bestia" 700 [("pistola", 1000), ("El relampago de Zeus", 500)] [matarAlLeonDeNemea (leonDeNemea)]
heracles2 :: Heroe
heracles2 = UnHeroe "Guardian del olimpoooooooo" 700 [("pistola", 1000), ("El relampago de Zeus", 500)] []
{-Modelar la tarea "matar al león de Nemea", que es una bestia cuya debilidad es que el epíteto del héroe sea de 20 caracteres o más. 
Esta es la tarea que realizó Heracles.-}

matarAlLeonDeNemea :: Bestia->Tarea
matarAlLeonDeNemea leonDeNemea heroe = matarBestia leonDeNemea heroe

leonDeNemea :: Bestia
leonDeNemea = UnaBestia {debilidadASuperar = (>20).length.epiteto}

--Hacer que un héroe haga una tarea. Esto 
--nos devuelve un nuevo héroe con todos los cambios que conlleva realizar una tarea.

realizarTarea :: Heroe -> Tarea -> Heroe
realizarTarea heroe tarea = tarea heroe

{--Hacer que dos héroes presuman sus logros ante el otro. Como resultado, queremos conocer la tupla que tiene en primer lugar 
al ganador de la contienda, y en segundo al perdedor. Cuando dos héroes presumen, comparan de la siguiente manera:
Si un héroe tiene más reconocimiento que el otro, entonces es el ganador.
Si tienen el mismo reconocimiento, pero la sumatoria de las rarezas de los artefactos de un héroe es mayor al otro,
 entonces es el ganador.
Caso contrario, ambos realizan todas las tareas del otro, y vuelven a hacer la comparación desde el principio. 
Llegado a este punto, el intercambio se hace tantas veces sea necesario hasta que haya un ganador.-}


------- No llegamos a nada porque la funcion elMejorEn me manda siempre a ObtenerElDeMayorRareza
{-obtenerMejorHeroe :: Heroe -> Heroe ->()-> (Heroe, Heroe)
obtenerMejorHeroe heroe1 heroe2 = elMejorEn reconocimiento heroe1 heroe2 obtenerElDeMayorRareza

obtenerElDeMayorRareza :: Heroe -> Heroe -> ()-> (Heroe, Heroe)
obtenerElDeMayorRareza heroe1 heroe2 = elMejorEn (sum. map rareza.artefactos) heroe1 heroe2

mayorRareza = (sum. map rareza.artefactos)

elMejorEn :: Ord a => (Heroe-> a) -> Heroe -> Heroe -> ()->(Heroe, Heroe)
elMejorEn funcion heroe1 heroe2 funcionAlternativa
    |funcion  heroe1 > funcion heroe2 = (heroe1, heroe2)
    |funcion heroe2 > funcion heroe1 = (heroe2, heroe2)
    |otherwise = funcionAlternativa heroe1 heroe2-}

------Primera opcion con repeticion de lógica
{-obtenerMejorHeroe :: Heroe -> Heroe -> (Heroe, Heroe)
obtenerMejorHeroe heroe1 heroe2 
    | reconocimiento heroe1 > reconocimiento heroe2 = (heroe1, heroe2)
    | reconocimiento heroe2 > reconocimiento heroe1 = (heroe2, heroe2)
    | otherwise = obtenerElDeMayorRareza heroe1 heroe2

obtenerElDeMayorRareza :: Heroe -> Heroe -> (Heroe, Heroe)
obtenerElDeMayorRareza heroe1 heroe2
    | rarezasTotales heroe1 > rarezasTotales heroe2 = (heroe1, heroe2)
    | rarezasTotales heroe2 > rarezasTotales heroe1 = (heroe1, heroe2)
    |otherwise = obtenerMejorHeroe (labor heroe1 (tareasRealizadas heroe2)) (labor heroe2 (tareasRealizadas heroe1) )
--(foldl realizarTarea heroe1 (tareasRealizadas heroe2)) (foldl realizarTarea heroe2 (tareasRealizadas heroe2)-}

----Opcion 3 y pudimos reciclar algunas de las cosas de arriba

type ResultadoFinal = (Heroe, Heroe)

presumir :: Heroe -> Heroe -> ResultadoFinal
presumir heroe1 heroe2 
    | reconocimiento heroe1 /= reconocimiento heroe2 = elMejorEn reconocimiento heroe1 heroe2
    | otherwise = obtenerElDeMayorRareza heroe1 heroe2

elMejorEn :: Ord a => (Heroe-> a) -> Heroe -> Heroe -> ResultadoFinal
elMejorEn funcion heroe1 heroe2 
    |funcion  heroe1 > funcion heroe2 = (heroe1, heroe2)
    |otherwise = (heroe2, heroe1) --Podemos evaluar el otherwise porque la igualdad ya se evaluó en la primera entrada

obtenerElDeMayorRareza :: Heroe -> Heroe -> ResultadoFinal
obtenerElDeMayorRareza heroe1 heroe2
    |rarezasTotales heroe1 /= rarezasTotales heroe2 = elMejorEn rarezasTotales heroe1 heroe2
    |otherwise = presumir (labor heroe1 (tareasRealizadas heroe2)) (labor heroe2 (tareasRealizadas heroe1))

rarezasTotales :: Heroe -> Int
rarezasTotales heroe = (sum. map rareza.artefactos) heroe

--Hacer que un héroe realice una labor, obteniendo como resultado el héroe tras haber realizado todas las tareas.

labor :: Heroe-> [Tarea] -> Heroe
labor heroe tareas = foldl realizarTarea heroe tareas

--Cuál es el resultado de hacer que presuman dos héroes con reconocimiento 100, 
--ningún artefacto y ninguna tarea realizada?
{-No funcionaría ya que no habría ganador por reconocimiento ni por artefacto y al intentar que un heroe realice las tareas del otro
se encontrará con una lista vacía, lo cuál arroja error..-}  
{-Si invocamos la función anterior con una labor infinita, ¿se podrá conocer el estado final del héroe? ¿Por qué?
No, porque antes de arrojar un resultado necesita recorrer toda la lista de tareas lo cual nunca va a terminar.
Esto se debe a eager evaluation. Se pudiera resolver si pidieramos las primeras n tareas, o la primera en cumplir alguna condicion (Este ultimo sólo
funcionaría si alguna tarea cumple la condición-}




