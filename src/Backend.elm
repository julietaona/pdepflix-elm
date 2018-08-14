module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- Finalizado ok!
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--

peliculaTienePalabrasClave: String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = List.all (peliculaTienePalabraClave pelicula) (String.words palabras)

peliculaTienePalabraClave: Movie -> String -> Bool
peliculaTienePalabraClave pelicula palabra = String.contains (String.toUpper palabra) (String.toUpper pelicula.title)

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- Finalizado ok! ver donde tipea los generos en el select
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (esPeliculaDelGenero genero)

esPeliculaDelGenero : String -> Movie -> Bool
esPeliculaDelGenero genero pelicula = List.any ((==) genero) pelicula.genre

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- Finalizado ok!
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = List.filter (esPeliculaParaMenores mostrarSoloMenores)

esPeliculaParaMenores : Bool -> Movie -> Bool
esPeliculaParaMenores mostrarSoloMenores pelicula = ((mostrarSoloMenores == pelicula.forKids) && mostrarSoloMenores) || not mostrarSoloMenores 

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- Finalizado ok! de menor a mayor
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.sortBy .rating

-- **************
-- Requerimiento: dar like a una película
-- Finalizado ok!
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id peliculas = List.map (sumarUnLike id) peliculas

tieneId id pelicula = pelicula.id == id

sumarUnLike id pelicula = if pelicula.id == id then
      { pelicula | likes = pelicula.likes + 1}
      else
        { pelicula | likes = pelicula.likes}

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

type alias PosiblesGustos = (String, List String)
otrosPosiblesGustos = ("Terror", ["Suspenso"])

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map (calcPorcentajes preferencias)

calcPorcentajes : Preferences -> Movie -> Movie
calcPorcentajes preferencias pelicula = (mayorA100 << (porcPalabras preferencias) << (porcActor preferencias) << (porcGenero preferencias) << porcCero) pelicula 

porcCero : Movie -> Movie
porcCero pelicula = {pelicula | matchPercentage = 0}

porcPalabras : Preferences -> Movie -> Movie 
porcPalabras preferencias pelicula = if (peliculaTienePalabrasClave preferencias.keywords pelicula) then
                                        {pelicula | matchPercentage = pelicula.matchPercentage + 20 * (List.length (String.words preferencias.keywords))}
                                     else
                                        {pelicula | matchPercentage = pelicula.matchPercentage}

porcActor : Preferences -> Movie -> Movie
porcActor preferencias pelicula = if (actuaEnLaPelicula preferencias.favoriteActor pelicula) then
                                   {pelicula | matchPercentage = pelicula.matchPercentage + 50}
                                  else
                                  {pelicula | matchPercentage = pelicula.matchPercentage}

porcGenero : Preferences -> Movie -> Movie
porcGenero preferencias pelicula = if (esPeliculaDelGenero preferencias.genre pelicula) then
                                      {pelicula | matchPercentage = pelicula.matchPercentage + 60}
                                   else
                                      {pelicula | matchPercentage = pelicula.matchPercentage}


porcGeneroAlternativo : Preferences -> Movie -> PosiblesGustos -> Movie 
porcGeneroAlternativo preferencias pelicula otrosPosiblesGustos = if (conGeneroAlternativo preferencias.genre pelicula otrosPosiblesGustos) then
                                                                    {pelicula | matchPercentage = pelicula.matchPercentage + 15}
                                                                  else
                                                                    {pelicula | matchPercentage = pelicula.matchPercentage}

conGeneroAlternativo : String -> Movie -> PosiblesGustos -> Bool
conGeneroAlternativo gen pelicula otrosPosiblesGustos = (gen == Tuple.first otrosPosiblesGustos) && (List.length (List.filter (esPeliculaDelGenero2 pelicula) (Tuple.second otrosPosiblesGustos)) > 0)

esPeliculaDelGenero2: Movie -> String -> Bool
esPeliculaDelGenero2 pelicula genero = esPeliculaDelGenero genero pelicula 

actuaEnLaPelicula: String-> Movie -> Bool
actuaEnLaPelicula actor pelicula = List.any ((==) actor) pelicula.actors

mayorA100 : Movie -> Movie
mayorA100 pelicula = if pelicula.matchPercentage > 100 then
                        {pelicula | matchPercentage = 100}
                        else
                         {pelicula | matchPercentage = pelicula.matchPercentage}