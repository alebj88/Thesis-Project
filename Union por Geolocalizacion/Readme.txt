Este programa permite agregarle a los datos colocados en la carpeta "Biologicas" la informacion
almacenada en las tablas copiadas dentro de la carpeta "Ambientales" mediante union por 
geolocalizacion

Este procedimiento no es mas que una version mejorada del "merge" que busca asignarle informacion
climatologica a los puntos de geolocalizacion contenidos en las tablas biologicas mediante la
busqueda de valores ambientales a diferentes radios de distancia.

Tanto las tablas biologicas como las ambientales deben tener las variables numericas "latitude" 
"longitude" para poder establecer la union. La misma se realiza mediante 4 radios de busqueda 
establecidos de forma creciente por prioridad (en cuanto los datos son conseguidos se suspende
la busqueda a radios de mayor distancia. En caso de no encontrarse alguno se asigna NA). Una vez
localizados los valores en los puntos vecinos se toma su valor promedio o su mediana en funcion 
de la opcion ingresada por el usuario.

Si la tabla ambiental se llama "GoogleEarthRiosLugares.csv", el programa inicia la secuencia
para calculo de distancia a objetos costeros. SOlicita la distancia maxima de busqueda y pregunta 
si de forma adicional se desea generar la tabla objetos a distancia minima.

