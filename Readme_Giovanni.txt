Procesamiento de mapas satelitales.

El programa Proyecto.R ofrece la posibilidad de convertir imagenes satelitales provenientes
de la pagina https://giovanni.gsfc.nasa.gov, en archivos CSV donde se muestre el valor de la
variable medida por punto de geolocalizacion (latitud-longitud-valor).

Los mismos deben ser descargados de la pagina como "Time Averaged Map" "Monthly and Seasonal Averages"
o "Animation", y ser almacenados en la carpeta "~/Thesis Project AB/Data/Raw Data/Giovanni". 
Las especificaciones de descarga las indicaremos mas abajo en este texto.

Los mapas "Time Averaged Map" y "Monthly and Seasonal Averages" son procesados de forma 
individual y son el tipo mas sencillo de procesamiento. Cada punto de geolocalizacion recibe 
su medida y eso se registra en un CSV. Los "Animation" son los mapas que vienen por lote y el
programa los procesara de esa forma. Cada punto de geolocalizacion recibira el valor promedio 
de la variable, la desviacion, maximo, minimo, mediana, sesgo y curtosis, y toda esa informacion
sera almacenada en un solo csv.

Por defecto la zona de estudio esta limitada a la region comprendida entre las latitudes 8 y
 17 grados norte y las longitudes 60 y 73 grados oeste. (-73,8,-60,17) pero esto puede ser
modificado si se desean estudiar datos de otras localidades. 

En el archivo Proyecto .R, en la parte superior del codigo se encuentra el "PANEL PARA PROCESAR 
IMAGENES DE GIOVANNI" donde aparecen dos variables, "alternativo" y "Margen_Map".

Colocando "alternativo" en FALSE, el programa utilizara la zona (-73,8,-60,17) establecida por
defecto y no requerira informacion adicional. Pero si se desea usar otra region se debe 
colocar "alternativo" en TRUE e ingresar en "Margen_Map" los nuevos limites de la zona de estudio.
Solo cuando "alternativo" recibe el valor TRUE, el margen (longitud izquierda ,latitud inferior,
longitud derecha , latitud superior) ingresado en "Margen_Map" como vector se considera. 

Es IMPORTANTE resaltar que la dimension de los mapas ingresados debe tener un rango exacto de
13 grados para las longitudes y 9 grados para las latitudes. Imagenes con mayor o menor zoom
no seran procesadas.

Una vez finalizado el procesamiento de todos los mapas almacenados en la carpeta 
"~/Thesis Project AB/Data/Raw Data/Giovanni" los archivos CSV generados sean juntados en una
sola tabla llamada "MatrizAmbientalGIO.csv" y en caso de que esta ya exista las tablas nuevas 
seran simplemente anexadas a ella. Por este motivo no se recomienda procesar datos de distintas
regiones de forma conjunta. Es preferible cambiarle el nombre a la matriz ambiental generada
cada vez que se deseen usar datos de otras regiones del mapa.

Los mapas solos seran procesados una vez lo que implica que pueden ser dejados en la carpeta
de procesamiento del programa sin problemas.


Para incluir en la matriz ambiental los datos de las variables provenientes de la página 
https://giovanni.gsfc.nasa.gov se deben considera las siguientes especificaciones:


1) Se debe realizar la solicitud de la variable colocando exactamente la zona comprendida 
entre las latitudes 8 y 17 grados norte y las longitudes 60 y 73 grados oeste. (-73,8,-60,17)

2) Si se desea usar otra region colocar la variable "alternativo" del panel de procesamiento
alternativo del programa Proyecto.R, en TRUE. Establecer el nuevo centro del mapa en "mapCenter" 
como un vector c(latitud, longitud) considerando que el mismo debe tener una dimension de 13 
grados exactos de longitud y 9 de latitud, y ejecutarlo en modo "Source".

4) Los graficos aceptados para procesamiento son los siguientes: 
- "Time Averaged Map"
- "Animation"
- "Monthly and Seasonal Averages"

4.1) Para graficos estilo "Time Averaged Map" o "Monthly and Seasonal Averages":

Por cada variable solicidada se debe descargar la imagen generada en formato PNG y 
copiarla dentro de la carpeta "~/Thesis Project AB/Data/Raw Data/Giovanni". Es importante 
desactivar las casillas de "Supporting Overlays" y activar las de "Decorations" dentro de 
de la pestaña "Layers" de la página antes de realizar la descarga. Dejar el resto de las opciones 
predefinidas.

Para graficos estilo "Animation":

Descargar el archivo ZIP, descomprimirlo y copiar la carpeta con las imagenes PNG dentro de
"~/Thesis Project AB/Data/Raw Data/Giovanni".Dejar las opciones del mapa establecidas por defecto.

El nombre de la variable medida en el CSV sera tomada del nombre del mapa. Se recomienda asignarle 
al mapa un nombre corto que represente bien la variable que mide.  EL NOMBRE NO DEBE TENER PUNTOS,
haga las separaciones entre variables con "_".

Ejemplo:

salinidad_max.png  

4.2)  Para graficos estilo "Animation":

Por cada variable solicidada se debe descargar el archivo ZIP que genera la pagina y copiar la
su carpeta descomprimida en el directorio "~/Thesis Project AB/Data/Raw Data/Giovanni". Las opciones
de los mapas deben ser dejadas por defecto antes de su descarga. La eliminacion de los datos manchados
con las lineas del mapa seran removidas por el procesador asignandoles NA's por medio del uso de una
plantilla. Es importante resaltar que esta limpieza se realiza solo cuando se usa la region por defecto
(-73,8,-60,17), para regiones alternativas considere que hay datos incorrectos producto de las lineas
de frontera de cada mapa particular.

El nombre la variable medida en el CSV sera tomada del nombre de la carpeta que contiene el conjunto de
imagenes "Animation". Se recomienda asignarle a la carpeta un nombre corto que represente bien la variable 
que miden sus imagenes. EL NOMBRE NO DEBE TENER PUNTOS, haga las separaciones entre variables con "_".


Ejemplo:

OxigenoDisuelto_Min    		(Es un directorio)	


Para ejecutar el procesamiento de las imagenes de Giovanni se debe correr el codigo "Proyecto.R" 
cada vez que se incluyan mapas nuevos a la carpeta "~/Thesis Project AB/Raw Data/Giovanni" y se deseen
agregar a la matriz ambiental (si se usan regiones diferentes en cada caso renombrar la el archivo 
"MatrizAmbientalGIO.csv" en cada uno). 

Observacion: El programa procesara solo aquellas imagenes que no manipulo previamente. 

La matriz ambiental (archivo MatrizAmbientalGIO.csv) es construida y editada en el directorio
"~/Thesis Project AB/Data/Pre-processed Data 2".

