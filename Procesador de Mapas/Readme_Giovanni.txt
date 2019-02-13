Procesamiento de mapas satelitales.



El programa "Procesador de Mapas.R" ofrece la posibilidad de convertir imágenes satelitales 
provenientes de la pagina https://giovanni.gsfc.nasa.gov, en archivos CSV donde se muestra el 
valor de la variable medida por punto de geolocalización (latitud-longitud-valor).

Los mismos, luego de ser descargados de la página, deben ser almacenados en la carpeta "Mapas" 
del programa. A diferencia de los archivos PNG provenientes de una descarga estilo "Time Averaged 
Map" o "Monthly and Seasonal Averages", los que vienen de "Animation" deben ser almacenados en
"Mapas" dentro de su respectiva carpeta descomprimida. Estos ultimos se descargan de la pagina 
como archivos ZIP.

Los mapas solo serán procesados una vez, lo que implica que pueden ser dejados en la carpeta "Mapas"
sin problemas. El programa procesará sólo aquellas imágenes que no manipuló previamente. 


#ENTENDIENDO EL PROCEDIMIENTO---------------------------------------------------------------------



Los mapas "Time Averaged Map" y "Monthly and Seasonal Averages" son procesados de forma 
individual y son el tipo más sencillo de procesamiento. Cada punto de geolocalización recibe 
su medida, se registra en un CSV y se guarda en la carpeta "Data Preprocesada". Los "Animation" son 
los mapas que vienen por lote, se descargan de la pagina como archivos ZIP y contienen las series 
temporales de mapas satelitales pertenecientes a la variable en cuestion. Estos deben ser descomprimidos
antes de ser procesados. Al correr el programa se creara de forma automatica un directorio en la carpeta 
"Data Preprocesada" y los CSV asociados a cada mapa de la serie temporal seran almacenados ahi.

Una vez que todos los mapas y carpetas almacenadas dentro del directorio "Mapas" hayan sido procesados,
el programa iniciara la construccion de la matriz ambiental. Los CSV provenientes de "Animation" seran 
unidos en una sola tabla mediante el resumen estadistico, cada punto de geolocalización recibirá el valor 
promedio de la variable, la  desviación, máximo, mínimo, mediana, cuartiles, sesgo y curtosis. Posteriormente
la misma sera unida via MERGE con el resto de los archivos de la carpeta "Data Preprocesada" y la tabla 
resultante sera guardada en la carpeta "Datos Finales" con el nombre "MatrizAmbientalGIO.csv".


Si al ejecutar el "Procesador de Mapas.R" la tabla "MatrizAmbientalGIO.csv" esta presente en la carpeta 
"Datos Finales", la tabla generada sera automaticamente anexada via merge a ella por tanto se recomienda 
una limpieza general de las carpetas cada vez que se inicie un procesamiento de datos nuevo. 

OBSERVACION: La carpeta "Ctrl" que esta por defecto dentro de "Mapas" no debe ser removida. El programa la 
usa para almacenar graficos de evaluacion del procesamiento de las imagenes. Su contenido puede ser vaciado
pero la carpeta no debe ser eliminada.

OBSERVACION: El archivo "GiovanniCleaner.csv" presente en la carpeta "Data Preprocesada" tampoco debe ser 
borrado. El programa lo utiliza para eliminar datos incorrectos cuando se utiliza la region de procesamiento
por defecto.


#CAMBIANDO ZONA DE ESTUDIO------------------------------------------------------------------



Por defecto la zona de estudio está limitada a la región comprendida entre las latitudes 8 y
 17 grados norte y las longitudes 60 y 73 grados oeste. (-73,8,-60,17) pero esto puede ser
modificado si se desean procesar mapas de otras localidades alrededor del globo lejanas a los polos. 

En el archivo "Procesador de Mapas.R", en la parte superior del código se encuentra el "PANEL PARA 
PROCESAR IMAGENES DE GIOVANNI" donde aparecen dos variables, "alternativo" y "Margen_Map".

Colocando "alternativo" en FALSE, el programa utilizará la zona (-73,8,-60,17) establecida por
defecto y no requerirá información adicional. Pero si se desea usar otra region se debe colocar 
"alternativo" en TRUE e ingresar en "Margen_Map" los nuevos límites de la zona de estudio.
Sólo cuando "alternativo" recibe el valor TRUE, el margen (longitud izquierda ,latitud inferior,
longitud derecha , latitud superior) ingresado en "Margen_Map" como vector, se considerará. 

Es IMPORTANTE resaltar que la dimensión de los mapas ingresados debe tener un rango exacto de
13 grados para las longitudes y 9 grados para las latitudes. Imagenes con mayor o menor zoom
no serán procesadas.

OBSERVACION: No se recomienda procesar datos de distintas regiones de forma conjunta ya que el programa
siempre buscara unir los datos en una sola matriz ambiental via merge . Es preferible cambiarle el 
nombre a la matriz ambiental generada o removerla cada vez que se deseen usar datos de otras regiones.




#ESPECIFICACIONES DE LA DESCARGA------------------------------------------------------------------


Para generar una matriz ambiental con datos provenientes de la página  https://giovanni.gsfc.nasa.gov 
se deben considerar las siguientes especificaciones:


1) Se debe realizar la solicitud de la variable colocando exactamente la zona comprendida 
entre las latitudes 8 y 17 grados norte y las longitudes 60 y 73 grados oeste. (-73,8,-60,17)

2) Si se desea usar otra región, colocar la variable "alternativo" del panel de procesamiento
alternativo del programa "Procesador de Mapas.R", en TRUE. Establecer el nuevo centro del mapa 
en "mapCenter" como un vector c(latitud, longitud) considerando que el mismo debe tener una 
dimensión de 13 grados exactos de longitud y 9 de latitud, y ejecutarlo en modo "Source".

4) Los gráficos aceptados para procesamiento son los siguientes: 
- "Time Averaged Map"
- "Animation"
- "Monthly and Seasonal Averages"

4.1) Para gráficos estilo "Time Averaged Map" o "Monthly and Seasonal Averages":

Por cada variable solicidada se debe descargar la imagen generada en formato PNG y copiarla dentro
de la carpeta "Mapas". Es importante desactivar las casillas de "Supporting Overlays" y activar 
las de "Decorations" dentro de la pestaña "Layers" de la página antes de realizar la descarga.
Dejar el resto de las opciones predefinidas.

El nombre de la variable medida en el CSV será tomada del nombre del mapa (archivo png).Se recomienda 
asignarle al mapa un nombre corto que represente bien la variable que mide.

Ejemplo:

salinidad.png  

4.2)  Para gráficos estilo "Animation":

Por cada variable solicidada se debe descargar el archivo ZIP que genera la página y copiar su carpeta 
descomprimida en el directorio "Mapas". Las opciones de los mapas deben ser dejadas por defecto antes 
de su descarga. La eliminación de los datos manchados con las lineas del mapa serán removidas por el 
procesador asignandoles NA's por medio del uso de una plantilla. Es importante resaltar que esta limpieza 
se realizá sólo cuando se usa la región por defecto (-73,8,-60,17), para regiones alternativas considere
que hay datos incorrectos producto de las lineas de frontera de cada mapa particular.

Descargar el archivo ZIP, descomprimirlo y copiar la carpeta con las imágenes PNG dentro de "Mapas".
Dejar las opciones del mapa establecidas por defecto.

El nombre la variable medida en el CSV será tomada del nombre de la carpeta que contiene el conjunto de
imágenes "Animation". Se recomienda asignarle a la carpeta un nombre corto que represente bien la variable 
que miden sus imagenes.


Ejemplo:

OxigenoDisuelto    		(Es un directorio)	





