Para incluir en la matriz ambiental los datos de las variables provenientes de la página 
https://giovanni.gsfc.nasa.gov se deben considera las siguientes especificaciones:


1) Se debe realizar la solicitud de la variable colocando exactamente la zona comprendida 
entre las latitudes 8 y 17 grados norte y las longitudes 60 y 73 grados oeste. (-73,8,-60,17)

2) Los graficos aceptados para procesamiento son los siguientes: 
- "Time Averaged Map"
- "Animation"
- "Monthly and Seasonal Averages"

3) Para graficos estilo "Time Averaged Map" o "Monthly and Seasonal Averages":

Por cada variable solicidada se debe descargar la imagen generada en formato PNG y 
copiarla dentro de la carpeta "~/Thesis Project AB/Data/Raw Data/Giovanni". Es importante 
desactivar las casillas de "Supporting Overlays" y activar las de "Decorations" dentro de 
de la pestaña "Layers" de la página antes de realizar la descarga. Dejar el resto de las opciones 
predefinidas.

Para graficos estilo "Animation":

Descargar el archivo ZIP, descomprimirlo y copiar la carpeta con las imagenes PNG dentro de
"~/Thesis Project AB/Data/Raw Data/Giovanni".Dejar las opciones del mapa establecidas por defecto.


3) Para graficos estilo "Time Averaged Map" o "Monthly and Seasonal Averages" Se deben reescribir
los nombres de los archivo PNG y darles el siguiente formato:

NombreDeLaVariable_unidadDeMedicion_RangoDeFechas.png

Para los archivos estilo "Animation" basta reescribir el nombre de la carpeta que los contiene en
el formato solicitado. Las imagenes en su interior copiaran el nombre de la carpeta donde estan.

Observacion: Cada una de las palabras entre los "_" debe contener solamente números y letras.

Para el rango de fechas usar:
- El formato DDMMYYYY si es una sola.
- El formato DDMMYYYYDDMMYYY si es un rango.

Ejemplo:

Para "Time Averaged Map" o "Monthly and Seasonal Averages":
OxigenoDisuelto_MicromolesPorLitro_0101199504042017.png  

Para "Animation":
OxigenoDisuelto_MicromolesPorLitro_0101199504042017	(Es un directorio.)	

Para procesar las imagenes de Giovanni se debe correr el codigo "ejecutable.R" cada vez que se 
incluyan mapas nuevos a la carpeta "~/Thesis Project AB/Raw Data/Giovanni" y se deseen agregar a la 
matriz ambiental. El programa procesara solo aquellas imagenes que no manipulo previamente. 

La matriz ambiental (archivo MatrizAmbientalGIO.csv) es construida y editada en el directorio
"~/Thesis Project AB/Data/Pre-processed Data 2".

Finalizada la inclusion de variables, se corregiran los valores faltantes que se generan producto
de la falta de coincidencia exacta entre los puntos de geolocalizacion de las tablas involucradas.
Se hace un barrido del mapa usando el algoritmo KNN.
