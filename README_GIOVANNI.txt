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
copiarla dentro de la carpeta "~/Thesis Project/Raw Data/Giovanni". Es importante 
desactivar las casillas de "Supporting Overlays" y activar las de "Decorations" dentro de 
de la pestaña "Layers" de la página antes de realizar la descarga. Dejar el resto de las opciones 
predefinidas.

Para graficos estilo "Animation":

Descargar el archivo ZIP, descomprimirlo y copiar las imagenes PNG dentro de la carpeta 
"~/Thesis Project/Raw Data/Giovanni".Dejar las opciones del mapa establecidas por defecto.


3) Se debe reescribir el nombre del archivo PNG y darle el siguiente formato:

TipoDeGrafico_NombreDeLaVariable_unidadDeMedicion_RangoDeFechas.png

Observacion: Cada una de las palabras entre los "_" debe contener solamente números y letras.

Para el rango de fechas usar:
- El formato DDMMYYYY si es una sola.
- El formato DDMMYYYYDDMMYYY si es un rango.

Ejemplo:

TimeAverageMap_OxigenoDisuelto_MicromolesPorLitro_0101199504042017.png


