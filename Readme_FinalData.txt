INFORMACION SOBRE LAS TABLAS GENERADAS

-----------------------------
Para crear los directorios de trabajo y descargar los codigos empleados en este trabajo de investigacion se debe
ejecutar el codigo Proyecto.R. El mismo montara las carpetas y descargara los scripts de Github.
 
-----------------------------
Las matrices Biologicas

Las matrices biologicas creadas por el LEEUSB y almacenadas en la carpeta "Raw Data" del proyecto

FactorsTransect_BenthicSubstrate.csv
FactorsTransect_GreatGroups.csv
FactorsTransect_SCLEGrowth.csv
FactorsTransect_SCLESpecies.csv

fueron procesadas mediante el codigo "Biologicas.R" el cual elimina variables inecesarias y promedia los datos
por transecta. Tal codigo genero las tablas 

BenthicSubstrate.csv
GreatGroups.csv
SCLEGrowth.csv
SCLESpecies.csv

en la carpeta Biologicas del proyecto.

-----------------------------
Las matrices oceanograficas 

MatrizAmbientalGIO1.csv
MatrizAmbientalGIO2.csv

fueron creadas con el codigo "Procesador de Mapas.R" al ejecutarlo sobre las carpetas de mapas satelitales
descargadas de los servidores de Earthdata. (Informacion mas detallada sobre este procedimiento esta disponible
en el archivo Readme_Giovanni.txt presente en la carpeta del procesador, "Procesador de Mapas.R").

La tabla de corrientes marinas 

corrientesAux.csv  

fue creada ejecutando el codigo "Corrientes.R".Este descarga y procesa los datos provenientes de la NOAA

La tabla con informacion sobre los puntos de interes costeros  

GoogleEarthRiosLugares.csv

fue creada con el codigo GoogleEarthRiosLugares.R. empleando datos de Google Earth. 

-----------------------------
Las matrices biologicas con datos oceanograficos y demograficos

BenthicSubstrate_Matrix.csv  
GreatGroups_Matrix.csv   
SCLEGrowth_Matrix.csv
SCLESpecies_Matrix.csv

fueron creadas utilizando el codigo "Union por Geolocalizacion.R" para unir las matrices ambientales con 
las biologicas.

Se tomo la tabla BenthicSubstrate.csv y mediante el codigo "Union por Geolocalizacion.R"se le agrego la 
informacion demografica contenida en la tabla "GoogleEarthRiosLugares.csv" (generada con el script 
"GoogleEarthRiosLugares.R") usando una distancia maxima de 100kms y 15 dias de viaje en corriente.
La misma fue procesada bajo la opcion de distancia minima ingresada durante la ejecucion del programa.

Se ejecuto el codigo nuevamente usando radios de 0.5kms ,1.5kms ,5kms y 20kms, (para la media) y con ello se
le agrego a la tabla procesada (TablaMin_BenthicSubstrate_proc1.csv) la informacion contenida en las matrices
 ambientales:

MatrizAmbientalGIO1.csv
MatrizAmbientalGIO2.csv

Finalmente se ejecuto el programa una ultima vez sobre la matriz de datos generada en el proceso anterior y 
con radios de 1, 5, 20 y 50kms se les agrego la informacion de corrientes marinas contenida en:

corrientesAux.csv   

Con el objetivo de evitar ejecutar el algoritmo de union por geolocalizacion 4 veces, uno por cada tabla 
biologica, se creo un script para su union por merge ("EasyMerge.R"). Se ejecuto tal script y de forma 
automatica las tablas  

BenthicSubstrate.csv
GreatGroups.csv
SCLEGrowth.csv
SCLESpecies.csv

recibieron la informacion ambiental y fueron guardadas en la carpeta Datos Finales de la carpeta del proyecto
con los nombres


BenthicSubstrate_Matrix.csv  
GreatGroups_Matrix.csv   
SCLEGrowth_Matrix.csv
SCLESpecies_Matrix.csv

----------------------------------------------------------
La tabla BenthicSubstrate_proc1.csv creada en la carpeta Datos Finales del programa "Union por geolocalizacion.R"
contiene la informacion de los puntos de geolocalizacion de los objetos presentes  en las matrices finales
(...._Matrix.csv).


-----------------------------------------------------------
Para el modelo predictivo implementado en la aplicacion shiny, varias tablas extra fueron creadas:



El codigo EsferasPredictivas.R le permite al usuario establecer la metodologia de estimacion de valores
ambientales. Una vez ejecutada crea el archivo "entrenador.txt" en la carpeta de la APP (ShinyPredictor) y 
en caso de haber existido un entrenamiento previo del modelo, lo elimina de forma automatica. (Siempre que 
se ejecute este script, el modelo debe ser creado nuevamente corriendo la APP.

Usando el codigo "FiltroPredictores.R" se crearon dentro de la carpeta de la APP las tablas

MatrizAmbientalGIO1_Filtrada.csv  
MatrizAmbientalGIO2_Filtrada.csv  

las cuales contienen las variables oceanograficas necesarias para la prediccion de los ecosistemas.


Para realizar una prediccion general sobre puntos marinos repartidos en el mar venezolano se creo la tabla
de puntos de geolocalizacion "Mar_Vzla.csv" aplicando el codigo "Mar_Vzla.R" sobre la tabla creada por el 
procesadorde mapas "Procesador de Mapas.R" al usar la imagen "Continental.jpg".

La tabla "Mar_Vzla.csv" recibio los datos de corrientes marinas, oceanograficos y demograficos mediante
el codigo "Union por Geolocalizacion.R". Los datos demograficos agregados con la tabla  
"GoogleEarthRiosLugares.csv" fueron procesados bajo la opcion de distancia minima solicitada en el codigo 
"Union por Geolocalizacion.R". Los radios de busqueda que se emplearon fueron los mismos tomados arriba.
La informacion demografica fue seleccionada con 100km de distancia y 0 dias de viaje en corriente.


Finalizada la union por geolocalizacion de las variables ambientales, las mismas fueron filtradas, procesadas
y reordenadas con el codigo "PredictoresMarVzla.R". De ese modo se mentuvieron solo aquellas utiles para la 
prediccion. La tabla resultante, "PredictoresMarVzla.csv", se crea automaticamente en la carpeta de la
APP.


Luego de crear la tabla "PredictoresMarVzla.csv" se ejecuto el codigo de la aplicacion con el programa
"global.R". En el proceso se construye el modelo predictivo con el archivo "entrenador.txt" y se genera la 
tabla con la prediccion de los puntos de geolocalizacion marinos con informacion ambiental contenida en 
"PredictoresMarVzla.csv". La misma es llamada "PredictoresMarVzla_Ecosystem.R" si se guarda de forma 
automatica en la carpeta de la APP. Esta cual que contiene la respectiva prediccion de los ecosistemas en 
los puntos creados. 

Observacion: Siempre que se desee cambiar la metologia a emplear con esferas predictivas se debe correr el
codigo "EsferasPredictivas.R" con las opciones deseas y luego ejecutar el codigo de la APP para aplicar las
actualizaciones. Siempre que se ejecute el codigo "EsferasPredictivas.R", este buscara en la carpeta de la
APP el modelo predictivo para eliminarlo.

Los archivos rmd de resultados deben ser corridos con la misma configuracion establecida en el codigo 
"EsferasPredictivas.R".

Al ejecutar la aplicacion con el "global.R", son creados los mapas de confiabilidad predictiva. Ellos 
indican los puntos de geolocalizacion del mapa donde las variables del modelo hacen match y donde no.


Para efectos de orden las distintas configuraciones usadas fueron almacenadas en la carpeta Test de
ShinyPredictor.