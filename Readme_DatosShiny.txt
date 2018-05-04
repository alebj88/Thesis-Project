Para construir las tablas DataCoralesProcMan.csv y DataCoralesProcMan2.csv se procede de la siguiente forma.

1- Correr en modo source el codigo Ejecutable.R e indicar que se desean agregar datos ambientales al archivo DataCorales.csv

2- Seleccionar la tabla WorldOceanographicAtlas.csv y especificar los radios de busqueda en 10, 120 y 400 kilometros. 
Incluir todas las variables existentes y continuar.

3- Una vez finalizado el procedimiento, correr en modo source el codigo Ejecutable.R de nuevo. Indicar que se desean agregar
datos ambientales al archivo DataCorales_proc1.csv y seleccionar la tabla de datos de Google Earth.

4- Establecer 15km de distancia en kilometros y 7dias de viaje en corrientes. 

5- Solicitar que se agregen todas las variables de la tabla de Google Earth y continuar.

6- Una vez finalizado el procedimiento renombrar el archivo DataCorales_proc1_proc1.csv, que se habra generado en la carpeta
Process Data, como DataCoralesProcMan.csv y agregarlo en el directorio ShinyExploratory ubicado en Shiny Apps. 

7- Ejecute el codigo DatosShiny.R alojado en la carpeta ShinyExploratory y el archivo DataCoralesProcMan2.csv sera creado
automaticamente. Adicionalmente se dara formato a la tabla DataCoralesProcMan.csv y se actualizara.