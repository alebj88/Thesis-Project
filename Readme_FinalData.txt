Los datos de esta carpeta fueron creados utilizando el codigo Proyecto.R al unir las matrices
ambientales con las biologicas.

Se tomaron las tablas generadas por DatosGoogleEarthCor.R: 

FactorsTransect_BenthicSubstrate_ProcMan2.csv
FactorsTransect_GreatGroups_ProcMan2.csv
FactorsTransect_SCLEGrowth_ProcMan2.csv
FactorsTransect_SCLESpecies_ProcMan2.csv

Y usando radios de 0.5kms ,1.5kms ,5kms y 20kms, (para la media) se unierion a las tablas:

MatrizAmbientalGIO1.csv
MatrizAmbientalGIO2.csv

Los archivos LatLonObjDist.csv y LatLonObjTemp.csv fueron creados una vez finalizo la construccion 
de la tabla FactorsTransect_BenthicSubstrate_Matrix.csv por medio del script "RecuperadorLatLonObj.R"

Esa tablas contiene la informacion de los puntos de geolocalizacion de los objetos mencionados en
las matrices finales.

Para evitar correr el programa Proyecto.R cuatro veces, se empleo el script EasyProyecto.R para hacer 
la union por geolocalizacion. 

Ese programa usa la informacion de la tabla FactorsTransect_BenthicSubstrate_ProcMan2_proc1_proc1.csv
(la generada por Proyecto.R al ejecutarlo dos veces sobre FactorsTransect_BenthicSubstrate_ProcMan2.csv) 
para agregar a la informacion oceanografica a las demas con un simple merge.

El programa EasyProyecto.R agrega las tablas de forma automatica en esta carpeta.

Las tablas aqui presentes recibieron la data de corrientes marinas usando la media y radios de 1, 5, 20 
y 50kms.