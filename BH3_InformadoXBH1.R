library(readxl)
library(readr)
library(rgdal)
library(sf)
library(sp)
library(raster)
library(terra)
library(spatialEco)
library(tidyverse)
data_bh3 <- read_csv("DATOS/DataReady_JUNTO.csv")
glimpse(data_bh3)

# sacar area de Grilla x Celda (Ana le dice estación) y luego saco porcentaje de area por Categoria de "CATESF"

#y luego hacer tabla con porcentaje y plot.

LAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
LonLatProj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'"

#Code to apply BH3 informed by BH1
#Cargamos los MSFD Broad Habitats de Edmonet (EDMONET, 2021) y los proyectamos en LAEA
#Habitats <- readOGR(dsn="C:/Users/ana.magro/Desktop/Codigo_taller/Cádiz Códigos/2_BH1_Preparing_Data_for_SSfunctionMisDatosCadiz/Input", layer="Habitats_region_IV")

#"E:/OneDrive/IEO/ESTRATEGIA MARINA/CircaSed/Carpeta_Maider_BH1/Scripts/2_BH1_Preparing_Data_for_SSfunction/Noratlantica/Input/Habitats", layer="Habitats_norte")
#CArgo la capa de hábitat y la proyecto a LAEA
Habitats <- sf::st_read(dsn="C:/Users/ana.magro/Desktop/Proyectos/Codigo_taller/Cádiz Códigos/5_BH1_Prediction_Threashold_Disturbance_SUD/Input/Habitats santander buenos script 5", layer="Habitats_cadiz")

Habitats_proj <- spTransform(Habitats, CRSobj = LAEA)

# Cargamos el stack raster con los valores de SAR (resultado del script 1_BH1_VMS_Data_preparation).
#Traw_norte <- stack("E:/OneDrive/IEO/ESTRATEGIA MARINA/CircaSed/Carpeta_Maider_BH1/Ana/Outputs/vms_nora_filtered_01_LAEA.tif") 

#Cargamos la pesca
Traw_norte <- stack("C:/Users/ana.magro/Desktop/Codigo_taller/Cádiz Códigos/1_BH1_VMS_data_preparationDatosCadiz/Output/vms_suda_filtered_01_LAEA.tif") 

head(Traw_norte)
windows()
plot(Traw_norte)


# Nos aseguramos de que, si no hay valor de SAR (= NaN) el valor sea =0 
Traw_norte[is.na(Traw_norte)] <- 0 

# Y de  los nombres de los rasters
names(Traw_norte) <- c("Te_2009", "Te_2010", "Te_2011", "Te_2012", "Te_2013",
                       "Te_2014", "Te_2015", "Te_2016", "Te_2017", "Te_2018",
                       "Te_2019","Te_2020","Te_2021", "Te_2022")
plot(Traw_norte)


#Convertimos el raster Brick (no te dejan modificar las cosas asi que lo convierto en una nuba de puntos) en un SpatialDataFrame y este en un DataFrame. Al hacer esto nos aparecen las coordenadas del centroide de cada celda
#Obtenemos el punto en el medio de la celda. Capas de pesca para cada una de las celdas con sus coordenadas
#Convertimos un raster en puntos
Traw_norte_DF <- as.data.frame(as(Traw_norte, "SpatialPixelsDataFrame"))

# Determinamos para cada pixel (en qu? pol?gono (h?bitat) cae su centroide
#pts.poly <- spatialEco::point.in.poly(SpPoints , Habitats_proj)
#head(pts.poly@data)
#Nos quedamos con las coordenadas
CC <- Traw_norte_DF
head(CC)
coords <- cbind(CC$x, CC$y)



#Nos quedamos solo con los campos que nos interesan
Habitats_Simp <- Habitats_proj[,"MSFD_BBHT"]

#Creamos un spatialPoints a partir del data frame
coordinates(CC) <- c("x", "y")
#Le decimos en qué coordenadas esta
crs(CC) <- LAEA 

#Sacamos el h?bitat para cada punto (PUEDE TARDAR UN POCO)
pts.poly2<- sp::over(CC, Habitats_Simp) #Vector con el tipo de h?bitat en el que cae cada punto
#A cada centroide le sacamos el habitat en el que cae ese punto

#Resumen: Hasta aquí he convertido un raster con la presión pesquera en puntos, y ahora estoy viendo en que habitat cae cada punto, de tal manera que a cada celda le asocio un
#habitat en función de donde cae el centroide de esa celda. REsoluciona 0.01

#PAra cada celda te dice el habitat pero no sabemos si tienen pesca o  no
head(pts.poly2)


MySPDF <- as(Traw_norte, "SpatialPixelsDataFrame")

#sacamos el valor medio de los periodos y hacemos un satck con los valores medio y los convertimos en spatialdataframe. Meto los valores año a año y tambien los valores medios

Period_1 <- MySPDF[,c("Te_2009",
                      "Te_2010",
                      "Te_2011",
                      "Te_2012",
                      "Te_2013",
                      "Te_2014",
                      "Te_2015")]
Period_2 <- MySPDF[,c("Te_2016",
                      "Te_2017",
                      "Te_2018",
                      "Te_2019",
                      "Te_2020",
                      "Te_2021",
                      "Te_2022")]

MeanEffort_P1 <- stackApply(stack(Period_1)  , indices =  rep(1,nlayers(Period_1)), fun = "mean", na.rm = T)
MeanEffort_P2 <- stackApply(stack(Period_2)  , indices =  rep(1,nlayers(Period_2)), fun = "mean", na.rm = T)

FinalStack <- stack(Traw_norte, MeanEffort_P1, MeanEffort_P2)
FinalSPDF <- as(FinalStack, "SpatialPixelsDataFrame")

#Generamos un nuevo campo para que cada celda tenga un valor unico
FinalSPDF$UniqCell <- c(1:nrow(FinalSPDF))

#convierto este objeto en un data frame.Sacamos el vector con todos los habitats y lo unimos al data frame con las presiones, y de ahí vamos a sacar primero que habitats tienen presion y luego el area y el objeto espacial para todos los habitat juntos evaluados con BH3 a partir de BH1

FinalDF <- as.data.frame(as(FinalStack, "SpatialPixelsDataFrame"))


names(FinalDF) <-c("Te_2009", "Te_2010", "Te_2011", "Te_2012", "Te_2013",
                   "Te_2014", "Te_2015", "Te_2016", "Te_2017", "Te_2018",
                   "Te_2019","Te_2020","Te_2021", "Te_2022", "Mean_P1", "Mean_P2", "Lon", "Lat")
FinalDF$MSFDBH <-pts.poly2$MSFD_BBHT
#Tenemos una columna para cada año con pesca, los valores medio, lon y lat y habitat asociado
head(FinalDF)

table(FinalDF$MSFDBH)

#Generamos un nuevo campo para que cada celda tenga un valor ?nico
FinalDF$UniqCell <- c(1:nrow(FinalDF))
#Creamos una copia para trabajar con ella para sacar areas.
FDF_Areas <- FinalDF

#CUIDADO. Una vez corramos esta linea de codigo, este objeto ya no sirve como raster

FDF_Areas <- na.omit(FDF_Areas)
#Miramos los habitats que hay en ese objeto
unique(FDF_Areas$MSFDBH)

#Eliminamos los MSFD BH evaluados con BH1. #Miramos qué habitats tienen presión pesquera y vemos cuales vamos a evaluar, eliminamos la roca

FDF_Areas <- FDF_Areas[!FDF_Areas$MSFDBH%in%c("Upper bathyal sediment", "Offshore circalittoral sand", "Circalittoral sand"),]

#Eliminamos la roca

Roca <- c("Lower bathyal rock and biogenic reef","Upper bathyal rock and biogenic reef", "Offshore circalittoral rock and biogenic reef", "Circalittoral rock and biogenic reef", "Infralittoral rock and biogenic reef")
FDF_Areas <- FDF_Areas[!FDF_Areas$MSFDBH%in%Roca,]
unique(FDF_Areas$MSFDBH)

#Ahora hacemos un loop para ver de los h?bitats que quedan, cuales estan expuestos a presion pesquera. PAra ello vamos a calcular el n?mero de pixels con presi?n mayor que cero
#Lo hago para el periodo dos. Para el el es camabiar P2 x P1

#CAda numero del vector es el codigo de un habitat. Vamos a mirar cuantas filas tienen presión mayor de cero
MyHab <- unique(FDF_Areas$MSFDBH)
Trawl <- vector()

for (i in 1:length(MyHab)){
  DataByHab <- FDF_Areas[FDF_Areas$MSFDBH==MyHab[i],] #El loop me dice: Primera linea me dice i = 1, me quedo con el habitat abisal (SI miro el data frame, solo estan los puntos que han caido en este habitat), y le digo que dentro del abisal se quede con los que tengan presion para el periodo 2 mayor de cero 
  DataByHab <-DataByHab[DataByHab$Mean_P2>0,]
  Trawl[i] <- nrow(DataByHab)} #le digo que me diga cuantas filas tiene ese objeto, tiene 0, es decir en el abisal no hay presion pesquera. Si pongo i=2 ya me pasa al siguiente habitat 

#Junto en un vector espacial y lo convierto a numerico
TrawlByHab <- cbind.data.frame(MyHab, Trawl)

TrawlByHab$Trawl <- as.numeric(TrawlByHab$Trawl)

#Nos cargamos las de cero
SelectedHabs <- TrawlByHab[TrawlByHab$Trawl>0,] #Nos quedamos solo con los Habs que tienen al menos un pixel de presi?n >0
SelectedHabs$MyHab

#Ahora hay que decidir que sensibilidad tienen los habitats que nos quedan sin evaluar. En nuestro caso son, el offshore coarse sediment, 
#y luego 3 circalitorales que hemos evaluado previamente en su version offshore (Circ. sand, mud y coarse)

SelectedHabs

#Nos quedamos con el habitat que queremos hacer por separado y tambien ponemos solo una sensibilidad
SelectedHabs<- SelectedHabs[SelectedHabs$MyHab=="Offshore circalittoral mixed sediment",]
#En base al orden en el que aparecen en el objeto SelectedHabs genero un vector con las sensibilidades de cada h?bitat. 
#En mi caso es 5 (para Upper bathyal sediment or Upper bathyal rock and biogenic reef) y 3 para los dem?s (excepto para C. mud  que es dos).

#Pongo las sensibilidades de los habitats que conocemos por otras demarcaciones, si no miramos las especies que salen

#SelectedHabs$Sens <- c(3,3,3,2,3,3,3,3,3) #El NA es xk uno de mis habitats es NA
##################################################################################################################################################################################
SelectedHabs$Sens <- c(3)
SelectedHabs <- na.omit(SelectedHabs)

  
#Ya tengo la columna de sensibilidad. Ahora se la añado al DF con los valores de presion 
#Para hacer el merge le doy el nombre adecuado al habitat
names(SelectedHabs) <- c("MSFDBH", "Freq", "Sens")
FDF_Areas2 <- merge(FDF_Areas, SelectedHabs, by="MSFDBH")

#Ahora hacemos el loop para covertir los valores de arrastre en Good (1) not good (2) usando en cada caso el TH apropiado (en funci?n de la sens)

DF_Th <- cbind.data.frame(c(2,3,4,5),c(3.6,2,0.6,0))#le damos a esas sensibilidades los umbrales
names(DF_Th) <- c("Sens", "Th")

Result <- list()#Loop, convertimos cada celda, cada punto en GES noGES, en funcion de si para la sensibilidad que le hemos asignado pasa el humbral que le corresponde

MyRelevantHabs <- SelectedHabs$MSFDBH #vector en el que van los habitats a evaluar (5)

for (i in 1:length(MyRelevantHabs)){
  MyHab <- MyRelevantHabs[i]  #si le digo que i=1 nos quedamos con los datos del habitat UBS,
  DataByHab <-FDF_Areas2[FDF_Areas2$MSFDBH==MyHab,] #nos quedamos con los datos
  SensByHab <- unique(DataByHab$Sens) # le decimos que se quede con el umbral que corresponde a la sensibilidad. 
  MyTh <- DF_Th[DF_Th$Sens==SensByHab,]$Th
  GES<- ifelse(DataByHab$Mean_P2>MyTh,2,ifelse(DataByHab$Mean_P2==0,0,1))#y le decimos que si es mayor que ese umbral es un dos y si no es un 1.
  Result[[i]] <-cbind.data.frame(rep(MyHab, length(GES)), GES, DataByHab$UniqCell) #Para cada celda del habitat ha comparado el valor de pesca con le umbral (Ej:UBS que tiene 0), si es mayor de 0 es un 2 si no un 1 y lo guarda
  
}
#GES <- ifelse(DataByHab$Mean_P2>MyTh,2,1)
FinalRes <- do.call(rbind.data.frame, Result)

names(FinalRes) <- c("MSFDBH", "GES", "UniqCell")

#table(FinalRes$MSFDBH, FinalRes$GES) #Con esto ya tenis el area en GES (1) y no GES (2, no afectados por la huella) (n?mero de celdas por area de cada celda)
FinalArea<-as.data.frame(table(FinalRes$MSFDBH, FinalRes$GES)) 
names(FinalArea)<- c("MSFDBH", "GES", "Freq")
FinalArea$Area<- (FinalArea$Freq*881*1080)/1000000 #Le damos el area mirando la resolucion del stack FinalStack
write.csv(FinalArea, "C:/Users/ana.magro/Desktop/Codigo_taller/Cádiz Códigos/FinalTable_Perido_2_OffCircaMixedSedim_BH3.csv")


#Pegamos el valor de GES no GES al objeto espacial, usamos el de los NA tambien. GES en verde, NoGES en marron

FinalSPDF <- merge(FinalSPDF, FinalRes, by="UniqCell", all.x=TRUE)
head(FinalSPDF)
windows()
plot(raster(FinalSPDF[,"GES"]))

#FinalRaster <- writeRaster(raster(FinalSPDF[,"GES"]), "")
FinalRaster <- writeRaster(raster(FinalSPDF[,"GES"]), "C:/Users/ana.magro/Desktop/Codigo_taller/Cádiz Códigos/Mapa_Periodo_2_OffCircaMixedSedim_GES_BHR2.tif", overwrite=TRUE)




  
  
  


