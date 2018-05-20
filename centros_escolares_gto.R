##Visualización de centros escolares en Guanajuato (datos a 2015)

##Cargamos librerías
library(foreach)
library(forcats)
library(ggplot2)
library(dplyr)
library(stringr)
library(raster)
library(readr)
library(reshape2)

##Cargamos el archivo csv
gto<- read_csv(sprintf("http://fs.planeacion.sep.gob.mx/cct/cct11.csv"))

##Importamos la función geom_sf de la versión en desarrollo de ggplot2
devtools::install_github("tidyverse/ggplot2")

##Descargamos un mapa de Guanajuato y convertimos a data.frame para utilizarlo en la llamada a ggplot
mapgto <- sf::read_sf("http://geoinfo.iplaneg.net/geoserver/wms/kml?layers=geonode%3Amgm_gto2010&mode=download", quiet=T)
map <- data.frame(mapgto$geometry)
 

##A fin de que los gráficos desplieguen en forma ascendente los niveles educativos, creamos el siguiente vector que se utilizará más adelante
nivel_ed <- c("Primaria", "Secundaria", "Bachillerato", "Superior")

gtomap <- subset(gto, select=c(longitud, latitud, nnivel)) %>%		#Latitud y longitud serán nuestros X y Y en el plot principal
	filter(nnivel%in%c("PRIMARIA", "SECUNDARIA", "BACHILLERATO", 
	"PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA", "PROFESIONAL")) %>%		#Seleccionamos los niveles educativos
	mutate(nnivel= case_when(nnivel== "PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA" ~ "Superior",
		nnivel=="PROFESIONAL" ~ "Superior", 	#Agrupamos las categorías en una sola y cambiamos el nombre
		nnivel=="SECUNDARIA" ~ "Secundaria",
		nnivel=="PRIMARIA" ~ "Primaria",
		nnivel=="BACHILLERATO" ~ "Bachillerato")) %>% 
	mutate(nnivel= factor(nnivel, levels=nivel_ed)) %>%	
	#Creamos el factor nnivel con el orden establecido anteriormente
	melt(id =c("longitud","latitud"))  %>% 
	mutate(value= factor(value, levels=nivel_ed)) %>%		
	#Dado que "melt" crea la variable "value", la convertimos en factor y ordenamos los niveles educativos
	ggplot(aes(x = longitud, y = latitud, col= value, shape=value)) +
	geom_sf(data=map, inherit.aes=F) + 
	#Con inherit.aes=F nos aseguramos que el mapa del estado se copie sin atributos aes
	geom_point() +
	facet_wrap(~value) +
	#Con esta instrucción, crearemos un mapa para cada nivel educativo
	theme(legend.title=element_blank()) +
	labs(x = "", y="") 
gtomap

#El mapa resultante es el archivo: photo_2018-05-19_19-04-40.jpg


