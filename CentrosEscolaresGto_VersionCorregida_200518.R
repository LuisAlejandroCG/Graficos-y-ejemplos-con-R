library(sf)
library(ggplot2)
library(foreach)
library(forcats)
library(dplyr)
library(stringr)
library(readr)

#Base de datos
gto <- read_csv(sprintf("http://fs.planeacion.sep.gob.mx/cct/cct11.csv"))

#Mapa
mapgto <- read_sf("http://geoinfo.iplaneg.net/geoserver/wms/kml?layers=geonode%3Amgm_gto2010&mode=download") %>%
  st_collection_extract("POLYGON")

#Vector de niveles educativos
nivel_ed <- c("Primaria", "Secundaria", "Bachillerato", "Superior")

#Creación del mapa
gtomap <- subset(gto, select=c(longitud, latitud, nnivel)) %>%  #Latitud y longitud serán nuestros X y Y en el plot principal
	filter(nnivel%in%c("PRIMARIA", "SECUNDARIA", "BACHILLERATO", 
      "PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA", "PROFESIONAL")) %>%	#Seleccionamos los niveles educativos
  	mutate(nnivel= case_when(nnivel== "PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA" ~ "Superior",
      nnivel=="PROFESIONAL" ~ "Superior", 	#Agrupamos las categorías de "Superior" en una sola y cambiamos el nombre
      nnivel=="SECUNDARIA" ~ "Secundaria",
      nnivel=="PRIMARIA" ~ "Primaria",
      nnivel=="BACHILLERATO" ~ "Bachillerato")) %>% 
  	mutate(nnivel= factor(nnivel, levels=nivel_ed)) %>%	
  	###No es necesario hacer melts, por lo tanto no se usa la librería reshape2###
  	ggplot(aes(x = longitud, y = latitud, colour = nnivel, shape=nnivel)) + geom_point() +
	###Los polígonos de municipios se superponen a los puntos
 	geom_sf(data=mapgto, inherit.aes=F, alpha=0) + 
   	facet_wrap(~nnivel) +
  	theme(legend.title=element_blank()) +
  	labs(x = "", y="", title="Distribución de centros escolares en Guanajuato por nivel educativo") 

gtomap