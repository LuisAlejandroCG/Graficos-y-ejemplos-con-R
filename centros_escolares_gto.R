### MAPA DE DISTRIBUCIÓN DE CENTROS ESCOLARES POR NIVEL EDUCATIVO EN EL ESTADO DE GUANAJUATO

library(sf)
library(ggplot2)
library(foreach)
library(forcats)
library(dplyr)
library(stringr)
library(readr)

#Descargar base de datos
gto <- read_csv(sprintf("http://fs.planeacion.sep.gob.mx/cct/cct11.csv"))

#Descargar mapa
mapgto <- read_sf("http://geoinfo.iplaneg.net/geoserver/wms/kml?layers=geonode%3Amgm_gto2010&mode=download") %>%
  st_collection_extract("POLYGON")

#Vector de niveles educativos 
#(Nos permitirá obtener los gráficos ordenados de menor a mayor nivel educativo)
nivel_ed <- c("Primaria", "Secundaria", "Bachillerato", "Superior")

#Creación del mapa
gtomap <- subset(gto, select=c(longitud, latitud, nnivel)) %>%  
	    filter(nnivel%in%c("PRIMARIA", "SECUNDARIA", "BACHILLERATO", 
      "PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA", "PROFESIONAL")) %>%	
  	  mutate(nnivel= case_when(nnivel== "PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA" ~ "Superior",
      nnivel=="PROFESIONAL" ~ "Superior", 	
      nnivel=="SECUNDARIA" ~ "Secundaria",
      nnivel=="PRIMARIA" ~ "Primaria",
      nnivel=="BACHILLERATO" ~ "Bachillerato")) %>% 
  	  mutate(nnivel= factor(nnivel, levels=nivel_ed)) %>%	
  	    ggplot(aes(x = longitud, y = latitud, colour = nnivel, shape=nnivel)) + geom_point() +
 	      geom_sf(data=mapgto, inherit.aes=F, alpha=0) + 
        facet_wrap(~nnivel) +
  	    theme(legend.title=element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5, color="black", size=7), 
	      plot.title=element_text(hjust = 0.5)) +
  	    labs(x = "", y="", title="Distribución de centros escolares en Guanajuato por nivel educativo") 

gtomap

###Pasos en la manipulación de la base de datos y creación del gráfico

#1. Seleccionamos las variables longitud, latitud y nnivel (nombre del nivel educativo)
#2. Filtramos los niveles educativos correspondientes a primaria, secundaria, bachillerato y profesional 
#3. Renombramos los niveles y agrupamos "PROFESIONAL" y "PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA" como "Superior"
#4. Asignamos a "nnivel" los niveles establecidos previamente
#5. Creamos el plot. Cabe señalar que geom_sf nos permite incorporar el mapa como parte del gráfico

##Para guardar el archivo en formato jpeg en el directorio de trabajo
jpeg(filename="gto.jpeg", width=10, height=5, units="in", res=3000)
gtomap
dev.off()
