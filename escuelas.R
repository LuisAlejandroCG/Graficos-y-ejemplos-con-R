library(foreach)
library(forcats)
library(ggplot2)
library(dplyr)
library(stringr)
library(raster)
library(readr)
library(reshape2)

##Leer las bases de datos entidad por entidad y combinarlas en una sola
#foreach(i=1:32, .combine=rbind) %do% {
#  read_csv(sprintf("http://fs.planeacion.sep.gob.mx/cct/cct%02d.csv", i))
#} -> nacional

##Notas
#sprintf() convierte cualquier expresión a un vector de caracteres
#"[...]cct%02d.csv", i)) %02d se refiere a los números de i=1:32 que se convierten en caracteres

##Leer desde archivo
gto <- read.csv("C:\\Users\\Alex\\Desktop\\cct11.csv", header=T, stringsAsFactors=F)
gto$region <- 11

##Archivo csv
gto<- read_csv(sprintf("http://fs.planeacion.sep.gob.mx/cct/cct11.csv"))

##Para importar geom_sf
devtools::install_github("tidyverse/ggplot2")

##Mapa
mapgto <- sf::read_sf("http://geoinfo.iplaneg.net/geoserver/wms/kml?layers=geonode%3Amgm_gto2010&mode=download", quiet=T)
map <- data.frame(mapgto$geometry)
 

##Vector de niveles educativos (para ordenar)
nivel_ed <- c("Primaria", "Secundaria", "Bachillerato", "Superior")

gtomap <- subset(gto, select=c(longitud, latitud, nnivel)) %>%
	filter(nnivel%in%c("PRIMARIA", "SECUNDARIA", "BACHILLERATO", 
	"PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA", "PROFESIONAL")) %>%
	mutate(nnivel= case_when(nnivel== "PEDAGOGICA, UNIVERSITARIA O TECNOLOGICA" ~ "Superior",
		nnivel=="PROFESIONAL" ~ "Superior", 
		nnivel=="SECUNDARIA" ~ "Secundaria",
		nnivel=="PRIMARIA" ~ "Primaria",
		nnivel=="BACHILLERATO" ~ "Bachillerato")) %>% 
	mutate(nnivel= factor(nnivel, levels=nivel_ed)) %>%
	melt(id =c("longitud","latitud"))  %>% 
	mutate(value= factor(value, levels=nivel_ed)) %>%
	ggplot(aes(x = longitud, y = latitud, col= value, shape=value)) +
	geom_sf(data=map, inherit.aes=F) +
	geom_point() +
	facet_wrap(~value) +
	theme(legend.title=element_blank()) +
	labs(x = "", y="") 
gtomap

ggplot() 


