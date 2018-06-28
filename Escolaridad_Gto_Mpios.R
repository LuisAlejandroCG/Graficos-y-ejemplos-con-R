#################################
### ESCOLARIDAD EN GUANAJUATO ###
#################################
#######################################################
#Estimaciones con base en la Encuesta Intercensal 2015#
#######################################################

#Objetivos: 
#1. Estimar la escolaridad promedio por municipio en Guanajuato utilizando la Encuesta Intercensal 2015
#2. Visualizar en un gráfico de barras los 5 municipios con mayor/menor escolaridad

library(tidyverse)

##Ajustamos opciones para desplegar resultados
options(tibble.print_max = 100, tibble.print_min = 50, digits=2)

###Descargar archivo zip
download.file("http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/intercensal/2015/microdatos/eic2015_11_csv.zip", "eic2015_11_csv.zip")
zipgto <- unzip("eic2015_11_csv.zip")
str(zipgto)
##Con str() podemos observar los nombres de los archivos en el zip. Elegimos el archivo TR_PERSONA11.CSV
##Dado que el archivo queda alojado en el directorio de trabajo actual, no es necesario establecer una ruta con read.csv

###Cálculo del promedio de escolaridad por municipio
promesc <- read.csv("TR_PERSONA11.CSV", header=T)%>%
	rename_all(tolower) %>% 
	filter(cobertura%in%c(1,2) & escoacum!=99& edad%in%c(15:110)) %>%
	subset(select=c("nom_mun", "factor", "escoacum")) %>%
	rename(Municipio = nom_mun) %>%
	group_by(Municipio) %>%
	summarise(grado_educ= sum(escoacum*factor), personas=sum(factor), 
	promedio_esc = grado_educ/personas) %>%
	rename("Escolaridad promedio" = promedio_esc) %>%
	subset(select=c("Municipio", "Escolaridad promedio"))
promesc

##Gráfico de barras
escmun <- subset(promesc, select= c(1:2)) %>%
	setNames(c("Municipio", "Escolaridad")) %>%
	arrange(desc(Escolaridad)) %>%
	"["(.,c(1:5, 42:46),) %>%	
	ggplot(aes(x=reorder(Municipio,-Escolaridad), y=Escolaridad)) + #ggplot no respeta el orden asignado, hay que utilizar reorder
	geom_bar(stat="identity", fill="mediumspringgreen") +
	labs(x="Municipio", title="Municipios con mayor y menor promedio de escolaridad (en años) en Guanajuato, 2015") +
	theme(axis.text.x = element_text(angle = 0, hjust = 0.5, color="black", size=10), plot.title=element_text(hjust = 0.5)) +
	geom_text(aes(label=round(Escolaridad, digits=2)), position=position_dodge(width=0.9), vjust=-0.25)

escmun
