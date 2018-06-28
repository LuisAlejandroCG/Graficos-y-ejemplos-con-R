########################################
### EDAD Y ESCOLARIDAD EN GUANAJUATO ###
########################################

#######################################################
#Estimaciones con base en la Encuesta Intercensal 2015#
#######################################################

#Objetivos: 
#1. Estimar la escolaridad promedio para la población de 15 años o más por municipio en Guanajuato utilizando la Encuesta Intercensal 2015
#2. Estimar la edad promedio para la población de 15 años o más por municipio en Guanajuato utilizado la EIC 2015
#2. Visualizar la relación entre edad promedio y escolaridad promedio

library(tidyverse)

##Ajustamos opciones para desplegar resultados
options(tibble.print_max = 100, tibble.print_min = 50, digits=2)

###Descargar archivo zip
download.file("http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/intercensal/2015/microdatos/eic2015_11_csv.zip", "eic2015_11_csv.zip")
zipgto <- unzip("eic2015_11_csv.zip")
str(zipgto)
##Con str() podemos observar los nombres de los archivos en el zip. Elegimos el archivo TR_PERSONA11.CSV
##Dado que el archivo queda alojado en el directorio de trabajo actual, no es necesario establecer una ruta con read.csv


###Cálculo de escolaridad promedio y edad promedio por municipio
edad_esc <- read.csv("TR_PERSONA11.CSV", header=T)%>%
	rename_all(tolower) %>% 
	filter(cobertura%in%c(1,2) & escoacum!=99& edad%in%c(15:110)) %>%
	subset(select=c("nom_mun", "factor", "escoacum", "edad")) %>%
	rename(Municipio = nom_mun) %>%
	group_by(Municipio) %>%
	summarise(grado_educ= sum(escoacum*factor), personas=sum(factor), 
	promedio_esc = grado_educ/personas, edad_promedio=sum(edad*factor)/personas) %>%
	subset(select=c("Municipio", "promedio_esc", "edad_promedio"))

##Para observar la tabla:
edad_esc

##Creación del gráfico
##Nota: Se agregó una recta de regresión para observar el tipo de relación entre los datos
muniescedad <- ggplot(edad_esc,aes(x=edad_promedio, y=promedio_esc)) + 
	geom_point(size=1.5) + 
	geom_smooth(method="lm", col="blue", se=F, size=0.8)+
	geom_text(aes(label=Municipio), size =2.8, check_overlap = TRUE) +
	theme_bw() +
	labs(x="Edad promedio", y= "Escolaridad promedio",
	title="Edad promedio y escolaridad promedio para la población de 15 años o más en los municipios de Guanajuato, 2015") +
	theme(axis.text.y=element_text(angle = 0, hjust = 0.5, color="black", size=9),
 	axis.text.x = element_text(angle = 0, hjust = 0.5, color="black", size=9), 
	plot.title=element_text(hjust = 0.5, size=12)) 

muniescedad

##Para guardar el gráfico en formato png en el directorio de trabajo, podemos emplear:
png(filename="esc_edad.png", width=12, height=6, units="in", res=2000)	
muniescedad
dev.off()
