#####################################################
### ESCOLARIDAD E INGRESOS POR SEXO EN GUANAJUATO ###
#####################################################

#######################################################
#Estimaciones con base en la Encuesta Intercensal 2015#
#######################################################

#Objetivos: 
#1. Estimar la escolaridad promedio por municipio para las personas de 15 años o más que reciben ingresos
#2. Estimar el ingreso promedio por municipio para las personas de 15 años o más que reciben ingresos
#3. Visualizar en un gráfico la relación entre escolaridad y nivel de ingreso por municipio para hombres y mujeres

library(tidyverse)

##Ajustamos opciones para desplegar resultados
options(tibble.print_max = 100, tibble.print_min = 50, digits=2)

###Descargar archivo zip
download.file("http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/intercensal/2015/microdatos/eic2015_11_csv.zip", "eic2015_11_csv.zip")
zipgto <- unzip("eic2015_11_csv.zip")
str(zipgto)
##Con str() podemos observar los nombres de los archivos en el zip. Elegimos el archivo TR_PERSONA11.CSV
##Dado que el archivo queda alojado en el directorio de trabajo actual, no es necesario establecer una ruta con read.csv

###Base de datos 
base <- read.csv("TR_PERSONA11.CSV", header=T)%>%
	rename_all(tolower) %>% 
	filter(cobertura%in%c(1,2) & escoacum!=99& edad%in%c(15:110) & ingtrmen!=0 & ingtrmen!=999999 & ingtrmen!=999998) %>%
	subset(select=c("nom_mun", "factor", "escoacum", "edad", "ingtrmen", "sexo"))%>%
	mutate(sexo=ifelse(sexo==1, "Hombre", "Mujer")) %>%
	rename(Municipio = nom_mun) %>%
	group_by(Municipio, sexo)

###Base de datos consolidada
eei <- summarise(base, grado_educ= sum(escoacum*factor), personas=sum(factor), 
	promedio_esc = grado_educ/personas, edad_promedio=sum(edad*factor)/personas, 
	ingreso_medio= sum(ingtrmen*factor)/personas, ingreso_mediano=median(ingtrmen), min_ing=min(ingtrmen),
	max_ing=max(ingtrmen), sd_ing=sd(ingtrmen), ing_3sd_max=ingreso_medio+3*sd_ing)%>%
	subset(select=c("Municipio", "promedio_esc", "edad_promedio", "ingreso_medio","max_ing", 
	"ing_3sd_max","sexo", "ingreso_mediano")) 


###Gráfico
esc_ing <- ggplot(eei, aes(promedio_esc, log(ingreso_medio), colour=sexo, shape=sexo)) +
	geom_point(size=2) +
	geom_text(aes(label=Municipio), size=2.7, colour="black", check_overlap=T) +
	theme_bw() +
	labs(x="Escolaridad promedio", y= "Logaritmo del ingreso medio",
	title="Escolaridad promedio y log ingreso promedio para la población de 15 años o más que recibe ingresos en los municipios de Guanajuato, 2015") +
	theme(legend.title=element_blank(),axis.text.y=element_text(angle = 0, hjust = 0.5, color="black", size=9),
 	axis.text.x = element_text(angle = 0, hjust = 0.5, color="black", size=9), 
	plot.title=element_text(hjust = 0.5, size=11)) 

###Guardar el gráfico en formato png en el directorio de trabajo
png(filename="esc_ing.png", width=12, height=7, units="in", res=2000)	
esc_ing
dev.off()
