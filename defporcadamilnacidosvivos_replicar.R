library(tidyverse)

##Breve script para calcular la tasa de mortalidad de niños menores de un año por cada mil nacidos vivos en Guanajuato (2012-2016)
##Se incluyen ejemplos de gráficos comparativos
##Las bases de datos para replicar este script son "muertes" y "nacimientos"
##Las BD se obtienen por separado del rubro "Registros administrativos" en http://www.inegi.org.mx/est/lista_cubos/

##Nacimientos
nac <- read.csv("ruta del usuario/nacimientos.csv", header=T) %>%
	select(c(1,X2012:X2016)) %>%          #Se puede seleccionar por posición y por nombre de variable en la misma línea
	rename_at(2:6, function(x) c("2012", "2013", "2014", "2015", "2016")) %>%
	gather(año, nacimientos, 2:6) %>%
	replace(is.na(.), 0) 

str(nac)

##Muertes
def <- read.csv("ruta del usuario/muertes.csv", header=T)%>%
	select(-48) %>%
	gather(municipio, defunciones, 2:47) %>%
	filter(año%in%c(2012:2016)) %>%
	arrange(año) %>%
	replace(is.na(.), 0) %>%
	mutate(defunciones=as.numeric(defunciones))

str(def)

##Extracción columna
defunciones <- def[,3]

##Base de datos final
nac_mort <- cbind(nac, defunciones) %>% 
mutate(defxmil= (defunciones/nacimientos)*1000)		#Tasa de defunciones por cada mil nacidos vivos
fix(nac_mort)

##Plots
##Municipios del corredor industrial
corredor <- filter(nac_mort, mpio%in%c("Celaya", "Irapuato", "León", "Salamanca")) %>%
ggplot(aes(x=año, y=defxmil, group=mpio))+
geom_line(size=1.5) +
facet_wrap(~mpio)
corredor

##Municipios del noroeste
noroeste <- filter(nac_mort, mpio%in%c("Atarjea", "Doctor Mora", "San Luis de la Paz", 
"San José Iturbide", "Santa Catarina", "Tierra Blanca", "Victoria", "Xichú")) %>%
ggplot(aes(x=año, y=defxmil, group=mpio))+
geom_line(size=1.5) +
facet_wrap(~mpio)
noroeste

