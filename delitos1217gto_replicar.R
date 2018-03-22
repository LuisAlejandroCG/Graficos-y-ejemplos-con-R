library(tidyverse)


###Vector meses
###Correr antes de ejecutar el código del siguiente bloque
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

##Código inicial
##Después de indicar "ruta_del_usuario/nombre_archivo.csv", el código está listo para correrse
d1217 <- read.csv("ruta_del_usuario/nombre_archivo.csv", header=T)%>%
	filter(ENTIDAD=="GUANAJUATO" & ANO<2018& CONCEPTO != "OTRAS LEYES Y CODIGOS" & CONCEPTO !="OTROS DELITOS") %>% 
	rename_all(tolower) %>%
	select(-(2:5)) %>%
	rename("año"="ano") %>%
	gather(clave, valor, 3:14)%>% 	##Crea las columnas "clave" y "valor" para los valores en las columnas 3:14
	arrange(año) %>%	
	spread(tipo, valor)%>%			##Convierte "tipo" en columnas y les asigna los valores en "valor"	
	rename("mes"="clave") %>%
	mutate(mes= factor(mes, levels=meses)) %>%	##Cambiamos los niveles de la variable mes según el vector meses
	arrange(mes)%>%
	arrange(año) %>%
	rename_all(tolower) %>%
	replace(is.na(.), 0) %>%		#Sustituye por ceros todos los valores con NA
	mutate(pos = posesion+posesión, prod= produccion+producción, 
	traf= trafico+tráfico) %>%		#Teniendo ceros, ya podemos sumar columnas
	select(-(9:15)) %>%
	rename_at(c(5, 7:8,10:12), function(x) c("narcomenudeo", "otros_lfcd", "otros_lgs",
	"posesion","produccion","trafico")) %>%
	mutate(total= rowSums(.[3:12]))
##Fin del primer bloque de código

head(d1217)		#Para verificar que el output sea el deseado


##CALCULAR LOS DELITOS POR CADA 100 MIL HABITANTES PARA LOS DISTINTOS PERÍODOS

##Primero creamos los subconjuntos para 2012-14 y 2015-17

###2012-2014
##Función 
gto2010 <- 5486372 					#Población en 2010. Se utilizará para el período 2012-14
d100mil <- function(x) (x/gto2010)*100000		#Función para calcular delitos por cada 100 mil habitantes
redondeo <- function(x) round(x,2)			#Función para redondear

d1214 <- filter(d1217, año<2015) %>% 
	mutate(año= factor(año)) %>%			#Con esto evitamos que el mutate_if haga cálculos sobre la variable "año"
	mutate_if(is.numeric, d100mil) %>%
	mutate_if(is.numeric, redondeo)

head(d1214)

###2015-2017
##Función
gto2015 <- 5853677
d_100mil <- function(x) (x/gto2015)*100000

d1517 <- filter(d1217, año%in%c("2015","2016","2017")) %>%
	mutate(año=factor(año))%>%
	mutate_if(is.numeric, d_100mil)%>%
	mutate_if(is.numeric, redondeo) 

###Base de datos consolidada
d1217full <- rbind(d1214,d1517)
head(d1217full)
str(d1217full)

##Exploración de estadísticos
media_anual <- group_by(d1217full,año) %>%
summarise(media=mean(total))
media_anual
		  
media_mensual <- group_by(d1217full,mes) %>%
summarise(media=mean(total))
		  
##Gráfico
d1217full %>%
	select("año", "mes", "narcomenudeo" , "posesion", "total") %>%
    	mutate(order = paste0(as.character(año), "-",sprintf("%02d",which(meses %in% mes)))) %>%
    	gather(variable, valor, -`año`, -mes, -order) %>%
    	ggplot(aes(x=order,y=valor,colour=variable, group=variable)) +
    	geom_line(size=1) +
    	scale_x_discrete(breaks=unique(paste0(as.character(d1217full$año), "-12"))) +
    	theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
	labs(x="", y="")
	
