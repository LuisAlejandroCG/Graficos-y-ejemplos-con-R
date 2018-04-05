###ENIGH 2016: Estimaciones para el estado de Guanajuato
###La base se descarga de 
###http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/regulares/enigh/nc/2016/microdatos/enigh2016_ns_concentradohogar_csv.zip
###El descriptor de archivos está en http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/regulares/enigh/nc/2016/doc/702825091996.pdf

library(tidyverse)

##Cargar base de datos y hacer ajustes básicos a las variables
##Nota: Indicar ruta propia de usuario 
conc_hogargto <- read.csv("ruta_del_usario/concentradohogar.csv", header=T)%>%
rename(folioviv= "ï..folioviv")%>% 
filter(str_detect(folioviv, "^11")) %>%
select("folioviv", "tam_loc", "est_socio", "est_dis", "upm", 
"factor", "sexo_jefe", "educa_jefe", "tot_integ", "percep_ing", 
"perc_ocupa", "ing_cor", "becas", "remesas", "bene_gob", "gasto_mon", 
"alimentos") %>%
mutate(urb_rural=substr(folioviv, 3,3)) %>%	##El tercer caracter de la cadena define rural-urbano
mutate(urb_rural=ifelse(urb_rural==6, "rural", "urbano"), 
tam_loc=ifelse(tam_loc==1, "Más de 100 mil habs.", ifelse(tam_loc==2, "De 15 a 99999 habs.", 
ifelse(tam_loc==3, "2500 a 14999 habs.", "Menor a 2500 habs."))), 
est_socio=ifelse(est_socio==1, "Bajo", ifelse(est_socio==2, "Medio bajo", 
ifelse(est_socio==3, "Medio alto", "Alto"))), 
sexo_jefe=(ifelse(sexo_jefe==1, "Hombre", "Mujer")))


##Análisis de encuesta utilizando survey
library(survey)

##Por tamaño de localidad
enighgtoloc <- svydesign(ids=~est_dis+upm, strata=conc_hogargto$tam_loc, data = conc_hogargto, weights = conc_hogargto$factor)
summary(enighgtoloc)

##Crear variable "Razón de ingreso por remesas"
enighgtoloc <- update(enighgtoloc, razoningrem = (remesas/ing_cor)*100)
##Crear variable "Razón de ingreso por becas y beneficios gubernamentales
enighgtoloc <- update(enighgtoloc, razoningbecben = ((becas+bene_gob)/ing_cor)*100)

##INGRESOS TRIMESTRALES POR REMESAS SEGÚN TAMAÑO DE LOCALIDAD
remesa <- subset(enighgtoloc, remesas>0)	##Creamos un subconjunto de variables que tengan valores de ingresos por remesas > 0

##Ingresos absolutos por remesas
svyby(~remesas,~tam_loc,design=remesa,svymean)
svyby(~remesas,~tam_loc+sexo_jefe,design=remesa,svymean)

##Ingresos relativos por remesas (respecto al total de ingresos)
svyby(~razoningrem,~tam_loc,design=remesa,svymean)
svyby(~razoningrem,~tam_loc+sexo_jefe,design=remesa,svymean)

##---Regresión lineal---
summary(svyglm(remesas~tam_loc+0, design=remesa))
summary(svyglm(razoningrem~tam_loc+0, design=remesa))


##Por ámbito-rural urbano
##Pendiente
enighgtourb <- svydesign(ids=~est_dis+upm, strata=conc_hogargto$urb_rural, data = conc_hogargto, weights = conc_hogargto$factor)
summary(enighgtourb)
