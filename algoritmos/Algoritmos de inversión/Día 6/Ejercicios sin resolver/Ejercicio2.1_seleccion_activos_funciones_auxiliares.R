# Objetivo ejercicio 2.1: calcula el Alpha de Jensen para cada activo, en cada instante de tiempo.

# Criterio de selección de activos (qué y cuando)
seleccion_activos <- function(datos_descargados, ventana, entrada, salida, percentil_dinamico, fecha_inicio){
  print("Iniciando el proceso de selección de activos")
  
  # Calculamos el Alpha de Jensen
  alpha_actual<-calculamos_alpha(datos_descargados, ventana)
  
  return(alpha_actual)
}

# Calculamos el Alpha de Jensen actual para cada activo.
library(taRifx)
library(dplyr)
library(RollingWindow)

calculamos_alpha<-function(datos_descargados, ventana){
  
  # Los datos descargados están en formato de lista. Recuperamos los que necesitamos como un DF, elimino los factores y los convierto a numéricos.
  cierre<-remove.factors(datos_descargados[[2]])
  fechas<-rownames(cierre)
  cierre<-cierre %>% mutate_all(as.numeric)
    rownames(cierre)<-fechas
  indice<-remove.factors(datos_descargados[[6]])
  indice<-indice %>% mutate_all(as.numeric)
    rownames(indice)<-fechas
  renta_fija<-remove.factors(datos_descargados[[8]])
  renta_fija<-renta_fija %>% mutate_all(as.numeric)
    rownames(renta_fija)<-fechas
  
  # Calculamos la rentabilidad de los activos.
  rent_activos<-XXXXXXXXXXXX
  
  # Rentabilidad del benchmark
  rent_indice <- XXXXXXXXXXXXX

  # Rentabilidad de la renta fija.
  rent_renta_fija<- XXXXXXXXXXXX
  
  # Calculamos la varianza del benchmark 
  varianza_indice <- XXXXXXXXXXXXXX

  # Calculamos la covarianza entre el activo y el benchmark
  covarianza_activos_benchmark<- XXXXXXXXXXXXXXXX
 
  # Sacamos la Beta de cada activo. β=cov(Rc,Rm)/σRm
  beta<- XXXXXXXXXXXXXXXX

  # Calculamos el Alpha de cada activo. α=Rc-(Rf+β(Rm-Rf))
  alpha_activos<- XXXXXXXXXXXXXXX

  return(alpha_activos)
}
