# Objetivo ejercicio 2.1: calcula el Alpha de Jensen para cada activo, en cada instante de tiempo.

library(rvest)
library(pipeR) 
library(stringr)
library(zoo)

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
library(RollingWindow) # install_github("andrewuhl/RollingWindow") # https://github.com/andrewuhl/RollingWindow

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
  rent_activos<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F)) 
    colnames(rent_activos)<-colnames(cierre)
    rownames(rent_activos)<-fechas
  rent_activos[1:(dim(cierre)[1]-1),1:dim(cierre)[2]] <- log(cierre[1:(dim(cierre)[1]-1),1:dim(cierre)[2]]/cierre[2:dim(cierre)[1],1:dim(cierre)[2]])
  
  # Rentabilidad del benchmark
  indice<-indice[1]
  rent_indice <- as.data.frame(rep(0,dim(indice)[1]))
  rownames(rent_indice)<-fechas
  rent_indice[1:(dim(indice)[1]-1),1] <- log(indice[1:(dim(indice)[1]-1),1]/indice[2:dim(indice)[1],1])
  
  # Rentabilidad de la renta fija.
  rent_renta_fija<-renta_fija/100 # El dato descargado del eonia ya es una rentabilidad, pero está en tanto por cien. Lo ajustamos.
  rownames(rent_renta_fija)<-fechas
  
  # Calculamos la varianza del benchmark 
  rent_indice<-rent_indice[nrow(rent_indice):1,] # Invertimos los datos para poder usar RollingVar sin perder los datos más recientes. 
  
  varianza_indice <-rep(0,dim(indice)[1]) 
  varianza_indice<-RollingVar(rent_indice,window = ventana) # Genera NA
  varianza_indice<-rev(varianza_indice) # Doy la vuelta al vector. Tengo NA al final del vector
  varianza_indice<-as.data.frame(na.locf(varianza_indice,na.rm = T)) # na.locf localiza los NA del DF y los sustituye por el valor de la fila anterior.
  colnames(varianza_indice)<-"Varianza indice"
  rownames(varianza_indice)<-fechas
  
  # Calculamos la covarianza entre el activo y el benchmark
  rent_activos<-rent_activos[nrow(rent_activos):1,] # Invertimos el orden de la matriz para poder usar RollingCov
  
  covarianza_activos_benchmark<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
  colnames(covarianza_activos_benchmark)<-colnames(cierre)
  
  for (activo in 1:dim(cierre)[2]){ # RollingCov solo acepta vectores, por lo que tenemos que recorrer los activos con un for,
    covarianza_activos_benchmark[,activo] <- RollingCov(rent_indice, rent_activos[,activo], window = ventana) 
  }
  
  rent_indice<-as.data.frame(rev(rent_indice)) # Pongo del derecho la información.
  colnames(rent_indice)<-"rent indice"
  rownames(rent_indice)<-fechas
  rent_activos<-rent_activos[nrow(rent_activos):1,]# Devolvemos la matriz a su posición.
  
  covarianza_activos_benchmark<-covarianza_activos_benchmark[nrow(covarianza_activos_benchmark):1,] # Ponemos del derecho la matriz
  covarianza_activos_benchmark<-na.locf(covarianza_activos_benchmark,na.rm = T) # Elimino los NA
  rownames(covarianza_activos_benchmark)<-fechas
  
  # Sacamos la Beta de cada activo. β=cov(Rc,Rm)/σRm
  beta<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F)) 
  colnames(beta)<-colnames(cierre)
  rownames(beta)<-fechas
  beta[1:dim(beta)[1],1:dim(beta)[2]] <- covarianza_activos_benchmark[1:dim(covarianza_activos_benchmark)[1],1:dim(covarianza_activos_benchmark)[2]]/varianza_indice[1:dim(rent_indice)[1],1]
  
  # Calculamos el Alpha de cada activo. α=Rc-(Rf+β(Rm-Rf))
  alpha_activos<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F)) 
  colnames(alpha_activos)<-colnames(cierre)
  rownames(alpha_activos)<-fechas
  
  alpha_activos[1:dim(alpha_activos)[1],1:dim(alpha_activos)[2]]<- rent_activos[1:dim(rent_activos)[1],1:dim(rent_activos)[2]]-(rent_renta_fija[1:dim(rent_renta_fija)[1],1]+beta[1:dim(beta)[1],1:dim(beta)[2]]*(rent_indice[1:dim(rent_indice)[1],1]-rent_renta_fija[1:dim(rent_renta_fija)[1],1]))
  
  return(alpha_activos)
}
