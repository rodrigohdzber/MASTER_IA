# Objetivo ejercicio 2.2: calcula el precio objetivo de compra y el precio objetivo de venta
  # Calcula el Alpha de entrada (punto a partir del que compraríamos)
  # Calcula el Alpha de salida (punto a partir del que venderíamos)
  # Calcula el precio objetivo de compra para el periodo siguiente
  # Calcula el precio objetivo de venta para el periodo siguiente

  # α = Rentabilidad_cartera-(Rentabildad_activo_libre_riesgo+ β(Rentabilidad_mercado- Rentabildad_activo_libre_riesgo))
  # β = covarianza (Rentabilidad_cartera, Rentabilidad_mercado) / varianza (Rentabilidad_mercado)

library(rvest)
library(pipeR) 
library(stringr)
library(zoo)

# Criterio de selección de activos (qué y cuando)
seleccion_activos <- function(datos_descargados, ventana, entrada, salida, percentil_dinamico, fecha_inicio){
  print("Iniciando el proceso de selección de activos")
  
  # Calculamos el Alpha de Jensen
  alpha_actual<-calculamos_alpha(datos_descargados, ventana)
  
  # Calculamos los alphas de entrada y salida, así como los precios objetivo de entrada y salida para cada activo, cada día.
  alpha_entrada<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(alpha_entrada)<-colnames(alpha_actual)
    rownames(alpha_entrada)<-rownames(alpha_actual)
  alpha_salida<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(alpha_salida)<-colnames(alpha_actual)
    rownames(alpha_salida)<-rownames(alpha_actual)
  precio_objetivo_compra<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(precio_objetivo_compra)<-colnames(alpha_actual)
    rownames(precio_objetivo_compra)<-rownames(alpha_actual)
  precio_objetivo_venta<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(precio_objetivo_venta)<-colnames(alpha_actual)
    rownames(precio_objetivo_venta)<-rownames(alpha_actual)
  seleccion_activos<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(seleccion_activos)<-colnames(alpha_actual)
    rownames(seleccion_activos)<-rownames(alpha_actual)
  percentil_entrada<-as.data.frame(matrix(entrada,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(percentil_entrada)<-colnames(alpha_actual)
    rownames(percentil_entrada)<-rownames(alpha_actual)
  percentil_salida<-as.data.frame(matrix(salida,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(percentil_salida)<-colnames(alpha_actual)
    rownames(percentil_salida)<-rownames(alpha_actual)
  ventana_dinamica<-as.data.frame(matrix(ventana,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
    colnames(ventana_dinamica)<-colnames(alpha_actual)
    rownames(ventana_dinamica)<-rownames(alpha_actual)
  
  apertura<-remove.factors(datos_descargados[[1]])
  fechas<-rownames(apertura)
  apertura<-apertura %>% mutate_all(as.numeric)
    rownames(apertura)<-fechas
  cierre<-remove.factors(datos_descargados[[2]])
  cierre<-cierre %>% mutate_all(as.numeric)
    rownames(cierre)<-fechas
  maximo<-remove.factors(datos_descargados[[3]])
  maximo<-maximo %>% mutate_all(as.numeric)
    rownames(maximo)<-fechas
  minimo<-remove.factors(datos_descargados[[4]])
  minimo<-minimo %>% mutate_all(as.numeric)
    rownames(minimo)<-fechas
  volumen<-remove.factors(datos_descargados[[5]])
  volumen<-volumen %>% mutate_all(as.numeric)
    rownames(volumen)<-fechas
  indice<-remove.factors(datos_descargados[[6]])
  indice<-indice %>% mutate_all(as.numeric)
    rownames(indice)<-fechas
  divisa<-remove.factors(datos_descargados[[7]])
  divisa<-divisa %>% mutate_all(as.numeric)
    rownames(divisa)<-fechas
  renta_fija<-remove.factors(datos_descargados[[8]])
  renta_fija<-renta_fija %>% mutate_all(as.numeric)
    rownames(renta_fija)<-fechas
  
  for (dia in (dim(alpha_actual)[1]-ventana):1){
    for (activo in 1:dim(alpha_actual)[2]){

      # Calculamos el Alpha de entrada (punto a partir del que compraríamos)
      alpha_entrada[dia,activo]<- XXXXXXXXXXXXXXX

      # Calculamos el Alpha de salida (punto a partir del que venderíamos)
      alpha_salida[dia,activo]<- XXXXXXXXXXXXXXX
      
      # Calculamos el precio objetivo de compra para el periodo siguiente
      precio_objetivo_compra[dia, activo]<-precio_objetivo(alpha_objetivo=alpha_entrada[dia,activo], cotizaciones_activo=cierre[dia:(dia+ventana_dinamica[dia, activo]), activo], cotizaciones_indice=indice[dia:(dia+ventana_dinamica[dia, activo]),1], cotizaciones_renta_fija=renta_fija[dia:(dia+ventana_dinamica[dia, activo]),1])
      
      # Calculamos el precio objetivo de venta para el periodo siguiente
      precio_objetivo_venta[dia, activo]<-precio_objetivo(alpha_objetivo=alpha_salida[dia,activo], cotizaciones_activo=cierre[dia:(dia+ventana_dinamica[dia, activo]), activo], cotizaciones_indice=indice[dia:(dia+ventana_dinamica[dia, activo]),1], cotizaciones_renta_fija=renta_fija[dia:(dia+ventana_dinamica[dia, activo]),1])
    }
  }

  return()
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


# Calculamos el precio que iguala el Alpha de Jensen objetivo α=Rc-(Rf+β(Rm-Rf)) Es decir, obtenemos los precios de compra y venta objetivos
  precio_objetivo<-function(alpha_objetivo, cotizaciones_activo, cotizaciones_indice, cotizaciones_renta_fija){
    
    # Calculamos la rentabilidad de los activos, su benchmark y el eonia.
    rent_activos <- log(cotizaciones_activo[1:(length(cotizaciones_activo)-1)]/cotizaciones_activo[2:length(cotizaciones_activo)])
    rent_activos<-c(rent_activos,rent_activos[length(rent_activos)]) # Igualamos el tamaño repitiendo el último dato.
    
    rent_benchmark <- log(cotizaciones_indice[1:(length(cotizaciones_indice)-1)]/cotizaciones_indice[2:length(cotizaciones_indice)])
    rent_benchmark<-c(rent_benchmark,rent_benchmark[length(rent_benchmark)])
    
    # Calculamos la varianza del benchmark
    varianza_benchmark<-var(rent_benchmark)
    
    # Calculamos la covarianza entre el activo y el benchmark
    covarianza_activos_benchmark <- cov(rent_benchmark, rent_activos) 
    
    # Sacamos la Beta de cada activo. β=cov(Rc,Rm)/σRm
    beta <- covarianza_activos_benchmark / varianza_benchmark
    
    # ¿Y ahora qué hacemos????
    XXXXXXXXXXXXXXXXXXXXXXXXXXX
    
    return(precio_objetivo)
  }  
