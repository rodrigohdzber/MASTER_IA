# Objetivo: Calcula el órden en el que los activos recibirán el capital disponible

library(rvest)
library(pipeR) 
library(httr)
library(stringr)
library(zoo)
library(dplyr)
library(taRifx)
library(RollingWindow) 

# Calculamos el órden en el que los activos recibirían capital disponible día a día.
  ranking_de_asignacion_recursos<-function(datos_descargados, activos_seleccionados, ventana, comision_minima){
    
    cierre<-remove.factors(datos_descargados[[2]])
      fechas<-rownames(cierre)
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
    divisa<-remove.factors(datos_descargados[[7]])
      divisa<-divisa %>% mutate_all(as.numeric)
      rownames(divisa)<-fechas
    precio_objetivo_compra<-remove.factors(activos_seleccionados[[5]])
      precio_objetivo_compra<-precio_objetivo_compra %>% mutate_all(as.numeric)
      rownames(precio_objetivo_compra)<-fechas
    precio_objetivo_venta<-remove.factors(activos_seleccionados[[6]])
      precio_objetivo_venta<-precio_objetivo_venta %>% mutate_all(as.numeric)
      rownames(precio_objetivo_venta)<-fechas
    
    volumen_minimo<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(volumen_minimo)<-colnames(cierre)
      rownames(volumen_minimo)<-rownames(cierre)
    volumen_maximo<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(volumen_maximo)<-colnames(cierre)
      rownames(volumen_maximo)<-rownames(cierre)
    frecuencia_condicion_compra<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(frecuencia_condicion_compra)<-colnames(cierre)
      rownames(frecuencia_condicion_compra)<-rownames(cierre)
    ultima_condicion_compra<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(ultima_condicion_compra)<-colnames(cierre)
      rownames(ultima_condicion_compra)<-rownames(cierre)
    frecuencia_condicion_venta<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(frecuencia_condicion_venta)<-colnames(cierre)
      rownames(frecuencia_condicion_venta)<-rownames(cierre)
    ultima_condicion_venta<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(ultima_condicion_venta)<-colnames(cierre)
      rownames(ultima_condicion_venta)<-rownames(cierre)
    ranking<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(ranking)<-colnames(cierre)
      rownames(ranking)<-rownames(cierre)
    
    for (dia in (dim(cierre)[1]-ventana):1){
      for (activo in 1:dim(cierre)[2]){
        
        # Calculamos el volumen mínimo a comprar (aquel que cubre las comisiones). redondear(comision/(precio_venta - precio_compra))
        volumen_minimo[dia,activo]<- XXXXXXXXXXXXX
        
        # Calculamos el volumen máximo a comprar (umbral de arrastre). 0,5% del volumen medio de las últimas sesiones.
        volumen_maximo[dia,activo]<- XXXXXXXXXXXXX
        
        # Calculamos el porcentaje de veces que se ha cumplido la condición de compra en la ventana temporal.
        frecuencia_condicion_compra[dia,activo]<- XXXXXXXXXXXXX
        
        # Calculamos la última vez que se ha cumplido la condición de compra en la ventana temporal.
        if(frecuencia_condicion_compra[dia,activo]>0){ 
          ultima_condicion_compra[dia,activo]<- XXXXXXXXXXXXX
        }else{
          ultima_condicion_compra[dia,activo]<-ventana*2
        }
        
        # Calculamos el porcentaje de veces que se ha cumplido la condición de venta en la ventana temporal.
        frecuencia_condicion_venta[dia,activo]<- XXXXXXXXXXXXX
        
        # Calculamos la última vez que se ha cumplido la condición de venta en la ventana temporal.
        if(frecuencia_condicion_venta[dia,activo]>0){
          ultima_condicion_venta[dia,activo]<- XXXXXXXXXXXXX
        }else{
          ultima_condicion_venta[dia,activo]<-ventana*2
        }
        
        # Fabricamos un ranking de asignación de recursos en función de la probabilidad de ocurrencia.
        ranking[dia,activo]<-(frecuencia_condicion_venta[dia,activo]/ultima_condicion_venta[dia,activo])*(frecuencia_condicion_compra[dia,activo]/ultima_condicion_compra[dia,activo])
      }
      
      # Transformamos la probabilidad de ocurrencia en un ranking (el activo con mayor puntuación será el primero que recibirá recursos disponibles).
      ranking[dia,]<- XXXXXXXXXXXXX
    }
    
    datos_ranking_asignacion<- list(volumen_minimo, volumen_maximo, frecuencia_condicion_compra, ultima_condicion_compra, frecuencia_condicion_venta, ultima_condicion_venta, ranking)
    
    return(datos_ranking_asignacion)
  }
  