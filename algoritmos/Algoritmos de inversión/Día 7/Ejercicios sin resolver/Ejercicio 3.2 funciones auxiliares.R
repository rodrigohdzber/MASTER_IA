# Objetivo: Genera la recomendación para mañana

library(rvest)
library(pipeR) 
library(httr)
library(stringr)
library(zoo)
library(dplyr)
library(taRifx)
library(RollingWindow) 
library(xlsx)

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
        volumen_minimo[dia,activo]<-floor(comision_minima*2/(precio_objetivo_venta[dia,activo]/divisa[dia,1]-precio_objetivo_compra[dia,activo]/divisa[dia,1]))+1
        
        # Calculamos el volumen máximo a comprar (umbral de arrastre). 0,5% del volumen medio de las últimas sesiones.
        volumen_maximo[dia,activo]<-floor(mean(volumen[dia:(dia+ventana),activo])*0.005)
        
        # Calculamos el porcentaje de veces que se ha cumplido la condición de compra en la ventana temporal.
        frecuencia_condicion_compra[dia,activo]<-sum(minimo[dia:(dia+ventana),activo]<=precio_objetivo_compra[dia,activo])/(ventana+1)
        
        # Calculamos la última vez que se ha cumplido la condición de compra en la ventana temporal.
        if(frecuencia_condicion_compra[dia,activo]>0){ 
          ultima_condicion_compra[dia,activo]<-min(which((minimo[dia:(dia+ventana),activo]<=precio_objetivo_compra[dia,activo]) == TRUE))
        }else{
          ultima_condicion_compra[dia,activo]<-ventana*2
        }
        
        # Calculamos el porcentaje de veces que se ha cumplido la condición de venta en la ventana temporal.
        frecuencia_condicion_venta[dia,activo]<-sum(maximo[dia:(dia+ventana),activo]>=precio_objetivo_venta[dia,activo])/(ventana+1)
        
        # Calculamos la última vez que se ha cumplido la condición de venta en la ventana temporal.
        if(frecuencia_condicion_venta[dia,activo]>0){
          ultima_condicion_venta[dia,activo]<-min(which((maximo[dia:(dia+ventana),activo]>=precio_objetivo_venta[dia,activo]) == TRUE))
        }else{
          ultima_condicion_venta[dia,activo]<-ventana*2
        }
        
        # Fabricamos un ranking de asignación de recursos en función de la probabilidad de ocurrencia.
        ranking[dia,activo]<-(frecuencia_condicion_venta[dia,activo]/ultima_condicion_venta[dia,activo])*(frecuencia_condicion_compra[dia,activo]/ultima_condicion_compra[dia,activo])
      }
      
      # Transformamos la probabilidad de ocurrencia en un ranking (el activo conmayor puntuación será el primero que recibirá recursos disponibles).
      ranking[dia,]<-rank(ranking[dia,])
    }
    
    datos_ranking_asignacion<- list(volumen_minimo, volumen_maximo, frecuencia_condicion_compra, ultima_condicion_compra, frecuencia_condicion_venta, ultima_condicion_venta, ranking)
    
    return(datos_ranking_asignacion)
  }
  
  
# Generamos la recomendación para el día siguiente
  generar_recomendacion<-function(datos_descargados, activos_seleccionados, datos_ranking_asignacion, beneficio_objetivo_por_operacion, stop_loss, comision, comision_minima){
    
    print("Generando la recomendación para mañana")
    
    cierre<-remove.factors(datos_descargados[[2]])
      cierre<-cierre %>% mutate_all(as.numeric)
    volumen<-remove.factors(datos_descargados[[5]])
      volumen<-volumen %>% mutate_all(as.numeric)
    divisa<-remove.factors(datos_descargados[[7]])
      divisa<-divisa %>% mutate_all(as.numeric)
    precio_objetivo_compra<-remove.factors(activos_seleccionados[[5]])
      precio_objetivo_compra<-precio_objetivo_compra %>% mutate_all(as.numeric)
    precio_objetivo_venta<-remove.factors(activos_seleccionados[[6]])
      precio_objetivo_venta<-precio_objetivo_venta %>% mutate_all(as.numeric)
    volumen_minimo<-remove.factors(datos_ranking_asignacion[[1]])
      volumen_minimo<-volumen_minimo %>% mutate_all(as.numeric)
    volumen_maximo<-remove.factors(datos_ranking_asignacion[[2]])
      volumen_maximo<-volumen_maximo %>% mutate_all(as.numeric)
    frecuencia_condicion_compra<-remove.factors(datos_ranking_asignacion[[3]])
      frecuencia_condicion_compra<-frecuencia_condicion_compra %>% mutate_all(as.numeric)
    frecuencia_condicion_venta<-remove.factors(datos_ranking_asignacion[[5]])
      frecuencia_condicion_venta<-frecuencia_condicion_venta %>% mutate_all(as.numeric)
    ranking<-remove.factors(datos_ranking_asignacion[[7]])
      ranking<-ranking %>% mutate_all(as.numeric)
    
    recomendacion_mañana<-as.data.frame(matrix(0,nrow=15,ncol=dim(cierre)[2],byrow=F))
      recomendacion_mañana[1,]<- XXXXXXXXXXXXX # Nombre de los activos.
      recomendacion_mañana[2,]<- XXXXXXXXXXXXX # Precio de cierre de ayer.
      recomendacion_mañana[3,]<- XXXXXXXXXXXXX # Precio objetivo de compra.
      recomendacion_mañana[4,]<- XXXXXXXXXXXXX # Precio objetivo de venta.
      recomendacion_mañana[5,]<- XXXXXXXXXXXXX # Horquilla.
      recomendacion_mañana[6,]<- XXXXXXXXXXXXX # Rentabilidad esperada
      recomendacion_mañana[7,]<- XXXXXXXXXXXXX # Probabilidad de ocurrencia
      recomendacion_mañana[8,]<- XXXXXXXXXXXXX # Stop loss
      recomendacion_mañana[9,]<- XXXXXXXXXXXXX # Beneficio objetivo por operación
    
    for (accion in 1:dim(recomendacion_mañana)[2]){
      
      # Bº=(pv-pc)*nacc-com
        # 100=(12-11)*nacc-(0,008*nacc*12)-(0,008*nacc*11)
        # 100=1nacc-0,096nacc-0,088nacc
        # 100=1nacc-0,096nacc-0,088nacc
        # 100=0,816nacc
        # nacc=100/0,816 --> 122,54 acciones a comprar
      
      if ((precio_objetivo_venta[1,accion]/divisa[1,1]-precio_objetivo_compra[1,accion]/divisa[1,1]-comision*precio_objetivo_venta[1,accion]/divisa[1,1]-comision*precio_objetivo_compra[1,accion]/divisa[1,1])>0){
        
        recomendacion_mañana[10,accion]<- XXXXXXXXXXXXX # Num de acciones
        recomendacion_mañana[11,accion]<- XXXXXXXXXXXXX # Comisiones
        
        # Comprobamos que la comisión es mayor que la comisión mínima.
        if (as.numeric(as.character(recomendacion_mañana[11,accion]))<comision_minima*2){
          
          recomendacion_mañana[10,accion]<-round((beneficio_objetivo_por_operacion+comision_minima*2)/(precio_objetivo_venta[1,accion]/divisa[1,1]-precio_objetivo_compra[1,accion]/divisa[1,1]),digits = 0)
          recomendacion_mañana[11,accion]<-comision_minima*2
        }
        
      }else{ # Las comisiones son superiores a los beneficios. No compensa hacer la operación.
        recomendacion_mañana[10,accion]<-0
        recomendacion_mañana[11,accion]<-0
      }
    }
    
    recomendacion_mañana[12,]<- XXXXXXXXXXXXX # Capital invertido
    recomendacion_mañana[13,]<- XXXXXXXXXXXXX # % sobre volumen diario
    recomendacion_mañana[14,]<- XXXXXXXXXXXXX # Volumen mínimo
    recomendacion_mañana[15,]<- XXXXXXXXXXXXX # Volumen máximo
    
    # Ordenamos las columnas por su probabilidad de asignación de capital.
    indice<-order(ranking[1,],decreasing = T)
    recomendacion_mañana<-recomendacion_mañana[,indice]
    
    recomendacion_mañana<-as.data.frame(t(recomendacion_mañana))
    colnames(recomendacion_mañana)<-c("Activo", "Ultimo cierre (en div)", "Precio obj compra (en div)", "Precio obj venta (en div)", "Horquilla", "Rentabilidad esperada", "Probabilidad ocurrencia",
                                      "Stop loss (en div)", "Bº obj por operación (en eur)", "Nº de acc a comprar", "Comisiones (en eur)", "Capital invertido (en eur)", "% sobre volumen diario", "Vol mínimo comprar (nº acc)", "Vol máximo (nº acc)")
    
    for (columna in 2:dim(recomendacion_mañana)[2]){
      recomendacion_mañana[,columna] <- round(as.numeric(as.character(recomendacion_mañana[,columna])), digits=3)
    }
    
    write.xlsx(recomendacion_mañana, "recomendacion_mañana.xlsx")
    
    return(recomendacion_mañana)
  }

