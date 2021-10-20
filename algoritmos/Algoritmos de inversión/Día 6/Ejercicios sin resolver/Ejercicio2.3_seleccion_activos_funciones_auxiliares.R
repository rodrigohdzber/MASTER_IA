# Objetivo ejercicio 2.3: haz dinámicos los percentiles de entrada y salida.

  # Tanto para la compra, como para la venta:
    # Si incremento el percentil, incremento el precio (subo la banda)
    # Si reduzco el percentil, bajo el precio (bajo la banda)
  
  # Queremos saltar entre configuraciones en función del número de días que la banda no ha sido tocada por el máximo o mínimo de la vela.
  # Debemos recordar la configuración del día anterior para cada activo. Empezamos en 15% - 85%

    # Si banda de compra, en la recomendación anterior:
      # No toca la vela y el min está por encima, incrementamos la configuración de compra de ayer.
      #	No toca la vela y el min está por debajo, reducimos la configuración de compra de ayer.
      #	Sí toca la vela, pero la banda de venta no la toca, reducimos la configuración de compra.
      #	Sí toca la vela y la banda de venta también, volvemos un paso a la configuración inicial de compra (15%).
  
    #	Si la banda de venta, en la recomendación anterior:
      # No toca la vela y el máximo está por debajo, reducimos la configuración de venta.
      # No toca la vela y el máximo está por encima, incrementamos la configuración de venta.
      # Sí toca la vela, pero la banda de compra no la toca, incrementamos la configuración de venta.
      # Sí toca la vela y la banda de compra también, volvemos un paso a la configuración de venta de (85%).

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
      
      if (percentil_dinamico==T){
        if(dia != dim(alpha_actual)[1]-ventana){
          
          # Consultamos si hemos tocado la vela el día anterior y variamos los percentiles de entrada y salida en función de ello. 
          percentil_entrada[dia, activo]<-percentil_entrada_dinamico(precio_objetivo_compra[dia+1,activo], precio_objetivo_venta[dia+1,activo], maximo[dia,activo], minimo[dia,activo], percentil_entrada[dia+1,activo])
          percentil_salida[dia, activo]<-percentil_salida_dinamico(precio_objetivo_compra[dia+1,activo], precio_objetivo_venta[dia+1,activo], maximo[dia,activo], minimo[dia,activo], percentil_salida[dia+1,activo])
          
          # Cambiamos la ventana en función únicamente del POC (lo que queremos evitar es que el POC esté siempre por debajo de las siguientes velas en una fuerte subida).
          if(percentil_entrada[dia, activo]>0.15){
            if(percentil_entrada[dia, activo]>percentil_entrada[dia+1, activo]){
              
              # Nos estamos alejando del percentil estandar, acortamos la ventana
              ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]/1.35,0)
              
            }else if (percentil_entrada[dia, activo]<percentil_entrada[dia+1, activo]){
              
              # Regresamos al percentil estandar, ampliamos la ventana
              ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]*1.35,0)
              
              if (ventana_dinamica[dia, activo]>ventana){
                ventana_dinamica[dia, activo]<-ventana
              }
              
            }else{
              
              # Mantenemos la ventana anterior
              ventana_dinamica[dia, activo]<-ventana_dinamica[dia+1, activo]
            }
            
          }else if(percentil_entrada[dia, activo]<0.15){
            
            if(percentil_entrada[dia, activo]<percentil_entrada[dia+1, activo]){
              
              # Nos estamos alejando del percentil estandar, acortamos la ventana
              ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]/1.35,0)
              
            }else if(percentil_entrada[dia, activo]>percentil_entrada[dia+1, activo]){
              
              # Regresamos al percentil estandar, ampliamos la ventana
              ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]*1.35,0)
              
              if (ventana_dinamica[dia, activo]>ventana){
                ventana_dinamica[dia, activo]<-ventana
              }
              
            }else{
              
              # Mantenemos la ventana anterior
              ventana_dinamica[dia, activo]<-ventana_dinamica[dia+1, activo]
            }
            
          }else{
            
            # Estamos en el percentil estandar, regresamos a la normalidad
            ventana_dinamica[dia, activo]<-ventana
          }
        }
      }
      
      # Comprobamos que el tamaño de la ventana dinámica no baja nunca de un umbral mínimo.
      if(ventana_dinamica[dia, activo]<5){
        ventana_dinamica[dia, activo]<-5
      }
      
      # Calculamos el Alpha de entrada (punto a partir del que compraríamos)
      alpha_entrada[dia,activo]<-quantile(alpha_actual[dia:(dia+ventana_dinamica[dia, activo]),activo], probs = c(percentil_entrada[dia, activo]))
      
      # Calculamos el Alpha de salida (punto a partir del que venderíamos)
      alpha_salida[dia,activo]<-quantile(alpha_actual[dia:(dia+ventana_dinamica[dia, activo]),activo], probs = c(percentil_salida[dia, activo]))
      
      # Calculamos el precio objetivo de compra para el periodo siguiente
      precio_objetivo_compra[dia, activo]<-precio_objetivo(alpha_objetivo=alpha_entrada[dia,activo], cotizaciones_activo=cierre[dia:(dia+ventana_dinamica[dia, activo]), activo], cotizaciones_indice=indice[dia:(dia+ventana_dinamica[dia, activo]),1], cotizaciones_renta_fija=renta_fija[dia:(dia+ventana_dinamica[dia, activo]),1])
      
      # Calculamos el precio objetivo de venta para el periodo siguiente
      precio_objetivo_venta[dia, activo]<-precio_objetivo(alpha_objetivo=alpha_salida[dia,activo], cotizaciones_activo=cierre[dia:(dia+ventana_dinamica[dia, activo]), activo], cotizaciones_indice=indice[dia:(dia+ventana_dinamica[dia, activo]),1], cotizaciones_renta_fija=renta_fija[dia:(dia+ventana_dinamica[dia, activo]),1])
    }
  }
  
  # Acortamos el tamaño de los DF para ajustarlos a la fecha_inicio y fecha_fin   
  fechas<-rownames(cierre)
  indice_booleano<-as.Date(fechas)>=as.Date(fecha_inicio, "%d/%m/%Y") 
  percentil_entrada<-percentil_entrada[indice_booleano,] 
  percentil_salida<-percentil_salida[indice_booleano,] 
  alpha_entrada<-alpha_entrada[indice_booleano,] 
  alpha_salida<-alpha_salida[indice_booleano,] 
  precio_objetivo_compra<-precio_objetivo_compra[indice_booleano,] 
  precio_objetivo_venta<-precio_objetivo_venta[indice_booleano,] 
  ventana_dinamica<-ventana_dinamica[indice_booleano,] 
  
  activos_seleccionados<- list(percentil_entrada, percentil_salida, alpha_entrada, alpha_salida, precio_objetivo_compra, precio_objetivo_venta, ventana_dinamica)
  
  # Acortamos el tamaño de los DF de la lista de datos_descargados para hacer cálculos coherentes en el resto del código.
  apertura<-apertura[indice_booleano,]
  datos_descargados[[1]]<<-apertura
  cierre<-cierre[indice_booleano,]
  datos_descargados[[2]]<<-cierre
  maximo<-maximo[indice_booleano,]
  datos_descargados[[3]]<<-maximo
  minimo<-minimo[indice_booleano,]
  datos_descargados[[4]]<<-minimo
  volumen<-volumen[indice_booleano,]
  datos_descargados[[5]]<<-volumen
  indice<-indice[indice_booleano,]
  datos_descargados[[6]]<<-indice
  divisa<-divisa[indice_booleano,]
  datos_descargados[[7]]<<-divisa
  renta_fija<-renta_fija[indice_booleano,]
  datos_descargados[[8]]<<-renta_fija
  
  return(activos_seleccionados)
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
    
    # Calculo la rentabilidad esperada RE= RF+Beta(RM-RF)
    rent_esperada<-cotizaciones_renta_fija[1]+beta*(rent_benchmark[1]-cotizaciones_renta_fija[1])
    
    # Calculo la rentabilidad del precio de compra o venta. RPC = RE + Alpha entrada o RPV = RE + Alpha salida.
    rent_precio<-alpha_objetivo+rent_esperada
    
    # Calculo el precio objetivo de compra o venta. Precio objetivo = Precio hoy * exp(Rent PC)
    precio_objetivo<-cotizaciones_activo[1]*exp(rent_precio)
    
    return(precio_objetivo)
  }  


# Calculo la variación del percentil dinámico de entrada, en función de la recomendación de ayer y los precios de hoy. 
  percentil_entrada_dinamico<-function(precio_objetivo_compra, precio_objetivo_venta, maximo, minimo, entrada){
    
    # Si incremento el percentil, incremento el precio (subo la banda)
    # Si reduzco el percentil, bajo el precio (bajo la banda)
    
    # Si la el precio de compra, recomendado para ayer, comparándolo con los precios de hoy:
    #	No toca la vela y el mínimo está por encima, incrementamos la configuración de compra de ayer.
    #	No toca la vela y el máximo está por debajo, reducimos la configuración de compra de ayer.
    #	Si toca la vela pero la banda de venta no la toca, reducimos la configuración de compra.
    #	Si toca la vela y la banda de venta la toca, volvemos un paso a la configuración inicial de compra (15%).
    
    vector_percentiles_entrada<-c(0.0381, 0.0424, 0.0471, 0.0523, 0.0581, 0.0646, 0.0717, 0.0797, 0.0886, 0.0984, 0.1094,
                                  0.1215, 0.1350, 0.1500, 0.1650, 0.1815, 0.1997, 0.2196, 0.2416, 0.2657, 0.2923, 0.3215,
                                  0.3537, 0.3891, 0.4280, 0.4708) # Usamos un vector de configuraciones xq los incrementos % son diferentes al ir en una direcicón y volver.
    
    # Comprobamos la posición del percentil dentro del vector.
    posicion<- XXXXXXXXXXXXXX
    
    # Comprobamos si el precio recomendado ha tocado la vela.
    if(XXXXXXXXXXXXXX){
      
      # Comprobamos si la banda de venta toca la vela.
      if(XXXXXXXXXXXXXXXXX){
        
        #	Si toca la vela y la banda de venta la toca, volvemos un paso a la configuración inicial de compra (15%).
        if (posicion > 14){ 
          
          entrada<- XXXXXXXXXXXXXXX
          
        } else if (posicion < 14){ 
          
          entrada<- XXXXXXXXXXXXXX
        }
        
      } else {
        
        # Si toca la vela y la banda de venta no la toca, reducimos la configuración de compra.
        if (posicion > 1){
          entrada<- XXXXXXXXXXXXXXX
        }
      }
      
    } else if(precio_objetivo_compra < minimo & posicion < length(vector_percentiles_entrada)){
      
      # No toca la vela y el mínimo está por encima, incrementamos la configuración de compra de ayer.
      entrada<- XXXXXXXXXXXXXX
      
    } else if(precio_objetivo_compra > maximo & posicion > 1){
      
      #	No toca la vela y el máximo está por debajo, reducimos la configuración de compra de ayer.
      entrada<- XXXXXXXXXXXXXXX
    }
    
    return(entrada)
  }
  
  
# Calculo la variación del percentil dinámico de entrada, en función de la recomendación de ayer y los precios de hoy. 
  percentil_salida_dinamico<-function(precio_objetivo_compra, precio_objetivo_venta, maximo, minimo, salida){
    
    # Si incremento el percentil, incremento el precio (subo la banda)
    # Si reduzco el percentil, bajo el precio (bajo la banda)
    
    #	Si la banda de venta, en la recomendación anterior:
    #	No toca la vela y el máximo está por debajo, reducimos la configuración de venta.
    #	No toca la vela y el mínimo está por encima, incrementamos la configuración de venta.
    #	Si toca la vela pero la banda de compra no la toca, incrementamos la configuración de venta.
    #	Si toca la vela y la banda de compra la toca, volvemos un paso a la configuración de venta de (85%).
    
    vector_percentiles_salida<-c(0.5909, 0.6029, 0.6152, 0.6278, 0.6406, 0.6537, 0.6670, 0.6806, 0.6945, 0.7087, 0.7231,
                                 0.7379, 0.7530, 0.7683, 0.7840, 0.8000, 0.8163, 0.8330, 0.8500, 0.8670, 0.8843, 0.9020,
                                 0.9201, 0.9385, 0.9572, 0.9764) # Usamos un vector de configuraciones xq los incrementos % son diferentes al ir en una direcicón y volver.
    
    # Comprobamos la posición del percentil dentro del vector.
    posicion<- XXXXXXXXXXXXXX
    
    # Comprobamos si el precio recomendado ha tocado la vela.
    if(XXXXXXXXXXXXXXXXXX){
      
      # Comprobamos si la banda de compra toca la vela.
      if(XXXXXXXXXXXXXXXXXXX){
        
        #	Si toca la vela y la banda de compra la toca, volvemos un paso a la configuración de venta (85%).
        if (posicion > 19){ 
          
          salida<- XXXXXXXXXXXXX
          
        } else if (posicion < 19){ 
          
          salida<- XXXXXXXXXXXXXXX
        }
        
      } else {
        
        #	Si toca la vela pero la banda de compra no la toca, incrementamos la configuración de venta.
        if (posicion < length(vector_percentiles_salida))
          salida<- XXXXXXXXXXXXXXX
      }
      
    } else if(precio_objetivo_venta < minimo & posicion < length(vector_percentiles_salida)){
      
      #	No toca la vela y el mínimo está por encima, incrementamos la configuración de venta.
      salida<- XXXXXXXXXXXXXXX
      
    } else if(precio_objetivo_venta > maximo & posicion > 1){
      
      #	No toca la vela y el máximo está por debajo, reducimos la configuración de venta.
      salida<- XXXXXXXXXXXXXXX
    }
    
    return(salida)
  }
  