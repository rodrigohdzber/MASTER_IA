# Calculamos el precio que iguala el Alpha de Jensen objetivo α=Rc-(Rf+β(Rm-Rf)) Es decir, obtenemos los precios de compra y venta objetivos
precio_objetivo<-function(alpha_objetivo, cotizaciones_activo, cotizaciones_indice, cotizaciones_renta_fija){
  
  # Teniendo un alpha objetivo, modificamos el precio de hoy para ver con qué precio compraríamos o venderíamos.
  # Hay que calcular la primera variación. El precio nunca puede ser negativo, por lo que un activo que valga menos de 1, no puede tener una variación inicial de 1 €. 
  # Lo mejor es usar un % del valor de la acción. 
  
  variacion <- cotizaciones_activo[1]*0.05
  num_cambios_sentido <-0
  diferencia_anterior <- 0
  control_de_iteraciones <-0
  
  cotizaciones_renta_fija<-cotizaciones_renta_fija/100 # Ponemos la rentabilidad en porcentaje.
  
  infinito<-F
  precio_activo_inicial<-cotizaciones_activo[1]
  
  while (num_cambios_sentido < 9) { 
    
    if (diferencia_anterior>0){
      cotizaciones_activo[1]<-cotizaciones_activo[1]+variacion 
    }else if(diferencia_anterior<0){
      cotizaciones_activo[1]<-cotizaciones_activo[1]-variacion
      
      if (cotizaciones_activo[1]<=0){ # La modificación del precio lo ha llevado a tener un valor negativo (lo que es imposible).
        print("El precio del activo no puede ser negativo")
        infinito<-T
        cotizaciones_activo[1]<-precio_activo_inicial
        break
      }
      
    }else{
      # No variamos el precio
    }
    
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
    
    # Calculamos el Alpha. α=Rc-(Rf+β(Rm-Rf))
    alpha_calculado<- rent_activos[1]-(cotizaciones_renta_fija[1]+beta*(rent_benchmark[1]-cotizaciones_renta_fija[1]))
    
    # Comparamos el Alpha calculado con el Alpha de entrada
    diferencia<-alpha_objetivo-alpha_calculado
    
    # Vemos si cambiamos el signo de la variación
    if ((diferencia > 0 & diferencia_anterior < 0) | (diferencia < 0 & diferencia_anterior > 0)){
      variacion <- variacion/2
      num_cambios_sentido <- num_cambios_sentido +1
    }
    
    # Modificamos la diferencia anterior.
    diferencia_anterior <- diferencia
    
    control_de_iteraciones<-control_de_iteraciones+1
    if (control_de_iteraciones>1000){
      # Hemos entrado en un bucle infinito. Nos salimos.
      print("Bucle infinito intentando calcular el precio objetivo")
      infinito<-T
      cotizaciones_activo[1]<-precio_activo_inicial
      break
    }
  }
  
  if (infinito==T){ # El sistema normal de cálculo de precio objetivo ha fallado. Invocamos al sistema alternativo. 
    cotizaciones_activo[1]<-calculo_alternativo_precio_objetivo(alpha_objetivo, cotizaciones_activo, cotizaciones_indice, cotizaciones_renta_fija)
  }
  
  return(cotizaciones_activo[1])
}  



# Otra manera de hacerlo (más sencilla)
precio_objetivo<-function(alpha_objetivo, cotizaciones_activo, cotizaciones_indice, cotizaciones_renta_fija){
  paso<-sd(cotizaciones_activo)
  error<-10^(-4)
  i=0
  
  cotizaciones_activo[1]<-cotizaciones_activo[1]
  alpha<-precio_objetivo(alpha_objetivo, cotizaciones_activo, cotizaciones_indice, cotizaciones_renta_fija)
  alpha1<-alpha
  
  if (alpha1>alpha){
    supp<-1
    cotizaciones_activo[1]<-cotizaciones_activo[1]-paso
  }else{
    supp<-0
    cotizaciones_activo[1]<-cotizaciones_activo[1]-paso
  }
  
  while(abs(alpha_objetivo-alpha)>error){
    i=i+1
    alpha<-calculamos_alpha_1d(alpha_objetivo, cotizaciones_activo, cotizaciones_indice, cotizaciones_renta_fija)
    alpha1<-c(alpha1,alpha)
    if (alpha1>alpha){
      supp<-c(supp,1)
      if (supp[i]==0) {
        paso<-paso/2
      }
      cotizaciones_activo[1]<-cotizaciones_activo[1]-paso
      
    }else{
      supp<-c(supp,0)
      if (supp[i]==1) {
        paso<-paso/2
      }
      cotizaciones_activo[1]<-cotizaciones_activo[1]+paso
    }
    
  }
}