# Importa los datos del fichero DAX.csv y calcula el Alpha de Jensen para todos los activos del índice. 
# Verás que en el mismo archivo encontrarás también los datos del índice (DAX) y de la renta fija (Bund) alemán.

  # α=Rentabilidad_cartera-(Rentabildad_activo_libre_riesgo+ β (Rentabilidad_mercado- Rentabildad_activo_libre_riesgo))
  # β= covarianza (Rentabilidad_cartera, Rentabilidad_mercado) / varianza (Rentabilidad_mercado)

# El alfa de Jensen es una medida de calidad sobre la gestión realizada (ya sea en un fondo, cartera de activos, o una única empresa). 
# Indica el exceso de rentabilidad obtenida para un nivel de riesgo determinado. 
# El Alfa explica la diferencia entre la rentabilidad esperada, es decir, la que corresponde al riesgo sistemático asumido, 
  # y la realmente obtenida por el gestor. En función de que el gestor supere, iguale o esté por debajo 
  # del rendimiento esperado tendrá un Alfa positivo, neutro o negativo.

library(profvis)
profvis({
  
  tiempo <- proc.time()
  # Cargamos los datos y extraemos por un lado los activos, la renta fija y el índice Dax
  datos <- read.csv("DAX.csv", sep=';',stringsAsFactors = FALSE) 
  
  n_activos <- length(datos)-2
  activos<- datos[,2:n_activos] # Extraemos los activos quitando las dos últimas columnas y la primera.
  
  BUND <- datos[,length(datos)] # Extraemos la renta fija.
  DAX <- datos[,length(datos)-1] # Extraemos el índice.
  rango_fechas <- as.Date(datos[,1])
  
  # Calculamos la rentabilidad de los 10 primeros días para usarlos como datos iniciales.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(rent_activos)<-names(activos) # Ponemos nombres a las columnas
  rownames(rent_activos) <- datos[,1] # Ponemos nombres a las filas.
  
  rent_BUND <- rep(0,length(BUND)) # Genero un vector donde guardaré las rentabilidades de la renta fija.
  rent_DAX <- rep(0,length(DAX)) # Genero un vector donde guardaré las rentabilidades del índice.
  
  alpha_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(alpha_activos)<-names(activos) # Ponemos nombres a las columnas
  rownames(alpha_activos) <- datos[,1] # Ponemos nombres a las filas.
  
  for (dia in 2:10){ # Calculamos la rentabilidad de los 10 primeros días (activos, Bund y Dax)
    for (activo in 1:length(activos)){
      rent_activos[dia,activo] <- log(activos[dia,activo]/activos[dia-1,activo])
    }
    rent_BUND[dia] <- log(BUND[dia]/BUND[dia-1])
    rent_DAX[dia] <- log(DAX[dia]/DAX[dia-1])
  }
  
  # Teniendo ya datos históricos, calculamos los Alphas para cada día con un bucle.
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = dim(activos)[1], width=300) 
  for (dia in 11:dim(activos)[1]){
    
    # Calculamos la rentabilidad de los activos, la renta fija y el índice para una fecha concreta.
    for (activo in 1:length(activos)){
      rent_activos[dia,activo] <- log(activos[dia,activo]/activos[dia-1,activo])
    }
    rent_BUND[dia] <- log(BUND[dia]/BUND[dia-1])
    rent_DAX[dia] <- log(DAX[dia]/DAX[dia-1])
    
    # Calculamos la varianza del DAX
    varianza_ind<-var(rent_DAX[2:dia]) # Excluimos la 1ª línea porque contiene ceros. 
    
    for (activo in 1:length(activos)){
      # Calculamos la covarianza entre el activo y el índice
      cov_act_ind <- cov(rent_activos[2:dia,activo],rent_DAX[2:dia])
      
      # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
      if (varianza_ind==0){
        beta <-0
      }else{
        beta <- cov_act_ind/varianza_ind
      }
      
      # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
      alpha_activos[dia,activo]<- rent_activos[dia,activo]-(rent_BUND[dia]+beta*(rent_DAX[dia]-rent_BUND[dia]))
    }
    setWinProgressBar(barra_progreso, dia, title=paste(round(dia/dim(activos)[1]*100,0), "% realizado"))
  }
  close(barra_progreso)
  
  print(proc.time()-tiempo)
})
