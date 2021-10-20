# Importa los datos del fichero DAX.csv y calcula el Ratio de Sharpe para todos los activos del índice. 
# Verás que en el mismo archivo encontrarás también los datos del índice (DAX) y de la renta fija (Bund) alemán.

# Ratio de Sharpe= (Rentabilidad_activo- Rentabildad_activo_libre_riesgo) / raiz(varianza del activo)
  
library(profvis)
profvis({
  
  tiempo <- proc.time()
  
  # Cargamos los datos y extraemos por un lado los activos, la renta fija y el índice Dax
  datos <- read.csv("DAX.csv", sep=';',stringsAsFactors = FALSE) 
  
  n_activos <- length(datos)-2
  activos<- datos[,2:n_activos] # Extraemos los activos quitando las dos últimas columnas y la primera.
  BUND <- datos[,length(datos)] # Extraemos la renta fija.
  rango_fechas <- as.Date(datos[,1])
  
  # Calculamos la rentabilidad de los 10 primeros días para usarlos como datos iniciales.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
    names(rent_activos)<-names(activos) # Ponemos nombres a las columnas
    rownames(rent_activos) <- datos[,1] # Ponemos nombres a las filas.
  
  rent_BUND <- rep(0,length(BUND)) # Genero un vector donde guardaré las rentabilidades de la renta fija.

  sharpe_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
    names(sharpe_activos)<-names(activos) # Ponemos nombres a las columnas
    rownames(sharpe_activos) <- datos[,1] # Ponemos nombres a las filas.
  
  for (dia in 2:10){ # Calculamos la rentabilidad de los 10 primeros días (activos, Bund y Dax)
    for (activo in 1:length(activos)){
      rent_activos[dia,activo] <- log(activos[dia,activo]/activos[dia-1,activo])
    }
    rent_BUND[dia] <- log(BUND[dia]/BUND[dia-1])
  }
  
  # Teniendo ya datos históricos, calculamos el ratio de Sharpe para cada día con un bucle.
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = dim(activos)[1], width=300) 
  for (dia in 11:dim(activos)[1]){
    
    # Calculamos la rentabilidad de los activos y la renta fija para una fecha concreta.
    for (activo in 1:length(activos)){
      rent_activos[dia,activo] <- log(activos[dia,activo]/activos[dia-1,activo])
    }
    rent_BUND[dia] <- log(BUND[dia]/BUND[dia-1])
    
    for (activo in 1:length(activos)){
      
      # Calculamos la volatilidad del activo (desviación típica).
      volatilidad<-sqrt(var(rent_activos[2:dia,activo])) # Excluimos la 1ª línea porque contiene ceros. 
      
      # Calculamos el ratio de Sharpe
      if (!is.nan((rent_activos[dia,activo]-rent_BUND[dia]) / volatilidad)){
        sharpe_activos[dia,activo]<- (rent_activos[dia,activo]-rent_BUND[dia]) / volatilidad
      } 
    }
    setWinProgressBar(barra_progreso, dia, title=paste(round(dia/dim(activos)[1]*100,0), "% realizado"))
  }
  close(barra_progreso)
  
  print(proc.time()-tiempo)
})
