# Vectoriza todo menos el cálculo de la volatilidad

library(profvis)
library(RollingWindow) 
options(warn=-1)

profvis({
  tiempo <- proc.time()
  
  # Cargamos los datos y extraemos por un lado los activos, la renta fija y el índice Dax
  datos <- read.csv("DAX.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-2
  activos<- datos[,2:n_activos] # Extraemos los activos quitando las dos últimas columnas y la primera.
  BUND <- datos[,length(datos)] # Extraemos la renta fija.

  # Generamos las matrices de datos donde guardaremos los resultados.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  volatilidad<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  rent_BUND <- rep(0,length(BUND)) 
  sharpe_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
    names(sharpe_activos)<-names(activos) 
    rownames(sharpe_activos) <- datos[,1] 
  
  # Calculamos la rentabilidad de los activos y renta fija.
  rent_activos <- XXXXXXXXXXXXXXXXX
  rent_BUND <- XXXXXXXXXXXXXXX

  # Calculamos la volatilidad
  volatilidad <- XXXXXXXXXXXXXXXX

  # Calculamos el ratio de Sharpe
  sharpe_activos<- XXXXXXXXXXXXXXXXX
  
  print(proc.time()-tiempo)
})
