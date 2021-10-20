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
  rent_activos[2:dim(activos)[1],1:length(activos)] <- log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])
  rent_BUND[2:dim(activos)[1]] <- log(BUND[2:dim(activos)[1]]/BUND[2:dim(activos)[1]-1])

  # Calculamos la volatilidad
  for (activo in 1:dim(activos)[2]){ # RollingCov solo acepta vectores, por lo que tenemos que recorrer los activos con un for,
    volatilidad[,activo] <- sqrt(RollingVar(rent_activos[,activo], window = 10))
  }

  volatilidad[is.na(as.data.frame(volatilidad))]<-0
  
  # Calculamos el ratio de Sharpe
  sharpe_activos<- (rent_activos-rent_BUND) / volatilidad
    sharpe_activos[is.na(sharpe_activos)]<-0
    sharpe_activos[is.infinite(as.matrix(sharpe_activos))]<-0
  
  print(proc.time()-tiempo)
})
