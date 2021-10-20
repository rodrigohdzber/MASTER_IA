# Vectoriza todo menos el cálculo de la volatilidad

library(profvis)
library(roll)

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
  rent_activos[2:dim(activos)[1],1:length(activos)] = log(activos[2:dim(activos)[1],1:length(activos)]/activos[(2:dim(activos)[1])-1,1:length(activos)])
  rent_BUND[2:length(BUND)] = log(BUND[2:length(BUND)]/BUND[(2:length(BUND))-1])
  
  # Calculamos la volatilidad
  fun_vola = function(activo){
    resultado = roll_sd(x=activo, width = 30)
    return(resultado)
  }
  volatilidad = sapply(rent_activos, FUN = fun_vola)
  
  # Calculamos el ratio de Sharpe
  sharpe_activos = (rent_activos - rent_BUND) / volatilidad
  names(sharpe_activos) = names(activos)
  rownames(sharpe_activos) = datos[,1]

  print(proc.time()-tiempo)
})
