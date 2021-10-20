# Objetivo: Vectoriza todo el programa.

library(profvis)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  window_width=30
  
  # Cargamos los datos y extraemos por un lado los activos, la renta fija y el índice Dax
  datos <- read.csv("DAX.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-1
  activos<- datos[,2:n_activos] # Extraemos los activos quitando la última columnasy la primera.
  BUND <- datos[,length(datos)] # Extraemos la renta fija.
  DAX <- datos[,length(datos)-1] # Extraemos el índice.
  
  # Generamos las matrices de datos donde guardaremos los resultados.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  rent_BUND <- rep(0,length(BUND)) 
  rent_DAX <- rep(0,length(DAX)) 
  alpha_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
    names(alpha_activos)<-names(activos) 
    rownames(alpha_activos) <- datos[,1] 
  
  # Calculamos la rentabilidad de los activos, renta fija e índice.
  rent_activos[2:dim(activos)[1],1:length(activos)] <- log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])
  rent_BUND[2:dim(activos)[1]] <- log(BUND[2:dim(activos)[1]]/BUND[2:dim(activos)[1]-1])
  rent_DAX[2:dim(activos)[1]] <- log(DAX[2:dim(activos)[1]]/DAX[2:dim(activos)[1]-1])
  
  # Calculamos la varianza del DAX
  CumSumDAX=cumsum(rent_DAX)
  CumAvgDAX=(CumSumDAX[window_width:length(rent_DAX)]-CumSumDAX[1:(length(CumSumDAX)-window_width+1)])/window_width
  SquaredDAX=rent_DAX**2
  CumSumSqaredDAX=cumsum(SquaredDAX)
  CumAvgSquaredDAX=(CumSumSqaredDAX[window_width:length(CumSumSqaredDAX)]-CumSumSqaredDAX[1:(length(CumSumSqaredDAX)-window_width+1)])/window_width
  CumVarDAX=CumAvgSquaredDAX-CumAvgDAX**2
    
  # Calculamos la covarianza entre el activo y el índice
  CumSumAssets=cumsum(rent_activos)
  CumAvgAssets=(CumSumAssets[window_width:dim(CumSumAssets)[1],1:dim(CumSumAssets)[2]]-CumSumAssets[1:(dim(CumSumAssets)[1]-window_width+1),1:dim(CumSumAssets)[2]])/window_width
  CumSumProduct=cumsum(rent_activos*rent_DAX)
  CumSumProductAVG=(CumSumProduct[window_width:dim(CumSumProduct)[1],1:dim(CumSumProduct)[2]]-CumSumProduct[1:(dim(CumSumProduct)[1]-window_width+1),1:dim(CumSumProduct)[2]])/window_width
  CumCovar=CumSumProductAVG-CumAvgAssets*CumAvgDAX
  
  # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
  beta=CumCovar/sqrt(CumVarDAX)
    
  # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
  alpha<- rent_activos[window_width:dim(rent_activos)[1],1:dim(rent_activos)[2]]-(rent_BUND[window_width:length(rent_BUND)]+beta*(rent_DAX[window_width:length(rent_DAX)]-rent_BUND[window_width:length(rent_BUND)]))
    
  print(proc.time()-tiempo)
})
