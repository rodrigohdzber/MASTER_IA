# Quita la función y vectoriza todo (menos varianza y covarianza). 

library(profvis)
profvis({
  tiempo <- proc.time()
  
  # Cargamos los datos y extraemos por un lado los activos, la renta fija y el índice Dax
  datos <- read.csv("DAX.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-2
  activos<- datos[,2:n_activos] # Extraemos los activos quitando las dos últimas columnas y la primera.
  BUND <- datos[,length(datos)] # Extraemos la renta fija.
  DAX <- datos[,length(datos)-1] # Extraemos el índice.
  
  # Generamos las matrices de datos donde guardaremos los resultados.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  rent_BUND <- rep(0,length(BUND)) 
  rent_DAX <- rep(0,length(DAX)) 
  varianza_DAX <-rep(0,length(DAX)) 
  cov_act_ind<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F))
  beta<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  alpha_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(alpha_activos)<-names(activos) 
  rownames(alpha_activos) <- datos[,1] 
  
  # Calculamos la rentabilidad de los activos, renta fija e índice.
  rent_activos[2:dim(activos)[1],1:length(activos)] <- log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])
  rent_BUND[2:dim(activos)[1]] <- log(BUND[2:dim(activos)[1]]/BUND[2:dim(activos)[1]-1])
  rent_DAX[2:dim(activos)[1]] <- log(DAX[2:dim(activos)[1]]/DAX[2:dim(activos)[1]-1])
  
  # Calculamos la varianza del DAX
  #varianza_DAX[2:length(rent_DAX)]<-var(rent_DAX[2:length(rent_DAX)]) # No funciona, usa el vector entero.
  for (dia in 11:dim(activos)[1]){
    varianza_DAX[dia]<-var(rent_DAX[2:dia]) # Excluimos la 1ª línea porque contiene ceros. 
  } # Este for únicamente lleva 0,06 segundos (no es ineficiente).
  
  # Calculamos la covarianza entre el activo y el índice
  #cov_act_ind[2:dim(activos)[1],1:length(activos)] <- cov(rent_activos[2:dim(activos)[1],1:length(activos)],rent_DAX[2:dim(activos)[1]]) 
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = dim(activos)[1], width=300)
  for (dia in 11:dim(activos)[1]){
    cov_act_ind[1:length(cov_act_ind)] <- cov(rent_activos[2:dia,1:length(activos)],rent_DAX[2:dia])
    setWinProgressBar(barra_progreso, dia, title=paste(round(dia/dim(activos)[1]*100,0), "% realizado"))
  } 
  close(barra_progreso)
  
  # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
  beta[2:dim(activos)[1],1:length(activos)] <- cov_act_ind[2:dim(activos)[1],1:length(activos)]/varianza_DAX[2:length(rent_DAX)]
  
  # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
  alpha_activos[2:dim(activos)[1],1:length(activos)]<- rent_activos[2:dim(activos)[1],1:length(activos)]-(rent_BUND[2:dim(activos)[1]]+beta[2:dim(activos)[1],1:length(activos)]*(rent_DAX[2:dim(activos)[1]]-rent_BUND[2:dim(activos)[1]]))
  alpha_activos<-alpha_activos[11:dim(activos)[1],1:length(activos)] # Eliminamos los primeros valores (ceros).
  
  print(proc.time()-tiempo)
})
