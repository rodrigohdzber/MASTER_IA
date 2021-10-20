# Cambia la función para que calcule al alpha de todos los activos de un día. 

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
  alpha_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(alpha_activos)<-names(activos) 
  rownames(alpha_activos) <- datos[,1] 
  
  # Calculamos la rentabilidad de los activos, renta fija e índice.
  rent_activos[2:dim(activos)[1],1:length(activos)] <- log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])
  rent_BUND[2:dim(activos)[1]] <- log(BUND[2:dim(activos)[1]]/BUND[2:dim(activos)[1]-1])
  rent_DAX[2:dim(activos)[1]] <- log(DAX[2:dim(activos)[1]]/DAX[2:dim(activos)[1]-1])
  
  # Generamos una función que calcule los Alpha de todos los activos en un mismo día.
  calcular_alpha<-function(dia,rent_activos,rent_DAX,rent_BUND){
    
    # Calculamos la varianza del DAX
    varianza_ind<-var(rent_DAX[2:dia])
    
    # Calculamos la covarianza entre el activo y el índice
    cov_act_ind<-as.data.frame(matrix(0,nrow=1,ncol=length(rent_activos),byrow=F)) 
    cov_act_ind[1:length(cov_act_ind)] <- cov(rent_activos[2:dia,1:length(activos)],rent_DAX[2:dia]) 
    
    # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
    beta<-as.data.frame(matrix(0,nrow=1,ncol=length(rent_activos),byrow=F))
    if (varianza_ind==0){
      beta <-0
    }else{
      beta[XXXXXXXXXXX] <- XXXXXXXXXXXXXXXX
    }
    
    # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
    alpha<-as.data.frame(matrix(0,nrow=1,ncol=length(rent_activos),byrow=F)) 
    alpha[XXXXXXXXXXXX]<- XXXXXXXXXXXXXXXXXXXX
    
    return(alpha)
  }
  
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = dim(activos)[1], width=300)
  for (dia in 11:dim(activos)[1]){
    alpha_activos[XXXXXXXXXXX]<-calcular_alpha(XXXXXXXXXXXXXX)
    setWinProgressBar(barra_progreso, dia, title=paste(round(dia/dim(activos)[1]*100,0), "% realizado"))
  } 
  close(barra_progreso)
  print(proc.time()-tiempo)
})
