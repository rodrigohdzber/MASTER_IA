# Vectoriza todo el programa, menos la covarianza. 

library(profvis)
library(devtools)
library(RollingWindow) # install_github("andrewuhl/RollingWindow") # https://github.com/andrewuhl/RollingWindow

profvis({
  tiempo <- proc.time() 
  
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
  varianza_DAX<-RollingVar(rent_DAX,window = 10)
  
  # Calculamos la covarianza entre el activo y el índice
  for (activo in 1:dim(activos)[2]){ # RollingCov solo acepta vectores, por lo que tenemos que recorrer los activos con un for,
    cov_act_ind[,activo] <- RollingCov(rent_DAX, rent_activos[,activo], window = 10) 
  }
  
  # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
  beta[11:dim(activos)[1],1:length(activos)] <- cov_act_ind[11:dim(activos)[1],1:length(activos)]/varianza_DAX[11:length(rent_DAX)]
  
  # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
  alpha_activos[11:dim(activos)[1],1:length(activos)]<- rent_activos[11:dim(activos)[1],1:length(activos)]-(rent_BUND[11:dim(activos)[1]]+beta[11:dim(activos)[1],1:length(activos)]*(rent_DAX[11:dim(activos)[1]]-rent_BUND[11:dim(activos)[1]]))
  alpha_activos<-alpha_activos[11:dim(activos)[1],1:length(activos)] # Eliminamos los primeros valores (ceros).
  
  print(proc.time()-tiempo)
})


#################### Otra manera de hacerlo (sin usar RollingWindow)

library(profvis)
library(devtools)
library(roll) # en lugar de library(RollingWindow) 

profvis({
  tiempo <- proc.time()
  
  ventana = 10
  
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
  varianza_DAX<-roll_var(rent_DAX, width = ventana)
  
  # Calculamos la covarianza entre el activo y el índice
  for (activo in 1:dim(activos)[2]){ # RollingCov solo acepta vectores, por lo que tenemos que recorrer los activos con un for,
    cov_act_ind[,activo] <- roll_cov(rent_DAX,rent_activos[,activo],width = ventana)
  }
  
  # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
  beta[ventana:dim(activos)[1],1:length(activos)] <- cov_act_ind[ventana:dim(activos)[1],1:length(activos)]/varianza_DAX[ventana:length(rent_DAX)]
  
  # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
  alpha_activos[ventana:dim(activos)[1],1:length(activos)]<- rent_activos[ventana:dim(activos)[1],1:length(activos)]-(rent_BUND[ventana:dim(activos)[1]]+beta[ventana:dim(activos)[1],1:length(activos)]*(rent_DAX[ventana:dim(activos)[1]]-rent_BUND[ventana:dim(activos)[1]]))
  alpha_activos<-alpha_activos[(ventana):dim(activos)[1],1:length(activos)] # Eliminamos los primeros valores (ceros).
  
  print(proc.time()-tiempo)
})

