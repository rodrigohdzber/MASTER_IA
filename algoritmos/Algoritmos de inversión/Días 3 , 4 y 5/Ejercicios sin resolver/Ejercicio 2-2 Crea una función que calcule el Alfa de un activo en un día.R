# Crea una función que calcule el Alpha de un activo en un día. 

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
  
  # Calculamos la rentabilidad de los activos, renta fija e índice de manera vectorial.
  rent_activos[XXXXXXXXXXXXX] <- XXXXXXXXXXXXXXXXXXXXXXXX
  rent_BUND[XXXXXXXXXXXX] <- XXXXXXXXXXXXXXXXXX
  rent_DAX[XXXXXXXXXXXX] <- XXXXXXXXXXXXXXXXXX
  
  # Generamos una función que calcule el Alpha para un solo activo.
  calcular_alpha<-function(activo,rent_activos,rent_DAX,rent_BUND,dia){
    
    # Calculamos la varianza del DAX
    varianza_ind<- XXXXXXXXXXXXXXX
    
    # Calculamos la covarianza entre el activo y el índice
    cov_act_ind <- XXXXXXXXXXXXXXXXXXX
    
    # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
    if (varianza_ind==0){
      beta <-0
    }else{
      beta <- cov_act_ind/varianza_ind
    }
    
    # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
    alpha<- rent_activos[dia,activo]-(rent_BUND[dia]+beta*(rent_DAX[dia]-rent_BUND[dia]))
    
    return(alpha)
  }
  
  # Invocamos a la función para que calcule el alpha de todos los activos, todos los días.
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = dim(activos)[1], width=300)
  for (dia in 11:dim(activos)[1]){
    for (activo in 1:length(activos)){
      alpha_activos[dia,activo]<- XXXXXXXXXXXXXX
    }
    setWinProgressBar(barra_progreso, dia, title=paste(round(dia/dim(activos)[1]*100,0), "% realizado"))
  }
  close(barra_progreso)
  
  print(proc.time()-tiempo)
})
