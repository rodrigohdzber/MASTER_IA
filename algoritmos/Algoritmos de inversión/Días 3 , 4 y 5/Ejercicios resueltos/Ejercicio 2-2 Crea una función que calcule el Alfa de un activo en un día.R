# Crea una función que calcule el Alpha de un activo en un día. 

# Ejemplos de cómo vectorizar progresivamente una instrucción
  # rent_activos[2,1]<-log(activos[2,1]/activos[1,1]) # 1 activo un día
  
  #rent_activos[2,1:length(activos)] <- log(activos[2,1:length(activos)]/activos[2-1,1:length(activos)]) # Todos los act un solo día (día 2)
  #rent_activos[2:5,1:length(activos)] <- log(activos[2:5,1:length(activos)]/activos[(2:5)-1,1:length(activos)]) # Todos los act días 2 a 5
  
  # Todos los activos todos los días
  #rent_activos[2:dim(activos)[1],1:length(activos)] <-log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])


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
  rent_activos[2:dim(activos)[1],1:length(activos)] <- log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])
  rent_BUND[2:dim(activos)[1]] <- log(BUND[2:dim(activos)[1]]/BUND[2:dim(activos)[1]-1])
  rent_DAX[2:dim(activos)[1]] <- log(DAX[2:dim(activos)[1]]/DAX[2:dim(activos)[1]-1])
  
  # Generamos una función que calcule el Alpha para un solo activo.
  calcular_alpha<-function(activo,rent_activos,rent_DAX,rent_BUND,dia){
    
    # Calculamos la varianza del DAX
    varianza_ind<-var(rent_DAX[2:dia])
    
    # Calculamos la covarianza entre el activo y el índice
    cov_act_ind <- cov(rent_activos[2:dia,activo],rent_DAX[2:dia])
    
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
      alpha_activos[dia,activo]<-calcular_alpha(activo,rent_activos,rent_DAX,rent_BUND,dia)
    }
    setWinProgressBar(barra_progreso, dia, title=paste(round(dia/dim(activos)[1]*100,0), "% realizado"))
  }
  close(barra_progreso)
  
  print(proc.time()-tiempo)
})
