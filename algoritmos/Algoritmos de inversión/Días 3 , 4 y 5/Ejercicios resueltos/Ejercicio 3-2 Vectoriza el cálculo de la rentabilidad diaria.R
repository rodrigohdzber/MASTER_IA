# Vectoriza el cálculo de la rentabilidad diaria

library(profvis)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  # Cargamos los datos
  datos <- read.csv("indices.csv", sep=';',stringsAsFactors = FALSE) 
  activos<- datos[,2:length(datos)] # Extraemos los activos quitando la primera.
  
  # Calculamos la rentabilidad de los activos.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(rent_activos)<-names(activos) # Ponemos nombres a las columnas
  rownames(rent_activos) <- datos[,1] # Ponemos nombres a las filas.
  rent_activos[2:dim(activos)[1],1:length(activos)] <- log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])
  rent_activos<-rent_activos[2:dim(activos)[1],1:length(activos)] # Quitamos la primera línea dado que son ceros.
  
  # Calculamos la matriz de correlaciones
  # matriz_correlaciones<-cor(rent_activos)
  
  # Calculamos la matriz de varianzas / covarianzas.
  matriz_var_covarianzas<-cov(rent_activos) # Obtenemos diréctamente la matriz de varianzas covarianzas 
  
  # calculamos la rentabilidad, riesgo y eficiencia de N carteras aleatorias.
  num_simulaciones <- 50000
  matriz_pesos <- as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(matriz_pesos)<-names(activos)
  rentabilidad_carteras <- c(1:num_simulaciones)
  riesgo_carteras <- c(1:num_simulaciones)
  eficiencia_carteras <- c(1:num_simulaciones)
  
  progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = num_simulaciones, width=300)
  for (cartera in 1:num_simulaciones){
    
    # Sacamos los pesos de la cartera 
    pesos <- runif(dim(activos)[2], min = 0, max = 100)
    pesos[1:dim(activos)[2]]<-(pesos[1:dim(activos)[2]]/sum(pesos)) # Los pesos deben sumar 100%.
    matriz_pesos[cartera,1:dim(activos)[2]]<- pesos
    
    # Calculamos la rentabilidad diaria del periodo para cada activo: LN(precio final/ precio inicial)/nº de datos
    rent_diaria <-as.data.frame(matrix(0,nrow=1,ncol=length(activos),byrow=F))
    names(rent_diaria)<-names(activos) # Ponemos nombres a las columnas 
    
    rent_diaria<-t(rent_diaria)
    activos<-t(activos) # Calculamos la rentabilidad vectorialmente.
    rent_diaria[1:dim(activos)[1]]<-log(activos[1:dim(activos)[1],dim(activos)[2]]/activos[1:dim(activos)[1],1])/dim(rent_activos)[1]
    rent_diaria<-t(rent_diaria)
    activos<-t(activos) # Devolvemos la matriz de activos a su posición original
    
    # Calculamos la rentabilidad de la cartera según los pesos (suma producto pesos con rentabilidad diaria)
    rentabilidad_carteras[cartera]<-sum(pesos*rent_diaria)
    
    # calculamos el riesgo de la cartera (desviación), en función de los pesos.
    vector<-matriz_var_covarianzas%*%pesos # Multiplicamos la matriz de cov/var por el vector fila de pesos.
    riesgo_carteras[cartera]<-(t(vector)%*%pesos)^0.5 # Riesgo = vector anterior por vector pesos elevado a 0,5
    
    # calculamos la eficiencia de la cartera (pendiente), en función de los pesos.
    # (y2 - y1)/(x2-x1) donde Y1 e Y1 son el origen (0,0)
    eficiencia_carteras[cartera] <- (rentabilidad_carteras[cartera])/(riesgo_carteras[cartera])
    
    setWinProgressBar(progreso, cartera, title=paste(round(cartera/num_simulaciones*100,0), "%"))
  }
  close(progreso)
  
  # Graficamos la frontera de Markowitz (riesgo y rentabilidad para cada vector de pesos)
  plot(riesgo_carteras,rentabilidad_carteras, cex = .5, pch=19)
  
  # Localizamos la cartera con mayor rentabilidad, menor riesgo y mayor eficiencia.
  matriz_pesos$rentabilidad_cartera <-rentabilidad_carteras # Unimos todos los datos en un único DF.
  matriz_pesos$riesgo_cartera <-riesgo_carteras
  matriz_pesos$eficiencia_cartera <-eficiencia_carteras
  
  max_rentabilidad<-matriz_pesos[matriz_pesos$rentabilidad_cartera==max(matriz_pesos$rentabilidad_cartera),]
  min_riesgo<-matriz_pesos[matriz_pesos$riesgo_cartera==min(matriz_pesos$riesgo_cartera),]
  max_eficiencia<-matriz_pesos[matriz_pesos$eficiencia_cartera==max(matriz_pesos$eficiencia_cartera),]
  
  max_rentabilidad
  min_riesgo
  max_eficiencia
  
  print(proc.time()-tiempo)
})
