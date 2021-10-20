# Solventa el problema de la generación de la frontera para 30 activos.

library(profvis)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  # Cargamos los datos 
  datos <- read.csv("DAX.csv", XXXXXXXXXXXXXXXXXX) 
  activos<- datos[,2:length(datos)] # Extraemos los activos quitando la primera columna.
  
  # Calculamos la rentabilidad de los activos.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
    names(rent_activos)<-names(activos) # Ponemos nombres a las columnas
    rownames(rent_activos) <- datos[,1] # Ponemos nombres a las filas.
  rent_activos[2:dim(activos)[1],1:length(activos)] <- log(activos[2:dim(activos)[1],1:length(activos)]/activos[2:dim(activos)[1]-1,1:length(activos)])
  rent_activos<-rent_activos[2:dim(activos)[1],1:length(activos)] # Quitamos la primera línea dado que son ceros.
  
  # Necesitamos un criterio de preselección de activos. Por ejemplo: rentabilidad / riesgo (varianza)
  rentabilidad_ponderada_riesgo<- XXXXXXXXXXXXXXXXXXXXXXXXX
  
    # Nos quedamos con los N activos que tengan mejor relación rentabilidad riesgo
    n_empresas<-5
    seleccion<- XXXXXXXXXXXXXXXXXXXXXX

  # Calculamos la matriz de correlaciones
  # matriz_correlaciones<- XXXXXXXXXXXXXX # De momento no es necesario
  
  # Calculamos la matriz de varianzas / covarianzas.
  matriz_var_covarianzas<-cov(rent_activos) # Obtenemos diréctamente la matriz de varianzas covarianzas
  
  # Calculamos la rentabilidad diaria del periodo para cada activo: LN(precio final/ precio inicial)/nº de datos
  rent_diaria <-as.data.frame(matrix(0,nrow=1,ncol=length(activos),byrow=F))
  names(rent_diaria)<-names(activos) # Ponemos nombres a las columnas
  
  rent_diaria<-t(rent_diaria)
  activos<-t(activos) # rentabilidad vectorial: traspongo precios para que los activos sean filas y no columnas.
  rent_diaria[1:dim(activos)[1]]<-log(activos[1:dim(activos)[1],dim(activos)[2]]/activos[1:dim(activos)[1],1])/dim(rent_activos)[1]
  rent_diaria<-t(rent_diaria)
  activos<-t(activos)
  
  # Sacamos la matriz de pesos para cada cartera
  num_simulaciones <- 50000
  pesos <- runif(dim(activos)[2]*num_simulaciones, min = 0, max = 100) # pesos de todas simulaciones de una vez.
  pesos <- matrix(pesos, nrow=num_simulaciones, ncol=dim(activos)[2], byrow=TRUE) 
  pesos[,1:dim(activos)[2]]<-(pesos[,1:dim(activos)[2]]/rowSums(pesos))
  
  # Calculamos la rentabilidad de la cartera en función de los pesos 
  # Para cada conjunto de pesos queremos hacer suma producto de pesos con rentabilidad diaria. 
  rentabilidad_carteras <- c(1:num_simulaciones) # vector para guardar las rentabilidades de cada cartera.
  matriz_intermedia <- matrix(0,nrow=dim(activos)[1],ncol=dim(activos)[2],byrow=F) 
  matriz_intermedia<-t(as.vector(rent_diaria)*t(pesos)) # Sin as.vector da error al multiplicar dos matrices.
  rentabilidad_carteras<-rowSums(matriz_intermedia) # Sumamos las filas para sacar rentabilidad de cada cartera.
  
  # calculamos el riesgo de la cartera (desviación), en función de los pesos.
  riesgo_carteras <- c(1:num_simulaciones) # Generamos un vector donde guardaremos el riesgo de cada cartera.
  # Ahora multiplicamos la matriz de var/covar por cada fila de la matriz de pesos.
  matriz_intermedia <- pesos[1:num_simulaciones,]%*%matriz_var_covarianzas 
  # Ahora multiplicamos cada fila de la matriz intermedia * cada fila de la matriz pesos y el rdo lo elevamos a 0.5. 
  # Ojo, la multiplicación es matricial de dos vectores.
  matriz_intermedia <- matriz_intermedia * pesos
  riesgo_carteras <- rowSums(matriz_intermedia)
  riesgo_carteras <- riesgo_carteras^0.5
  
  # calculamos la eficiencia de la cartera (pendiente), en función de los pesos.
  eficiencia_carteras <- c(1:num_simulaciones)
  eficiencia_carteras[1:num_simulaciones] <- rentabilidad_carteras[1:num_simulaciones]/riesgo_carteras[1:num_simulaciones]
  
  # Graficamos la frontera de Markowitz (riesgo y rentabilidad para cada vector de pesos)
  plot(riesgo_carteras,rentabilidad_carteras, cex = .5, pch=19)
  
  # Localizamos la cartera con mayor rentabilidad, menor riesgo y mayor eficiencia.
  pesos<-as.data.frame(pesos)
  names(pesos)<-names(rent_activos) # Ponemos nombres a las columnas 
  pesos$rentabilidad_cartera <-rentabilidad_carteras # Unimos todos los datos en un único DF.
  pesos$riesgo_cartera <-riesgo_carteras
  pesos$eficiencia_cartera <-eficiencia_carteras
  
  max_rentabilidad<-pesos[pesos$rentabilidad_cartera==max(pesos$rentabilidad_cartera),]
  min_riesgo<-pesos[pesos$riesgo_cartera==min(pesos$riesgo_cartera),]
  max_eficiencia<-pesos[pesos$eficiencia_cartera==max(pesos$eficiencia_cartera),]
  
  print(proc.time()-tiempo)
})
