# Programa la frontera de Markowitz con 5 activos y 50.000 simulaciones.

library(profvis)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  # Cargamos los datos
  datos <- read.csv("indices.csv", sep=';',stringsAsFactors = FALSE) 
  activos<- datos[,2:length(datos)] # Extraemos los activos quitando la primera columna.
  
  # Calculamos la rentabilidad de los activos.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
    names(rent_activos)<-names(activos) # Ponemos nombres a las columnas
    rownames(rent_activos) <- datos[,1] # Ponemos nombres a las filas.
    
  for (dia in 2:dim(activos)[1]){
    for (activo in 1:length(activos)){ 
      rent_activos[dia,activo]<-log(activos[dia,activo]/activos[dia-1,activo])
    }  
  }
  
  rent_activos<-rent_activos[2:dim(activos)[1],1:length(activos)] # Quitamos la primera línea dado que son ceros.
  
  # Calculamos la matriz de correlaciones
  # matriz_correlaciones<-cor(rent_activos)
  
  # Calculamos la matriz de varianzas / covarianzas.
  matriz_var_covarianzas<-cov(rent_activos) # Obtenemos diréctamente la matriz de varianzas covarianzas 
  
  # No es necesario calcular las varianzas, cov ya saca las var y cov. Si las queremos sacar a mano sería
    # varianzas<-as.data.frame(matrix(0,nrow=length(rent_activos),ncol=1,byrow=F)) # DF para las varianzas.
    # varianzas[1:length(rent_activos)]<-var(rent_activos[1:dim(rent_activos)[1],1:length(rent_activos)]) 
    # da una matriz. La diagonal son las varianzas que queremos.
    # varianzas<-diag(as.matrix(varianzas)) # Extraemos la diagonal
    # diag(matriz_var_covarianzas)<-varianzas # Cambiamos la diagonal de la matriz var/covar.
  
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
    
      # Si tenemos un número de activos grande, y todos los activos tienen peso, no se llega a formar nunca la frontera eficiente.
      # Tenemos 2 posibles soluciones.
        # Mantener los pesos de los activos que superen cierto umbral (con lo que no sabremos el número de activos de cada cartera, quizás 0)
        # Quedarnos con los K-ésimos (por ejemplo, los 5 mejores).
        # Dado que queremos soluciones con distintos números de activos, nos quedamos con el umbral.

    pesos[1:dim(activos)[2]]<-(pesos[1:dim(activos)[2]]/sum(pesos)) # Los pesos deben sumar 100%.
    matriz_pesos[cartera,1:dim(activos)[2]]<- pesos
    
    # Calculamos la rentabilidad diaria del periodo para cada activo: LN(precio final/ precio inicial)/nº de datos
    rent_diaria <-as.data.frame(matrix(0,nrow=1,ncol=length(activos),byrow=F))
    names(rent_diaria)<-names(activos) # Ponemos nombres a las columnas 
    
    for (activo in 1:length(activos)){ # Una manera lenta de hacerlo.
      rent_diaria[activo]<-log(activos[dim(activos)[1],activo]/activos[1,activo])/dim(rent_activos)[1]
    }  
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

































