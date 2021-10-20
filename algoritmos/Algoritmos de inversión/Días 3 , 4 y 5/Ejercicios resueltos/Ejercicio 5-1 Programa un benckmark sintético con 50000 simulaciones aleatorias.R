# Programa un Benchmark sintético con 50.000 simulaciones aleatorias + un umbral

library(profvis)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  # Cargamos los datos del Dax
  datos <- read.csv("DAX.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-2
  DAX<- datos[,2:n_activos] # Extraemos los activos quitando las 2  últimas columnas y la primera.
  div_EUR <- rep(1,dim(DAX)[1]) # Fabricamos el tipo de cambio.
  
  # Cargamos los datos del FTSE
  datos <- read.csv("FTSE.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-1
  FTSE<- datos[,2:n_activos] # Extraemos los activos quitando la última columna y la primera.
  div_GBP <- datos[,length(datos)] # Extraemos el tipo de cambio.
  FTSE<-FTSE/div_GBP # Pasamos todos los precios a euros.
  
  # Cargamos los datos del NASDAQ
  datos <- read.csv("NASDAQ.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-1
  NASDAQ<- datos[,2:n_activos] # Extraemos los activos quitando la última columna y la primera.
  div_USD <- datos[,length(datos)] # Extraemos el tipo de cambio.
  NASDAQ<-NASDAQ/div_USD # Pasamos todos los precios a euros.
  
  # Cargamos los datos del HANG SENG
  datos <- read.csv("HANG SENG.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-1
  HANGSENG<- datos[,2:n_activos] # Extraemos los activos quitando la última columna y la primera.
  div_HKD <- datos[,length(datos)] # Extraemos el tipo de cambio.
  HANGSENG<-HANGSENG/div_HKD # Pasamos todos los precios a euros.
  
  # Cargamos los datos de BOVESPA
  datos <- read.csv("BOVESPA.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-1
  BOVESPA<- datos[,2:n_activos] # Extraemos los activos quitando la última columna y la primera.
  div_BRL <- datos[,length(datos)] # Extraemos el tipo de cambio.
  BOVESPA<-BOVESPA*div_BRL # Pasamos todos los precios a euros.
  
  # Unimos todos los activos en un único data.frame
  benchmark_global<-data.frame(DAX,FTSE,NASDAQ,HANGSENG,BOVESPA)
  
  num_simulaciones<-50000
  
  # Sacamos las fechas de compra y venta de cada una de las simulaciones.
  fecha_compra<-matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE)
  fecha_venta<-matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE)
  for (simulacion in 1:num_simulaciones){
    fecha_compra[simulacion] <- round(runif(1, min = 1, max = dim(benchmark_global)[1]))
    fecha_venta[simulacion] <- round(runif(1, min = fecha_compra[simulacion], max = dim(benchmark_global)[1]))
  }
  
  # Obtenemos la cartera de inversión de cada simulación. 
    # Tenemos dos opciones
      # 1ª que todos los activos tengan un % del dinero asignado.
      # 2ª Que primero se obtenga el número de activos a invertir y luego se distribuya el dinero entre ellos.
    # Optamos por la 1ª opción.
  pesos <- matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE)
  sumatoria_pesos <- matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE)
  for (simulacion in 1:num_simulaciones){
    pesos[simulacion,]<-round(runif(dim(benchmark_global)[2], min = 0, max = 100))
    sumatoria_pesos[simulacion]<-sum(pesos[simulacion,])
  }
  
  for (simulacion in 1:num_simulaciones){ # Los activos de cada simulación no suman 100%, hay que dividirlos entre el total de la simulación para que sí lo hagan.
    for (activo in 1:dim(benchmark_global)[2]){
      pesos[simulacion,activo]<-pesos[simulacion,activo]/sumatoria_pesos[simulacion]
    }
  }
  
  # Calculamos el resultado de cada acción
    # rdo acción = (Precio venta - precio compra)*número de acciones.
    # precio compra = (precio * divisa * fecha de compra)
    # precio venta = (precio * divisa * fecha de venta)
    # número de acciones = dinero total * % asignado a la acción / precio de compra.
  
  dinero<-1000000
  
  precios_compra<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  num_acciones<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  precios_venta<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  rdo_accion<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  
  for (simulacion in 1:num_simulaciones){
    for (activo in 1:dim(benchmark_global)[2]){
      precios_compra[simulacion,activo]<-benchmark_global[fecha_compra[simulacion],activo]
      num_acciones[simulacion,activo]<-dinero*pesos[simulacion,activo]/precios_compra[simulacion,activo] 
      precios_venta[simulacion,activo]<-benchmark_global[fecha_venta[simulacion],activo]
      rdo_accion[simulacion,activo]<-(precios_venta[simulacion,activo]- precios_compra[simulacion,activo])*num_acciones[simulacion,activo]
    }
  }
  
  # Agrupamos el rdo de cada activo para sacar el rdo de cada simulación
  rdo_simulacion<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE))
  for (simulacion in 1:num_simulaciones){
    rdo_simulacion[simulacion]<-sum(rdo_accion[simulacion,])
  }
  rdo_simulacion<-t(rdo_simulacion[1,]) # antes del for es un vector de 100x1, después una matriz de 100x100 
  
  # Añadimos las columnas rdo_simulación, fecha compra y fecha de venta al DF de pesos.
  pesos<-as.data.frame(pesos)
  nombres_activos<-c(names(DAX),names(FTSE),names(NASDAQ),names(HANGSENG),names(BOVESPA))
  names(pesos)<-nombres_activos
  pesos$rdo_simulacion<-rdo_simulacion
  pesos$fecha_compra<-fecha_compra
  pesos$fecha_venta<-fecha_venta
  
  # Calculamos la rentabilidad de cada simulación
  rentabilidad<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE))
  for (simulacion in 1:num_simulaciones){
    rentabilidad[simulacion]<-log((rdo_simulacion[simulacion]+dinero)/dinero) 
  }
  rentabilidad<-t(rentabilidad[1,])# antes del for, es un vector de 100x1 después  una matriz de 100x100
  pesos$rentabilidad<-rentabilidad
  
  # Sacamos la mejor simulación y los deciles de rentabilidad.
  mejor_simulacion <- pesos[pesos$rentabilidad==max(pesos$rentabilidad),]
  quantile(pesos$rentabilidad,c(0, .1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) # Los deciles son el Bencmark sintético con el que puedes comparar tu rentabilidad obtenida.
  
  print(proc.time()-tiempo)
})
