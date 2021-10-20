# Vectoriza todo el programa y trata de bajar del minuto de cálculo

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
  
    # Una manera de hacerlo sería:
    fechas <- round(runif(num_simulaciones*2, min = 1, max = dim(benchmark_global)[1]))
    fechas <- matrix(fechas, nrow=num_simulaciones, ncol=2, byrow=TRUE) # Coloco las fechas en una matriz.
    
    # fechas<-t(apply(t(fechas),2,sort)) # Ordeno las fechas (fecha de compra se menor de compra). Funciona, pero esta línea tarda 1,91 seg.
    fecha_compra<-matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE) # Este sistema para ordenar las fechas tarda solo 0,03 seg.
    fecha_compra[1:num_simulaciones]<-ifelse(fechas[1:num_simulaciones,1]<fechas[1:num_simulaciones,2], fecha_compra[1:num_simulaciones]<-fechas[1:num_simulaciones,1], fecha_compra[1:num_simulaciones]<-fechas[1:num_simulaciones,2])
    
    fecha_venta<-matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE)
    fecha_venta[1:num_simulaciones]<-ifelse(fechas[1:num_simulaciones,1]<fechas[1:num_simulaciones,2], fecha_venta[1:num_simulaciones]<-fechas[1:num_simulaciones,2], fecha_venta[1:num_simulaciones]<-fechas[1:num_simulaciones,1])
    
    # Otra manera más sencilla de hacerlo es:
    fecha_compra<-round(runif(num_simulaciones,min=1,max=dim(benchmark_global)[1]),0)
    fecha_venta<- round(runif(num_simulaciones,min=fecha_compra,max=dim(benchmark_global)[1]),0)
    
  # Obtenemos la cartera de inversión de cada simulación.
  # Hay dos opciones
    # 1ª que todos los activos tengan un % del dinero asignado.
    # 2ª Que primero se obtenga el número de activos a invertir y luego se distribuya el dinero entre ellos.
  # Optamos por la 1ª opción.
  pesos <- runif(dim(benchmark_global)[2]*num_simulaciones, min = 0, max = 100) # pesos de todas simulaciones de una vez.
  pesos <- matrix(pesos, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE)
  pesos[,1:dim(benchmark_global)[2]]<-(pesos[,1:dim(benchmark_global)[2]]/rowSums(pesos))
  
  # Calculamos el resultado de cada acción
    # rdo acción = (Precio venta - precio compra)*número de acciones.
    # precio compra = (precio * divisa * fecha de compra)
    # precio venta = (precio * divisa * fecha de venta)
    # número de acciones = dinero total * % asignado a la acción / precio de compra.
  
  precios_compra<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  precios_compra[1:num_simulaciones,]<-benchmark_global[fecha_compra[1:num_simulaciones],]
  
  dinero<-1000000
  
  num_acciones<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  num_acciones<-dinero*pesos/precios_compra # Presuponemos que se puede comprar una fracción de acción.
  
  precios_venta<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  precios_venta[1:num_simulaciones,]<-benchmark_global[fecha_venta[1:num_simulaciones],]
  
  rdo_accion<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  rdo_accion<-(precios_venta - precios_compra)*num_acciones
  
  # Agrupamos el rdo de cada activo para sacar el rdo de cada simulación
  rdo_simulacion<-rowSums(rdo_accion)
  
  # Añadimos las columnas rdo_simulación, fecha compra y fecha de venta al DF de pesos.
  pesos<-as.data.frame(pesos)
  nombres_activos<-c(names(DAX),names(FTSE),names(NASDAQ),names(HANGSENG),names(BOVESPA))
  names(pesos)<-nombres_activos 
  pesos$rdo_simulacion<-rdo_simulacion
  pesos$fecha_compra<-fecha_compra
  pesos$fecha_venta<-fecha_venta
  
  # Calculamos la rentabilidad de cada simulación
  pesos$rentabilidad<-log((pesos$rdo_simulacion+1000000)/1000000) # sumamos el bº al capital inicial para calcular la rentabilidad.
  
  # Sacamos la mejor simulación y los deciles de rentabilidad.
  # Los deciles son el Bencmark sintético con el que puedes comparar tu rentabilidad obtenida.
  mejor_simulacion <- pesos[pesos$rentabilidad==max(pesos$rentabilidad),]
  quantile(pesos$rentabilidad,c(0, .1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) 
  
  print(proc.time()-tiempo)
})
