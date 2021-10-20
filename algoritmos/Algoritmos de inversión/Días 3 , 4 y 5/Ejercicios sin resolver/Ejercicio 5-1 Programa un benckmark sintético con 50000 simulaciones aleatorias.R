# Programa un Benchmark sintético con 50.000 simulaciones aleatorias + un umbral

library(profvis)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  # Cargamos los datos del Dax
  DAX <- XXXXXXXXXXXXXXXXXXXXX 

  # Cargamos los datos del FTSE
  FTSE <- XXXXXXXXXXXXXXXXXXXXX
  
  # Cargamos los datos del NASDAQ
  NASDAQ <- XXXXXXXXXXXXXXXXXXXXX
  
  # Cargamos los datos del HANG SENG
  HANGSENG <- XXXXXXXXXXXXXXXXXXXXX
  
  # Cargamos los datos de BOVESPA
  BOVESPA <- XXXXXXXXXXXXXXXXXXXXX
  
  # Unimos todos los activos en un único data.frame
  benchmark_global<-data.frame(DAX,FTSE,NASDAQ,HANGSENG,BOVESPA)
  
  num_simulaciones<-50000
  
  # Sacamos las fechas de compra y venta de cada una de las simulaciones.
  fecha_compra<-matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE)
  fecha_venta<-matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE)
  for (simulacion in 1:num_simulaciones){
    fecha_compra <- XXXXXXXXXXXXXXXX
    fecha_venta <- XXXXXXXXXXXXXXXX
  }
  
  # Obtenemos la cartera de inversión de cada simulación. 
    # Tenemos dos opciones
      # 1ª que todos los activos tengan un % del dinero asignado.
      # 2ª Que primero se obtenga el número de activos a invertir y luego se distribuya el dinero entre ellos.
    # Optamos por la 1ª opción.
  pesos <- matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE)
  sumatoria_pesos <- matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE)
  for (simulacion in 1:num_simulaciones){
    pesos<-XXXXXXXXXXXXXXXXXXX
    sumatoria_pesos[simulacion]<-sum(pesos[simulacion,])
  }
  
  # Los activos de cada simulación no suman 100%, hay que dividirlos entre el total de la simulación para que sí lo hagan.
  for (simulacion in 1:num_simulaciones){ 
    for (activo in 1:dim(benchmark_global)[2]){
      pesos<- XXXXXXXXXXXXXXXXX
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
      precios_compra<- XXXXXXXXXXXXXX
      num_acciones<- XXXXXXXXXXXXXX 
      precios_venta<-XXXXXXXXXXXXXXX
      rdo_accion<-XXXXXXXXXXXXXXXX
    }
  }
  
  # Agrupamos el rdo de cada activo para sacar el rdo de cada simulación
  rdo_simulacion<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=1, byrow=TRUE))
  for (simulacion in 1:num_simulaciones){
    rdo_simulacion[simulacion]<-XXXXXXXXXXXXX
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
    rentabilidad[simulacion]<-XXXXXXXXXXXXXXXXXXXXXX 
  }
  rentabilidad<-t(rentabilidad[1,])# antes del for, es un vector de 100x1 después una matriz de 100x100
  pesos$rentabilidad<-rentabilidad
  
  # Sacamos la mejor simulación y los deciles de rentabilidad.
  mejor_simulacion<-XXXXXXXXXXXXXXXX
  XXXXXXXXXXXXXXXXX # Los deciles son el Bencmark sintético con el que puedes comparar tu rentabilidad obtenida.
  
  print(proc.time()-tiempo)
})
