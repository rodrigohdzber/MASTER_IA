library(profvis)

profvis({
  
  tiempo <- proc.time() # Inicia el cronómetro
  
  # Cantidad de monos
  num_simulaciones<-50000
  
  # Cantidad de activos elegibles por mono para Markowitz
  num_activos_mono = 5
  
  # Ventana de cálculo de markowitz (120 aprox 6 meses)
  ventana_mark = 240
  
  # Cantidad de simulaciones de cada mono para calcular markowitz
  num_simulaciones_mark_mono <- 1000
  
  # Rango mínimo de días entre compra y venta
  min_dias_mono = 2
  
  # Dinero del que dispone el mono
  dinero<-1000000
  
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
  FTSE<-FTSE*div_GBP # Pasamos todos los precios a euros.
  
  # Cargamos los datos del NASDAQ
  datos <- read.csv("NASDAQ.csv", sep=';',stringsAsFactors = FALSE) 
  n_activos <- length(datos)-1
  NASDAQ<- datos[,2:n_activos] # Extraemos los activos quitando la última columna y la primera.
  div_USD <- datos[,length(datos)] # Extraemos el tipo de cambio.
  NASDAQ<-NASDAQ*div_USD # Pasamos todos los precios a euros.
  
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
  
  # Calculamos la rentabilidad de los activos.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(benchmark_global)[1],ncol=length(benchmark_global),byrow=F)) 
  names(rent_activos)<-names(benchmark_global)
  rent_activos[2:dim(benchmark_global)[1],1:length(benchmark_global)] <- log(benchmark_global[2:dim(benchmark_global)[1],1:length(benchmark_global)]/benchmark_global[2:dim(benchmark_global)[1]-1,1:length(benchmark_global)])
  rent_activos<-rent_activos[2:dim(benchmark_global)[1],1:length(benchmark_global)] 
  
  activos_disponibles = c(1:dim(benchmark_global)[2])
  fechas_disponibles = c((ventana_mark+1):dim(benchmark_global)[1])
  
  # Generamos una función que calcule los activos de cada mono
  calcular_mono <- function(){
    
    fechas_mono <- XXXXXXXXXXXXXXX
    fecha_compra_mono <- XXXXXXXXXXXXXXX
    fecha_venta_mono <- XXXXXXXXXXXXXXX
    activos_seleccionados_mono = XXXXXXXXXXXXXXX
    
    activos_mono<- XXXXXXXXXXXXXXX
    rent_activos_mono<- XXXXXXXXXXXXXXX
    
    matriz_correlaciones_mono<- XXXXXXXXXXXXXXX
    matriz_var_covarianzas_mono<- XXXXXXXXXXXXXXX
    
    rent_diaria_mono <-as.data.frame(matrix(0,nrow=1,ncol=length(activos_mono),byrow=F))
    names(rent_diaria_mono)<-names(activos_mono) 
    
    rent_diaria_mono<-t(rent_diaria_mono)
    activos_mono<-t(activos_mono) 
    rent_diaria_mono[1:dim(activos_mono)[1]]<- log(activos_mono[1:dim(activos_mono)[1],dim(activos_mono)[2]] /activos_mono[1:dim(activos_mono)[1],1])/dim(rent_activos_mono)[1]
    rent_diaria_mono<-t(rent_diaria_mono)
    activos_mono<-t(activos_mono) 
    
    pesos_mono <- XXXXXXXXXXXXXXX

    rentabilidad_carteras_mono <- c(1:num_simulaciones_mark_mono) 
    matriz_intermedia_mono <- matrix(0,nrow=dim(activos_mono)[1],ncol=dim(activos_mono)[2],byrow=F) 
    matriz_intermedia_mono<-t(as.vector(rent_diaria_mono)*t(pesos_mono)) 
    rentabilidad_carteras_mono<-rowSums(matriz_intermedia_mono) 
    
    riesgo_carteras_mono <- c(1:num_simulaciones_mark_mono) 
    matriz_intermedia_mono <- pesos_mono[1:num_simulaciones_mark_mono,]%*%matriz_var_covarianzas_mono 
    matriz_intermedia_mono <- matriz_intermedia_mono * pesos_mono
    riesgo_carteras_mono <- rowSums(matriz_intermedia_mono)
    riesgo_carteras_mono <- riesgo_carteras_mono^0.5
    
    eficiencia_carteras_mono <- c(1:num_simulaciones_mark_mono)
    eficiencia_carteras_mono[1:num_simulaciones_mark_mono] <- (rentabilidad_carteras_mono[1:num_simulaciones_mark_mono] /riesgo_carteras_mono[1:num_simulaciones_mark_mono])
    
    pesos_mono=cbind(pesos_mono,eficiencia_carteras_mono)
    max_eficiencia_mono<-pesos_mono[pesos_mono[,dim(pesos_mono)[2]]==max(pesos_mono[,dim(pesos_mono)[2]]),1:(dim(pesos_mono)[2]-1)]
    
    resultado_mono = c(1:dim(benchmark_global)[2])*0
    resultado_mono[activos_seleccionados_mono] = unlist(max_eficiencia_mono[1:dim(activos_mono)[2]])
    
    #añado las fechas al final del resultado
    resultado_mono = c(resultado_mono,fecha_compra_mono, fecha_venta_mono)
    
    return(resultado_mono)
  }
  
  pesos = t(replicate(num_simulaciones,calcular_mono()))
  
  #quito las fechas del final del resultado
  fecha_compra = pesos[,dim(pesos)[2]-1]
  fecha_venta = pesos[,dim(pesos)[2]]
  pesos = pesos[,1:(dim(pesos)[2]-2)]
  
  # Calculamos el resultado de cada acción
    # rdo acción = (Precio venta - precio compra)*número de acciones.
    # precio compra = (precio * divisa * fecha de compra)
    # precio venta = (precio * divisa * fecha de venta)
    # número de acciones = dinero total * % asignado a la acción / precio de compra.
  
  precios_compra<-as.data.frame(matrix(0, nrow=num_simulaciones, ncol=dim(benchmark_global)[2], byrow=TRUE))
  precios_compra[1:num_simulaciones,]<-benchmark_global[fecha_compra[1:num_simulaciones],]
  
  num_acciones<-dinero*t(t(pesos)/t(precios_compra)) # Presuponemos que se puede comprar una fracción de acción.
  num_acciones<-as.data.frame(num_acciones)
  
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
  pesos$rentabilidad<-log((pesos$rdo_simulacion+dinero)/dinero) # sumamos el bº al capital inicial para calcular la rentabilidad.
  
  # Sacamos la mejor simulación y los deciles de rentabilidad.
  # Los deciles son el Bencmark sintético con el que puedes comparar tu rentabilidad obtenida.
  mejor_simulacion <- pesos[pesos$rentabilidad==max(pesos$rentabilidad),]
  quantile(pesos$rentabilidad,c(0, .1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) 
  
  print(proc.time()-tiempo)
})
