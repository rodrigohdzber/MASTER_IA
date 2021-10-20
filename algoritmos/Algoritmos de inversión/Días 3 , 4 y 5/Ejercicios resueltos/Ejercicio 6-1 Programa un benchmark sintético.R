# Programa un Benchmark sintético

  # No siempre dispondremos de un índice de referencia con el que compararnos.
  # Por ejemplo, si seleccionamos 50 empresas del S&P500, ¿con qué nos vamos a comparar?, ¿con el índice completo?
  # Necesitamos crear un índice sintético.
  # Reconstruye la composición del índice en cada instante de tiempo.
  # Haz una estrategia de Hold & Sell (con proporciones iguales), según entren y salgan los activos del índice.

tiempo <- proc.time() # Inicia el cronómetro

# Cargamos los datos de los activos
datos <- read.csv("data_ibex.csv", sep=',',stringsAsFactors = FALSE) 
  datos$X<-gsub("_0", "", datos$X) 
  datos$X<-gsub("_1", "", datos$X) 

# Sacamos la composición del índice durante el periodo 2003 - 2019 y su cotización
fechas <- sort(as.Date(unique(datos$X.1)))
activos <- unique(datos$X)
composicion <- as.data.frame(matrix(0,nrow=length(fechas),ncol=length(activos),byrow=F)) 
  colnames(composicion)<- activos
  rownames(composicion)<- fechas
  
cotizacion <- as.data.frame(matrix(0,nrow=length(fechas),ncol=length(activos),byrow=F)) 
  colnames(cotizacion)<- activos
  rownames(cotizacion)<- fechas
  
invertido<- as.data.frame(matrix(0,nrow=length(fechas),ncol=length(activos),byrow=F)) 
  colnames(invertido)<- activos
  rownames(invertido)<- fechas
  
resultado <- as.data.frame(matrix(0,nrow=length(fechas),ncol=1,byrow=F)) 
  rownames(invertido)<- fechas
  
for (activo in 1:length(activos)){
  composicion[rownames(composicion) %in% datos$X.1[datos$X==activos[activo]],activo]<-1
  cotizacion[rownames(composicion) %in% datos$X.1[datos$X==activos[activo]],activo]<- datos[datos$X.1 %in% datos$X.1[datos$X==activos[activo]] & datos$X == activos[activo], 3]
}
  
cotizacion<-cotizacion*composicion
  
# Calculamos la proporción que tenemos en cada activo del índice, en cada instante de tiempo. Suponemos pesos iguales para cada activo.
proporcion_invertida <- composicion / rowSums(composicion)

# Calculamos la rentabilidad que habría obtenido el índice sintético, presuponiendo una estrategia buy & hold, según entran y salen los activos del índice.
  dinero <- 100000

  # Realizo la inversión inicial
  invertido[1,]<-dinero*proporcion_invertida[1,]
  
  # Recorro los días calculando el resultado obtenido
  for (dia in 2:dim(cotizacion)[1]){
    
    # Compruebo si ha cambiado la composición del índice
    if (all(composicion[dia,]==composicion[dia-1,])==T) {
    
      # Calculo la rentabilidad que los activos han obtenido en el día
      rentabilidad<- log(cotizacion[dia,]/cotizacion[dia-1,])
        rentabilidad[is.na(rentabilidad)]<-0
      
      # Actualizo el resultado  
      resultado[dia,1]<- sum(invertido[dia-1,]*rentabilidad)
      
      # Actualizo la inversión
      invertido[dia,]<- invertido[dia-1,] + (invertido[dia-1,]*rentabilidad)
      
    } else {
      
      # Ha cambiado la composición (ha entrado o salido algún valor). Hay que rehacer el índice.
      dinero <- sum(invertido[dia-1,])
      invertido[dia,]<-dinero*proporcion_invertida[dia,]
      
      # Este día no obtenemos ningún resultado
    }
  }

# Comprobamos la evolución de nuestro capital
evolucion_inversion<-rowSums(invertido)

  
# Cargamos los datos del índice
datos_indice <- read.csv("benchmark_ibex.csv", sep=',',stringsAsFactors = FALSE) 

  # Sacamos los datos del Ibex (sin dividendos), filtrando las fechas que nos interesan y guardando la columna de cierre
  ibex<- as.data.frame(datos_indice[datos_indice$X=="ibex" & datos_indice$X.1 %in% rownames(composicion),3])
    colnames(ibex)<-"Ibex"
    rownames(ibex)<- fechas
    
# Graficamos el Ibex,enfrentándo al índice sintético.
rentabilidad_ibex<-c(0,log(ibex[2:dim(ibex)[1],1]/ibex[1:(dim(ibex)[1]-1),1]))
  evolucion_ibex<-rep(0,length(evolucion_inversion))
  evolucion_ibex[1]<-evolucion_inversion[1]

  for (dia in 2:dim(ibex)[1]){
    evolucion_ibex[dia]<- evolucion_ibex[dia-1]*(1+rentabilidad_ibex[dia])
  }
    
  # Calculamos el máximo y mínimo para graficar.
  maximo<-max(c(evolucion_inversion, evolucion_ibex))
  minimo<-min(c(evolucion_inversion, evolucion_ibex))
  
  grafico<-plot(evolucion_inversion~as.Date(fechas), main="Comparación Ibex VS Sintético",col="darkblue", type="l", cex.lab=1, cex.axis=0.6, lwd = 2, xlab="La línea roja representa al Ibex sin dividendos \ny la línea azul al Sintético", ylab = "", ylim=c(minimo, maximo))
    lines(evolucion_ibex~as.Date(fechas), col="red")

print(proc.time()-tiempo)

