# Programa un Benchmark sintético

  # No siempre dispondremos de un índice de referencia con el que compararnos.
  # Por ejemplo, si seleccionamos 50 empresas del S&P500, ¿con qué nos vamos a comparar?, ¿con el índice completo?
  # Necesitamos crear un índice sintético.
  # Reconstruye la composición del índice en cada instante de tiempo.
  # Haz una estrategia de Hold & Sell (con proporciones iguales), según entren y salgan los activos del índice.

# Cargamos los datos de los activos
datos <- read.csv("data_ibex.csv", sep=',',stringsAsFactors = FALSE) 

# Sacamos la composición del índice durante el periodo 2003 - 2019 y su cotización
fechas <- XXXXXXXXXXXXXXX
activos <- XXXXXXXXXXXXXX
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
  composicion[XXXXXXXXXXXXXXXXXX]<-1
  cotizacion[XXXXXXXXXXXXXXXXXXX]<- datos[XXXXXXXXXXXXXXXXXX]
}
  
cotizacion<-cotizacion*composicion
  
# Calculamos la proporción que tenemos en cada activo del índice, en cada instante de tiempo. Suponemos pesos iguales para cada activo.
proporcion_invertida <- XXXXXXXXXXXXXXXXX

# Calculamos la rentabilidad que habría obtenido el índice sintético, presuponiendo una estrategia buy & hold, según entran y salen los activos del índice.
  dinero <- 100000

  # Realizo la inversión inicial
  invertido[1,]<-dinero*proporcion_invertida[1,]
  
  # Recorro los días calculando el resultado obtenido
  for (dia in 2:dim(cotizacion)[1]){
    
    # Compruebo si ha cambiado la composición del índice
    if (XXXXXXXXXXXXXXXXXXXXXXX) {
    
      # Calculo la rentabilidad que los activos han obtenido en el día
      rentabilidad<- XXXXXXXXXXXXXXXXXX
      
      # Actualizo el resultado  
      resultado[XXXXXX]<- XXXXXXXXXXXXXX
      
      # Actualizo la inversión
      invertido[XXXXXX]<- XXXXXXXXXXXXXX
      
    } else {
      
      # Ha cambiado la composición (ha entrado o salido algún valor). Hay que rehacer el índice.
      dinero <- XXXXXXXXXX
      invertido[XXXXXXX]<-XXXXXXXXXXX
      
      # Este día no obtenemos ningún resultado
    }
  }

# Comprobamos la evolución de nuestro capital
evolucion_inversion<-rowSums(invertido)

# Cargamos los datos del índice
datos_indice <- read.csv("benchmark_ibex.csv", sep=',',stringsAsFactors = FALSE) 

  # Sacamos los datos del Ibex (sin dividendos), filtrando las fechas que nos interesan y guardando la columna de cierre
  ibex<- XXXXXXXXXXXXXXXXXXXXX
    colnames(ibex)<-"Ibex"
    rownames(ibex)<- fechas
    
# Graficamos el Ibex,enfrentándo al índice sintético.
  rentabilidad_ibex<- XXXXXXXXXXXXXXXXXXXX
    evolucion_ibex<-rep(0,length(evolucion_inversion))
    evolucion_ibex[1]<-evolucion_inversion[1]

  for (dia in 2:dim(ibex)[1]){
    evolucion_ibex[dia]<- XXXXXXXXXXXX
  }
    
  # Calculamos el máximo y mínimo para graficar.
  maximo<-max(c(evolucion_inversion, evolucion_ibex))
  minimo<-min(c(evolucion_inversion, evolucion_ibex))
  
  grafico<- XXXXXXXXXXXXXXXXXXX
