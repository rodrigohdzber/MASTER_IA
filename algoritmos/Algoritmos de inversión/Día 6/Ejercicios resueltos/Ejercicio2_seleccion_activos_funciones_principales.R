# Objetivo: Realiza la selección de los activos en función del percentil del Alpha de Jensen.

  # Objetivo ejercicio 2.1: calcula el Alpha de Jensen para cada activo, en cada instante de tiempo.
  # Objetivo ejercicio 2.2: calcula los precios objetivo de compra y venta
  # Objetivo ejercicio 2.3: haz dinámicos los percentiles y la ventana

options(scipen=999, digits = 6) 
options(warn=-1)
source('Ejercicio2.3_seleccion_activos_funciones_auxiliares.R', encoding="UTF-8")


# Información de índices a atacar.
  
  nombres_indices<-c("Euro Stoxx 50", "IBEX 35", "S&P 500", "DAX", "Bovespa", "Nikkei 225", "Reino Unido 100",
                     "Dow Jones Industrial Average", "IBEX Medium Cap", "IBEX Small Cap", "Hang Seng")
  tickers_indices<-c("STOXX50E", "IBEX", "SPX", "GDAXI", "BVSP", "N225", "invuk100", "DJI", "IBEXC", "IBEXS", "HSI")
  urls_indices<-c("https://es.investing.com/indices/eu-stoxx50-components","https://es.investing.com/indices/spain-35-components", 
                  "https://es.investing.com/indices/us-spx-500-components", "https://es.investing.com/indices/germany-30-components",
                  "https://es.investing.com/indices/bovespa-components", "https://es.investing.com/indices/japan-ni225-components", 
                  "https://es.investing.com/indices/investing.com-uk-100-components", "https://es.investing.com/indices/us-30-components", 
                  "https://es.investing.com/indices/ibex-medium-cap-components", "https://es.investing.com/indices/ibex-small-cap-components", 
                  "https://es.investing.com/indices/hang-sen-40-components")
  
  tabla_indices<-data.frame(nombres_indices,tickers_indices,urls_indices)

###########################################################################################################################################
  
  # Parámetros del algoritmo
  fecha_inicio<-"1/1/2018"
  fecha_fin<-"31/12/2018"
  ventana<-30
  entrada<-0.15
  salida<-0.85
  percentil_dinamico<-T
  comision<-0.0008
  comision_minima<-8
  beneficio_objetivo_por_operacion<-100
  stop_loss<-5
  asignacion_maxima<-50000
  indice<-as.character(tabla_indices$urls_indices[2])
  
  # Descarga de datos
  # datos_descargados<-descarga_limpieza_homogeneizacion_desmanipulacion(indice,fecha_inicio, fecha_fin, ventana)
  load("datos_descargados.RData")
  
  # Selección de activos (qué y cuando)
  activos_seleccionados<-seleccion_activos(datos_descargados, ventana, entrada, salida, percentil_dinamico, fecha_inicio)
  
###########################################################################################################################################
  
# apertura<-datos_descargados[[1]]
# cierre<-datos_descargados[[2]]
# maximo<-datos_descargados[[3]]
# minimo<-datos_descargados[[4]]
# volumen<-datos_descargados[[5]]
# indice<-datos_descargados[[6]]
# divisa<-datos_descargados[[7]]
# renta_fija<-datos_descargados[[8]]
# maestro_valores<-datos_descargados[[9]]
  
# percentil_entrada<-activos_seleccionados[[1]]
# percentil_salida<-activos_seleccionados[[2]]
# alpha_entrada<-activos_seleccionados[[3]]
# alpha_salida<-activos_seleccionados[[4]]
# precio_objetivo_compra<-activos_seleccionados[[5]]
# precio_objetivo_venta<-activos_seleccionados[[6]]
# ventana_dinamica<-activos_seleccionados[[7]]
