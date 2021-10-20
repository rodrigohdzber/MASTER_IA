
# Objetivo: Describe el árbol de funciones que descarga, limpia, homogeneiza y desmanipula la información. 
  # Que inputs recibe cada función
  # A qué funciones se llama dentro de cada función
  # Que output arroja cada función

# ¿Encuentras alguna mejora en la función de desmanipular (ajustar)?


options(scipen=999, digits = 6) 
options(warn=-1)
source('ejercicio1_descarga_datos_funciones_auxiliares.R', encoding="UTF-8")


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
  fecha_inicio<-"01/01/2018"
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
  datos_descargados<-descarga_limpieza_homogeneizacion_desmanipulacion(indice,fecha_inicio, fecha_fin, ventana)

