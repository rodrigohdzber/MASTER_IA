
options(scipen=999, digits = 6) 
options(warn=-1)
options(java.parameters = "-Xmx8000m") 
source('algoritmo_completo_funciones_auxiliares.R', encoding="UTF-8") 


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
  # datos_descargados<-descarga_limpieza_homogeneizacion_desmanipulacion(indice,fecha_inicio, fecha_fin, ventana)
    # save(datos_descargados, file = "datos_descargados.RData")
    load("datos_descargados.RData")
  
  # Selección de activos (qué y cuando)
  activos_seleccionados<-seleccion_activos(datos_descargados, ventana, entrada, salida, percentil_dinamico, fecha_inicio)
  
  # Ranking para la asignación de recursos
  datos_ranking_asignacion<-ranking_de_asignacion_recursos(datos_descargados, activos_seleccionados, ventana, comision_minima)
  
  # Calculamos la recomendación para el día siguiente
  recomendacion_mañana<-generar_recomendacion(datos_descargados, activos_seleccionados, datos_ranking_asignacion, beneficio_objetivo_por_operacion, stop_loss, comision, comision_minima)

  # Calculamos el histórico de operaciones
  historico_operaciones_capital_dinamico<-calcular_historico_operaciones_capital_dinamico(datos_descargados,activos_seleccionados, datos_ranking_asignacion, stop_loss, asignacion_maxima, comision, comision_minima, beneficio_objetivo_por_operacion)

  # Calculamos los flujos de tesorería
  flujos_tesoreria<-calcular_flujos_tesoreria(historico_operaciones_capital_dinamico, datos_descargados)

  # Fabricamos un resumen de los resultados del algoritmo
  resumen_resultados<-resumen_operaciones_flujos(historico_operaciones_capital_dinamico, flujos_tesoreria)

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

# volumen_minimo<-datos_ranking_asignacion[[1]]
# volumen_maximo<-datos_ranking_asignacion[[2]]
# frecuencia_condicion_compra<-datos_ranking_asignacion[[3]]
# ultima_condicion_compra<-datos_ranking_asignacion[[4]]
# frecuencia_condicion_venta<-datos_ranking_asignacion[[5]]
# ultima_condicion_venta<-datos_ranking_asignacion[[6]]
# ranking<-datos_ranking_asignacion[[7]]
