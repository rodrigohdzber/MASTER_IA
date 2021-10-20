# Obtén el histórico (60 días) de cada activo de varios índices (usa los resultados del paso anterior)
# Recomendación: Yahoo Finance 
# Índices: "%5EHSI", "%5EIBEX"

library(RCurl)
library(stringr)
library(stringi)
library(rvest)

tickers_de_indices<-c("%5EHSI", "%5EIBEX")

obtener_datos_activo<-function(activo, fecha_inicial, fecha_fin, columna_fechas){
  
  # Obtengo las cookies de su web (curl) 
  curl = XXXXXXXXXXXXXXXXXX
  res = XXXXXXXXXXXXXXXXX 
  
  # Buscamos CrumbStore dentro del código que nos hemos bajado.
  crumb<- XXXXXXXXXXXXXXXXXXXX
  
  # Construimos la url para bajarnos los datos
  url<-XXXXXXXXXXXXXXXX
  
  # En la llamada tenemos que pasar la url con el crumb y la cookie.
  info_activo = getURLContent(url, curl = curl, verbose = F)
  info_activo <- read.csv ( textConnection ( info_activo ) , header = T )
  
  return(info_activo)
}



# Establecemos el rango de fechas que queremos importar.
ventana <- 60
tiempo<-as.character(Sys.time())
substr(tiempo, 12, 24) <- "00:00:00"
tiempo<-as.POSIXct(tiempo)
fecha_fin <-ceiling(as.numeric(as.POSIXct(tiempo)))
fecha_inicial <- ceiling(as.numeric(as.POSIXct(tiempo - ventana*86400)))

for(indice in tickers_de_indices){
  
  # Construimos la url que contiene la información de los activos de cada índice.
  datos_web<- XXXXXXXXXXXXXX
  
  if (dim(datos_web)[1]>1){ # Comprobamos si Yahoo publica algún activo del índice. Si la longitud es 1, no.
    
    # Reconstruimos la tabla. Tiene 6 columnas (simbolo, nombre de la empresa, último precio, cambio, cambio % y volumen.)
    activos_a_importar<- XXXXXXXXXXXXXX
    nombres_empresas<-XXXXXXXXXXXX
    
    num_activos<-length(activos_a_importar)
    
    # Extraemos la divisa en la que cotizan los activos.
    divisa<- XXXXXXXXXXXX
    
  } else {
    print(paste("El índice",indice,"no tiene activos descargables"))
  }  
  
  # Importamos los activos que hemos seleccionado.
  for (activo in activos_a_importar){
    print(paste("Descargando ",activo, sep= ""))
    info_activo<- obtener_datos_activo(activo, fecha_inicial, fecha_fin, columna_fechas)
    assign(activo,info_activo)
  }
}
