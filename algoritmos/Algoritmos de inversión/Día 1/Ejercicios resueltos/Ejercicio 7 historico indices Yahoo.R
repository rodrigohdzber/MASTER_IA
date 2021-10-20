# Obtén los datos históricos de cada índice (60 días)
# Recomendación: Yahoo Finance 
# Tiempo objetivo: 45 minutos

tickers_de_indices<-c("%5EBFX","%5EBVSP", "%5EDJI", "%5EFCHI", "%5EFTSE", "%5EGDAXI", "%5EHSI", "%5EIBEX", "%5EMXX", "%5EJKSE", "%5EMERV", "%5EOMXSPI", "%5EOSEAX", "%5ESSMI", "%5ESTI")

library(RCurl)
library(stringr)
library(stringi)

obtener_indice<-function(url, curl){
  benchmark <- tryCatch(    
    {
      benchmark = getURLContent(url, curl = curl, verbose = F)      
      
      # No es necesario que indique el valor de retorno a través de `return ()` en el código.
    },
    
    error=function(cond) {
      message("No nos hemos podido descargar el indice")
      message("Este es el mensaje de error que ha dado:")
      message(cond)
      
      return("Sin datos")
    },
    
    finally={
      # Este código se ejecuta al final, independientemente de si ha funcionado o ha dado un error.
    }
  )    
  return(benchmark)
}

# Establecemos la ventana temporal
ventana<-60

for(benchmark in tickers_de_indices){

  print(paste(benchmark," descargando", sep=""))
  
  # Establecemos el rango de fechas que queremos importar.
  # Yahoo utiliza formato de fecha poxis - time unix
  # (cantidad de segundos transcurridos desde la medianoche UTC del 1 de enero de 1970, teniendo cada día 86400 segundos)
  # Convertimos la fecha a formato poxis
  tiempo<-as.character(Sys.time())
  substr(tiempo, 12, 24) <- "00:00:00"
  tiempo<-as.POSIXct(tiempo)
  fecha_fin <- ceiling(as.numeric(as.POSIXct(tiempo)))
  fecha_inicial <- ceiling(as.numeric(as.POSIXct(tiempo - ventana*86400)))
  
  # Obtengo las cookies de su web (curl) 
  curl = getCurlHandle(cookiejar="", verbose = F)
  res = getURI(paste("https://es.finance.yahoo.com/quote/",benchmark,"?ltr=1", sep=""), curl = curl) 
  
  # Buscamos CrumbStore dentro del código que nos hemos bajado (crumb rastro de migas)
  posicion<-unlist(str_locate_all(pattern ='CrumbStore', res))
  crumb<-unlist(strsplit(substr(res, posicion[1], posicion[2]+30), "\""))
  crumb<-crumb[nchar(crumb)==max(nchar(crumb))]
  crumb<-stri_escape_unicode(crumb) # Algunas veces, el crumb tiene una función de escape (un salto de página).  
  
  # Construimos la url para bajarnos los datos
  url<- paste("https://query1.finance.yahoo.com/v7/finance/download/",benchmark,"?period1=",fecha_inicial,"&period2=",fecha_fin,"&interval=1d&events=history&crumb=",crumb, sep= "")
  
  # En la llamada tenemos que pasar la url con el crumb y la cookie.
  benchmark <- obtener_indice(url, curl)
  benchmark <- read.csv (textConnection (benchmark) , header = T)
}
