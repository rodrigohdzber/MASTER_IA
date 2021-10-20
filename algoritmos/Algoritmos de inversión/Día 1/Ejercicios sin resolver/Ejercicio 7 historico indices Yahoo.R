# Obtén los datos históricos de cada índice (60 días)
# Recomendación: Yahoo Finance 

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
  fecha_fin<- XXXXXXXXXXXXXXXXX
  fecha_inicial<- XXXXXXXXXXXXXXX
  
  # Obtengo las cookies de su web (curl) 
  curl = getCurlHandle(cookiejar="", verbose = F)
  res = getURI(paste("https://es.finance.yahoo.com/quote/",benchmark,"?ltr=1", sep=""), curl = curl) 
  
  # Buscamos CrumbStore dentro del código que nos hemos bajado.
  crumb<- XXXXXXXXXXXXXXX
  
  # Construimos la url para bajarnos los datos
  url<- XXXXXXXXXXXXXXXXXXX
  
  # En la llamada tenemos que pasar la url con el crumb y la cookie.
  benchmark<-obtener_indice(url, curl)
  benchmark <- read.csv (textConnection (benchmark) , header = T)
}
