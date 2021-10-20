# Obtén los activos que cotizan en cada índice y en qué divisa están cotizando
# Recomendación: Yahoo Finance 
# Los componentes de cada índice se renuevan cada x meses. La mejora manera de descargarnos datos sobre un índice no es consultar una lista fija de activos, sino consultar previamente qué activos están cotizando en cada índice antes de hacer la consulta activo por activo. 

library(RCurl)
library(stringr)
library(stringi)
library(rvest)

tickers_de_indices<-c("%5EBFX","%5EBVSP", "%5EFCHI", "%5EFTSE", "%5EGDAXI", "%5EHSI", "%5EIBEX", "%5EMXX", "%5EJKSE", "%5EMERV", "%5EOMXSPI", "%5EOSEAX", "%5ESSMI", "%5ESTI")

for(indice in tickers_de_indices){
  
  # Construimos la url que contiene la información de los activos de cada índice.
  url <- XXXXXXXXXXXXXXXXXXXXXXX
  datos_web <- XXXXXXXXXXXXXXXXXXXXXXX
  
  if (dim(datos_web)[1]>1){ # Comprobamos si Yahoo publica algún activo del índice. Si la longitud es 1, no.
    
    # Queremos obtener los nombres de las empresas y los tickers para poder importarlos posteriormente
    nombres_empresas<- XXXXXXXXXXXXXXXXX
    tickers_empresas<- XXXXXXXXXXXXXXXXX

    
    # Extraemos la divisa en la que cotizan los activos.
    divisa <- XXXXXXXXXXXXXXXXXXXXXXXX
          
  
    print(nombres_empresas)
    print(divisa)
  }
}


