# Otra manera de "atacar" Alphavantage sería leer directamente la URL del activo, dado que es un CSV.
# Descárgate los mismos datos usando la URL y limpia los resultados.
# Tiempo objetivo: 45 minutos

#"https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",activo,"&interval=1min&outputsize=full&apikey=AOURS93CWW9EDZWO.csv"

library(stringr)
activos_a_descargar<-c("AAPL","AXP","BA","CAT","CSCO","CVX","DIS","GE","GS","HD","IBM","INTC","JNJ","JPM")

for (activo in activos_a_descargar){
  
  # Añadimos el datatype csv a la url inicial
  url<-paste("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",activo,"&interval=1min&outputsize=full&apikey=AOURS93CWW9EDZWO&datatype=csv", sep="")
  
  # Obtenemos el csv ya limpio junto al header apropiado.
  datos <- read.csv(file=url, stringsAsFactors = FALSE, header = T)
  
  # Guardamos los datos con el nombre del activo.
  assign(activo, datos)
  
  # Solo nos dejan 5 llamadas por minuto. Debemos tener esto en cuenta a la hora de hacer la llamada.
  if (which(activos_a_descargar %in% activo) %% 5 ==0){
    Sys.sleep(62)
  }
}



#### Otra manera de hacerlo

library(stringr)
library(jsonlite)
library(tidyr)

activos_a_descargar<-c("AAPL","AXP","BA","CAT","CSCO","CVX","DIS","GE","GS","HD","IBM","INTC","JNJ","JPM")

for (activo in activos_a_descargar){
  url<-paste("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",activo,"&interval=1min&outputsize=full&apikey=demo.json", sep="") 
  x=fromJSON(url)
  datos=spread(plyr::ldply(x$`Time Series (1min)`, stack), key = ind, value = 'values')
  colnames(datos)=c("timestamp","open","high","low","close","volume")
  
  # Guardamos los datos con el nombre del activo.
  assign(activo,datos)
  
  # Solo nos dejan 5 llamadas por minuto. Debemos tener esto en cuenta a la hora de hacer la llamada.
  if (which(activos_a_descargar %in% activo) %% 5 ==0){
    Sys.sleep(62)
  }
}

