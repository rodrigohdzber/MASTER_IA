# Otra manera de "atacar" Alphavantage sería leer directamente la URL del activo, dado que es un CSV.
# Descárgate los mismos datos usando la URL y limpia los resultados.

#"https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",activo,"&interval=1min&outputsize=full&apikey=AOURS93CWW9EDZWO.csv"

library(stringr)
activos_a_descargar<-c("AAPL","AXP","BA","CAT","CSCO","CVX","DIS","GE","GS","HD","IBM","INTC","JNJ","JPM")

for (activo in activos_a_descargar){
  
  # Descargo la info de la web
  url<- XXXXXXXXXXXXXXXXXXXXXXXX
  
  # Limpiamos los datos
 
  
  # Guardamos los datos con el nombre del activo.
  
  
  # Solo nos dejan 5 llamadas por minuto. Debemos tener esto en cuenta.

  
}
