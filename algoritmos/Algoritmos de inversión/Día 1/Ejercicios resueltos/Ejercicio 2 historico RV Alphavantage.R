# Consigue el histórico de renta variable, minuto a minuto, de 5 días, de las siguientes empresas
# Recomendación: Alphavantage 
# Tiempo objetivo: 30 minutos


library(lubridate)
library(alphavantager)

activos_a_descargar<-c("AAPL","AXP","BA","CAT","CSCO","CVX","DIS","GE","GS","HD","IBM","INTC","JNJ","JPM")

av_api_key("AOURS93CWW9EDZWO")
for (activo in activos_a_descargar){

  # Hacemos la llamada a Alphavantage
  datos<-av_get(symbol = activo, av_fun = "TIME_SERIES_INTRADAY", interval = "1min", outputsize = "full") 
  
  # La hora está en EEUU, la ajustamos a nuestro horario calculando la diferencia entre el nombre de la fila y Sys.time()
    
    # Una manera compleja de hacerlo sería calcular la diferencia entre la serie y la hora del ordenador...
    #datos = as.data.frame(datos)
    #datos$timestamp = as_datetime(datos$timestamp)
    #diferencia<-difftime(Sys.time(),datos$timestamp[length(datos$timestamp)])
    
    # Podríamos modificar la hora que consta en los datos con la diferencia que acabamos de calcular.
    #diferencia<-as.numeric(floor(diferencia*60*60)) # Transformamos la diferencia de horas a segundos para sumárselos a las fechas.
    #datos[,1]<-datos[,1]+diferencia
  
    # Pero es mucho más sencillo si lo hacemos así
    datos$timestamp <- as.POSIXct(datos$timestamp, tz = "ETC")
    datos$timestamp <- with_tz(datos$timestamp, tz = "Europe/Madrid")
  
  # Guardamos los datos con el nombre del activo.
  assign(activo,datos)
  
  # Solo nos dejan 5 llamadas por minuto. Debemos tener esto en cuenta a la hora de hacer la llamada.
  if (which(activos_a_descargar %in% activo) %% 5 ==0){
    Sys.sleep(62)
  }
}


















