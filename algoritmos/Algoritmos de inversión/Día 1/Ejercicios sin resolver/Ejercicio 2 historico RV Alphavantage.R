# Consigue el histórico de renta variable, minuto a minuto, de 5 días, de las siguientes empresas
# Recomendación: Alphavantage 

library(alphavantager)

activos_a_descargar<-c("AAPL","AXP","BA","CAT","CSCO","CVX","DIS","GE","GS","HD","IBM","INTC","JNJ","JPM")

av_api_key("XXXXXXXXXXXXXXXXXXX")

for (activo in activos_a_descargar){
  
  # Hacemos la llamada a Alphavantage
   
  
  # La hora está en EEUU, la ajustamos a nuestro horario.
  
  
  # Guardamos los datos con el nombre del activo.
  

}
