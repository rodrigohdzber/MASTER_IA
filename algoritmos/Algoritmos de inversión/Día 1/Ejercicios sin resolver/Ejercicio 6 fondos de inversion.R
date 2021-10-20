# Consigue la rentabilidad los fondos de inversión en el año actual
# Recomendación: quefondos  https://www.quefondos.com/es/fondos/ranking/anual/index.html?cardCount=1&cardSize=100

library(rvest)

obtener_fondos<-function(){
  
  # Consultamos cuantos fondos están listados en la web.

  
  # Redondeamos el número para que sea divisible entre 100.
 
  
  # Creamos una barra de progreso para conocer el avance del proceso.
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = num_fondos/100, width=300) # width es el nº de pixeles de la barra.
  
  # Descargamos los fondos de inversión.
  for (pagina in 1:(num_fondos/100)){
    
    setWinProgressBar(barra_progreso, pagina, title=paste(round(pagina/(num_fondos/100)*100,0), "% realizado"))
    
    url<-XXXXXXXXXXXXXXXXXXXXXX 
    
    
    
    
    
    print(paste(pagina*100," fondos descargados de ",num_fondos))
  }
  
  close(barra_progreso)
  
  # Construimos la tabla de fondos de inversión.
  tabla_fondos<- XXXXXXXXXXXXXXX
  
  return(tabla_fondos)
}

tabla_fondos<-obtener_fondos()
