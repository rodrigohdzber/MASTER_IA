# Obtenemos cada combinación de una sola vez.

library(profvis)
profvis({
  
  tiempo <- proc.time()
  
  # Establecemos una semilla de generación aleatoria determinada para que los rdos sean siempre iguales.
  set.seed(1000)
  
  # Sacamos la combinación ganadora. Al usar replace False no pueden salir dos números iguales.
  combi_ganadora <- sort(sample(seq(1,50,1), 5, replace = FALSE))
  
  # Sacamos las combinaciones que apostamos y comprobamos el nº de aciertos en cada una de ellas.
  combinaciones <- 50000
  apuestas<- matrix(0,nrow=combinaciones,ncol=5,byrow=T) # Creamos una matriz para las apuestas.
  aciertos<-matrix(0,nrow=combinaciones,ncol=1,byrow=T) # Creamos un vector para los aciertos.
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = combinaciones, width=300) # width es el nº de pixeles de la barra.
  
  for (combinacion in 1:combinaciones){
    
    # Obtenemos las apuestas realizadas  
    apuestas[combinacion,]<-sort(sample(seq(1,50,1), 5, replace = FALSE)) # La combinación sale ordenada
    
    # Comprobamos los aciertos que tenemos entre nuestras apuestas y la combinación ganadora.
    for (bola_apostada in 1:5){ 
      
      # Aunque las combinaciones estén ordenadas, no tiene xq coincidir el nº en la misma columna.
      for (bola_premiada in 1:5){ # Por lo que comparamos cada bola_apostada con cada bola_premiada
        if(apuestas[combinacion,bola_apostada]==combi_ganadora[bola_premiada]){
          aciertos[combinacion]<- aciertos[combinacion]+1
        }
      }
    }
    setWinProgressBar(barra_progreso, combinacion, title=paste(round(combinacion/combinaciones*100,0), "% realizado"))
  }
  close(barra_progreso)
  
  # Calculamos la frecuencia de los aciertos (cuantas veces hemos acertado 1 nº, cuantas veces 2 etc)
  library(plyr) 
  aciertos<-count(aciertos)
  aciertos
  
  print(proc.time()-tiempo)
})
