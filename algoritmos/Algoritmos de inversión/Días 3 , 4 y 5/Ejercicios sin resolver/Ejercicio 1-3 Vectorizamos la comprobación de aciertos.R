# Vectorizamos la comprobación de aciertos

library(profvis)
profvis({
  
  tiempo <- proc.time()
  
  # Establecemos una semilla de generación aleatoria determinada para que los rdos sean siempre iguales.
  set.seed(1000)
  
  # Sacamos la combinación ganadora.
  combi_ganadora <- XXXXXXXXXXXXXXXXXX
  
  # Creamos una matriz de 50.000 filas por 50 columnas para guardar las apuestas.
  combinaciones <- 50000
  apuestas <- matrix(0, nrow=combinaciones, ncol=50, byrow=TRUE)

  # Creamos una función que saque combinaciones y las guarde en la matriz apuestas.
  # Usamos Lapply (for optimizado) para llamar a la función que saque combinaciones y las meta en la matriz.
  XXXXXXXXXXXXXXXXXXXXXXXX
  
  # Comprobamos los aciertos que hemos tenido.
  aciertos <- XXXXXXXXXXXXXXXXXXXX

  # Sumamos las filas para obtener el número de aciertos de cada apuesta
  num_aciertos <- rowSums(aciertos)
  
  # Calculamos la frecuencia de los aciertos (cuantas veces hemos acertado 1 nº, cuantas veces 2 etc)
  library(plyr) 
  aciertos<-count(num_aciertos)
  aciertos

  print(proc.time()-tiempo)
})
