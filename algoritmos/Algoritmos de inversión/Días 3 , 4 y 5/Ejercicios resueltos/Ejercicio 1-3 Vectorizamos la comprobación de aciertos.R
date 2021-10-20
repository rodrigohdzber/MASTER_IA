# Vectorizamos la comprobación de aciertos

library(profvis)
profvis({
  
  tiempo <- proc.time()
  
  # Establecemos una semilla de generación aleatoria determinada para que los rdos sean siempre iguales.
  set.seed(1000)
  
  # Sacamos la combinación ganadora y la guardamos en un vector de 50 columnas.
  # Si sale el nº 32, en la columna 32 pondremos un 1. 
  # Esto evitará recorrer los números de la combinación para comprobar los aciertos.
  combi_ganadora <- matrix(0, nrow=1, ncol=50, byrow=TRUE)
  combinacion <- sort(sample(seq(1,50,1), 5, replace = FALSE))
  combi_ganadora[combinacion]<-1
  
  # Creamos una matriz de 50.000 filas por 50 columnas para guardar las apuestas.
  combinaciones <- 50000
  apuestas <- matrix(0, nrow=combinaciones, ncol=50, byrow=TRUE)

  # Creamos una función que saque combinaciones y las guarde en la matriz apuestas.
  # Usamos Lapply (for optimizado) para llamar a la función que saque combinaciones y las meta en la matriz.
  unlist(lapply(1:combinaciones, function(fila) c(apuestas[fila,sort(sample(seq(1,50,1), 5, replace = FALSE))]<<-1)))
  
  # Comprobamos los aciertos que hemos tenido. Apuestas y combi_ganadora son matrices de 1 y 0.
  # Si multiplicamos el vector combi_ganadora por la matriz de apuestas, en el caso de que coincida 
  # el nº apostado con el ganador tendremos un 1, y en el resto de casos un 0.
  aciertos <- t(apuestas) * as.vector(combi_ganadora)
  aciertos<-t(aciertos)
  
  # Sumamos las filas para obtener el número de aciertos de cada apuesta
  num_aciertos <- rowSums(aciertos)
  
  # Calculamos la frecuencia de los aciertos (cuantas veces hemos acertado 1 nº, cuantas veces 2 etc)
  library(plyr) 
  aciertos<-count(num_aciertos)
  aciertos

  print(proc.time()-tiempo)
})
