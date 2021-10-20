# Vectoriza todo, salvo la obtención de combinaciones

library(profvis)
profvis({
  
  tiempo <- proc.time()
  
  # Establecemos una semilla de generación aleatoria determinada para que los rdos sean siempre iguales.
  set.seed(1000)
  
  # Sacamos la combinación ganadora y la guardamos en un vector de 50 columnas.
  combi_ganadora <- matrix(0, nrow=1, ncol=50, byrow=TRUE)
  combinacion <- sort(sample(seq(1,50,1), 5, replace = FALSE))
  combi_ganadora[combinacion]<-1
  
  # Para maximizar la eficiencia hay que eliminar el bucle lapply del código anterior. 
  # Para ello generamos 50.000 * 50 nº aleatorios entre 0 y 1
  combinaciones <- 50000
  apuestas  <- runif(combinaciones*50)
  
  # Convertimos el vector en una matriz
  apuestas <- matrix(apuestas, nrow=combinaciones, ncol=50, byrow=TRUE)
  
  # Para elegir los 5 nº de la combinación de entre los 50, nos quedamos con los 5 késimos mayores. 
  # Nos quedamos con los 5 nº aleatorios más grandes (les asignamos un 1 dentro y al resto un 0). 
  # apuestas[order(apuestas, decreasing = TRUE)[1:5],]<-1 # ESTO solo FUNCIONA para una fila aislada
  # Se podrían ordenar las filas de la matriz, pero perderíamos la posición de cada número apuestas <-t(apply(t(apuestas),2,sort)).
  for (fila in 1:combinaciones){
    vector<-apuestas[fila,]
    vector[order(vector, decreasing = TRUE)[1:5]]<-1
    vector[vector!=1]<-0
    apuestas[fila,]<-vector  
  }
  
  # Comprobamos los aciertos que hemos tenido. Apuestas y combi_ganadora son matrices de 1 y 0.
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
