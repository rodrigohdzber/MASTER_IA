# Vectoriza todo el programa

library(profvis)
library(plyr) 

profvis({
  
  tiempo <- proc.time()
  
  # Establecemos una semilla de generación aleatoria determinada para que los rdos sean siempre iguales.
  set.seed(1000)
  
  # Sacamos la combinación ganadora y la guardamos en un vector de 50 columnas.
  combi_ganadora <- matrix(0, nrow=1, ncol=50, byrow=TRUE)
  combinacion <- sample(seq(1,50,1), 5, replace = FALSE)
  combi_ganadora[combinacion]<-1
  
  # Sacamos 50.000 combinaciones. 
  apuestas<-t(replicate(50000,rank(runif(50)))) # sacamos 50.000 permutaciones de 1 a 50 en cada línea.
  apuestas[apuestas<=45]<-0# los que sean menor o igual a 45 son 0.
  apuestas[apuestas>45]<-1# los que sean mayores de 45 los igualamos a 1.
  
  # Comprobamos los aciertos que hemos tenido. Apuestas y combi_ganadora son matrices de 1 y 0.
  aciertos <- t(apuestas) * as.vector(combi_ganadora)
  aciertos<-t(aciertos)
  
  # Sumamos las filas para obtener el número de aciertos de cada apuesta
  num_aciertos <- rowSums(aciertos)
  
  # Calculamos la frecuencia de los aciertos (cuantas veces hemos acertado 1 nº, cuantas veces 2 etc)
  aciertos<-count(num_aciertos)
  aciertos
  
  print(proc.time()-tiempo)
})
