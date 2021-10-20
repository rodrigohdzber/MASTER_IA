# Vectoriza todo el programa
#ejecutandolo varias vecess algunas da 0.30 y otras 0.20 suele dar 0.20 por lo general
library(profvis)
library(plyr)
library(picante)  #randomizeMatrix
#library(Rfast)  #con esta libreria hay una funcion que es colShuffle() que tambi�n valdria en vez de randomizeMatrix pero va mas lento
profvis({
  
  tiempo <- proc.time()
  
  #Establecemos una semilla de generación aleatoria determinada para que los rdos sean siempre iguales.
  set.seed(1000)
  
  #Sacamos la combinación ganadora y la guardamos en un vector de 50 columnas.
  combi_ganadora <- matrix(0, nrow=1, ncol=50, byrow=TRUE)
  combinacion <- sample(seq(1,50,1), 5, replace = FALSE)  #5numeros aleatorios entre 1 y 50
  combi_ganadora[combinacion]<-1
  
  #Sacamos 50.000 combinaciones.
  
  #Creamos una matriz de 50000 filas con los primeros 5 elementos que sean 1 y el resto 0
  Combinaciones = 50000
  
  apuestas = matrix(rep(c(rep(1,5),rep(0,45)),Combinaciones), nrow = Combinaciones, ncol = 50, byrow = TRUE) #lo hacemos para 50000 filas y 50 columnas
           #contenido de la matriz 5 unos 45 ceros eso para 50000 filas y 50 columnas y rellena por filas
 
  #Intentar aleatorizar esa matriz...
  #despues de usar la libreria vegan la funcion permatswap, despues de usar la libreria bipartite, ambas sin resultados, 
  #usando la libreria picante parece que si se consigue mediante la funcion randomizeMatrix
  #de todos los metodo solo sale con "richness" (para mantener la suma de las filas)
  #para mi *** https://www.it-swarm-es.com/es/r/como-aleatorizar-o-permutar-un-marco-de-datos-en-forma-de-fila-y-columna/972584365/
  
  apuestas = randomizeMatrix(apuestas, null.model = "richness", iterations = 10)
  
  #Comprobamos los aciertos que hemos tenido. Apuestas y combi_ganadora son matrices de 1 y 0.
  #cada vex que coincida sumara uno(multiplicacion matricial)
  aciertos = apuestas %*% t(combi_ganadora) #multiplicacion matricial
  
  #Calculamos la frecuencia de los aciertos (cuantas veces hemos acertado 1 nº, cuantas veces 2 etc)
  Frec_aciertos = count(aciertos)   
  print(Frec_aciertos)
  
  print(proc.time()-tiempo)
})
