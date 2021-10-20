# Vectoriza el programa para que tarde menos de 1 segundo

library(profvis)
library(data.table)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  n_escenarios <- 50000
  
  Cartera <- read.csv('Cartera.csv', header = T, dec = ".", stringsAsFactors = F, sep = ";")
  Cartera$perd.esper <- Cartera$PD * Cartera$LGD * Cartera$EAD # pérdida esperada
  Cartera$perd.default <- Cartera$EAD * Cartera$LGD # pérdida asociada a default
  Cartera$cuantil <- qnorm(Cartera$PD, mean = 0, sd = 1)
  num_exposic <- length(Cartera$LGD) # Número de exposiciones “filas” = 103
  EAD_total <- sum(Cartera$EAD) # EAD total
  
  y <- rnorm(n_escenarios, mean = 0, sd = 1) # vector con estado de la economía
  xi <- rnorm(n_escenarios*num_exposic, mean = 0, sd = 1) # N(0,1) # vector 103*10000
  
  # Transformamos el vector en una matriz de 50.000 filas (escenarios) por 103 columnas
  xi <- matrix(xi, nrow=n_escenarios, ncol=num_exposic, byrow=TRUE) 
  
  # Calcular las pérdidas de la cartera
  p <- 0.1
  zi <- sqrt(p)*y+sqrt(1-p)*xi # filas = escenarios y columnas = carteras
  
  # zi(filas escenarios,columnas exposiciones) < Cartera$cuantil(filas exposiciones, transponer)
    # Cartera$cuantil es un vector de 4. Para poder compararlo con la matriz hay que hacer la  traspuesta. 
    # Hay que hacer la traspuesta al rdo para que vuelva a su posición original.
  def <- t(t(zi) < Cartera$cuantil)  # Matriz de true o False
  
  # perdida_exposiciones = def(filas escenarios,columnas exposiciones) * Cartera$perd.default
    # Cartera$oerd_dafaul es un vector de 4. Para poder multiplicarlo hay que usar la traspuesta.
  simulacion_carteras <- t(def)*Cartera$perd.default
  perdidas_por_escenario <- colSums(simulacion_carteras)   
  
  # Ordenamos los escenarios de menor a mayor pérdida.
  simulacion_carteras<-simulacion_carteras[,(order(perdidas_por_escenario))]
  
  # Recuperamos el escenario que corresponde al percentil 95% y lo metemos en el DF original.
  Cartera$escenario_95<- simulacion_carteras[,round(n_escenarios*0.95)]
  
  # Comparamos el peso de la perdida de cada exposición en el escenario 95 con el peso EAD.
  Cartera$porcentaje_EAD <- Cartera$EAD/sum(Cartera$EAD)
  Cartera$porcentaje_esc95 <- Cartera$escenario_95/sum(Cartera$escenario_95)
  
  # Hacemos la comparación a nivel de grupo y de segmento.
  Cartera<-data.table(Cartera) # Convierto el data frame en un data table.
  
  # Mostramos el % EAD y % escenario 95
  print(Cartera[,lapply(.SD,sum),by=Grupo,.SDcols=10:11])
  print(Cartera[,lapply(.SD,sum),by=Segmento,.SDcols=10:11])
  
  print(proc.time()-tiempo)
})
