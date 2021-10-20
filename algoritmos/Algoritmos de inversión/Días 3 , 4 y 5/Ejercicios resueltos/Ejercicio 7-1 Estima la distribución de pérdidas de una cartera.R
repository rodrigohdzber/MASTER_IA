# Estima la distribución de pérdidas de una cartera (50.000 simulaciones)
# Vectoriza el programa para que tarde menos de 1 segundo

library(profvis)

profvis({
  tiempo <- proc.time() # Inicia el cronómetro
  
  Cartera<-read.table("Cartera.csv",header=T,sep=";",stringsAsFactors = F,dec=".") # Importamos el archivo
  
  analisis_estadistico<-function(p){
    
    # Cálculo de escenarios  
    simulacion_carteras<-data.frame(0,length(Cartera$Grupo),n_escenarios) # DF guardar simulaciones 
    perdidas_por_escenario<-1:n_escenarios # Creamos un vector vacío donde guardar las pérdidas.
    
    for (escenario in 1:n_escenarios){
      
      # Sacamos el valor de Y y los pesos de cada cartera
      y<- rnorm(1, mean = 0, sd = 1) # Sacamos el factor común de todas las exposiciones.
      Cartera$w<- rnorm(length(Cartera$Grupo), mean = 0, sd = 1) # pesos de cada cartera .
      
      # Sabiendo que Zi = raiz(p)*y + raiz(1-p)*w, calculamos Zi
      Cartera$zi<-0
      for (fila in 1:length(Cartera$zi)){
        Cartera$zi[fila]<-sqrt(p)*y+sqrt(1-p)*Cartera$w[fila]
      }
      
      # Calculamos las carteras que han hecho default
      for (fila in 1:length(Cartera$zi)){
        if (Cartera$zi[fila] < qnorm(Cartera$PD[fila], mean = 0, sd = 1)){ # qnorm punto en función de prob.
          
          # Guardamos las pérdidas de cada cartera para poder recuperar el escenario del 95%
          simulacion_carteras[fila,escenario]<-Cartera$EAD[fila]*Cartera$LGD[fila]
        }else{
          simulacion_carteras[fila,escenario]<-0
        }
      }
      perdidas_por_escenario[escenario]<-sum(simulacion_carteras[,escenario])
    }
    
    # Ordenamos los escenarios de menor a mayor pérdida.
    simulacion_carteras<-simulacion_carteras[,(order(perdidas_por_escenario))]
    
    # Recuperamos el escenario que corresponde al percentil 95% y lo metemos en el DF original.
    Cartera$escenario_95<- simulacion_carteras[,round(n_escenarios*0.95)]
    
    # Identificamos qué exposiciones dan las mayores pérdidas en dicho escenario
    Cartera[Cartera$escenario_95>0,]
    
    # Comparamos el peso de la perdida de cada exposición en el escenario 95 con el peso EAD.
    Cartera$porcentaje_EAD <- Cartera$EAD/sum(Cartera$EAD)
    Cartera$porcentaje_esc95 <- Cartera$escenario_95/sum(Cartera$escenario_95)
    
    # Hacemos la comparación a nivel de grupo y de segmento y guardamos los resultados.
    library(data.table)
    Cartera<-data.table(Cartera) # Convierto el data frame en un data table.
    
    # Guardamos el % EAD y % escenario 95
    resultados[[length(resultados)+1]] <- Cartera[,lapply(.SD,sum),by=Grupo,.SDcols=9:10] 
    resultados[[length(resultados)+1]] <- Cartera[,lapply(.SD,sum),by=Segmento,.SDcols=9:10] 
    return (resultados)
  }
  
  # Ejecutamos el programa
  resultados <- list()
  n_escenarios <- 50000
  p<- c(0.3,0.1)
  lapply(p,analisis_estadistico)
  
  print(proc.time()-tiempo)
})
