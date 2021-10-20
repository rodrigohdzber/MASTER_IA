# Importa los datos del fichero DAX.csv y calcula el Alpha de Jensen para todos los activos del índice. 
# Verás que en el mismo archivo encontrarás también los datos del índice (DAX) y de la renta fija (Bund) alemán.

  # α=Rentabilidad_cartera-(Rentabildad_activo_libre_riesgo+ β (Rentabilidad_mercado- Rentabildad_activo_libre_riesgo))
  # β= covarianza (Rentabilidad_cartera, Rentabilidad_mercado) / varianza (Rentabilidad_mercado)

# El alfa de Jensen es una medida de calidad sobre la gestión realizada (ya sea en un fondo, cartera de activos, o una única empresa). 
# Indica el exceso de rentabilidad obtenida para un nivel de riesgo determinado. 
# El Alfa explica la diferencia entre la rentabilidad esperada, es decir, la que corresponde al riesgo sistemático asumido, 
  # y la realmente obtenida por el gestor. En función de que el gestor supere, iguale o esté por debajo 
  # del rendimiento esperado tendrá un Alfa positivo, neutro o negativo.

#tiempo de ejecucion 0,20 seg 0,17 seg

library(profvis)
library(roll)  #para hacerlo como en python
profvis({
  
  tiempo <- proc.time()
  # Cargamos los datos y extraemos por un lado los activos, la renta fija y el índice Dax
  datos <- read.csv("DAX.csv", sep=';',stringsAsFactors = FALSE) 
  
  n_activos <- length(datos)-2
  activos<- datos[,2:n_activos] # Extraemos los activos quitando las dos últimas columnas y la primera.
  
  BUND <- datos[,length(datos)] # Extraemos la renta fija.
  DAX <- datos[,length(datos)-1] # Extraemos el índice.
  
  #Creamos matrices vacias para cov var y beta igual que para rent_activos bund dax y alpha.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  rent_BUND <- rep(0,length(BUND))
  rent_DAX <- rep(0,length(DAX))
  var_Dax = rep(0,length(DAX))
  cov_act_indi = as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F))
  beta = as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F))
  alpha_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  #renombramos  las columnas con el nombre de los activos y las filas con las fechas
  names(alpha_activos) = names(activos)
  rownames(alpha_activos) = datos[,1]
  
  # Calculamos la rentabilidad de los 10 primeros días para usarlos como datos iniciales.
  #rent_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  #names(rent_activos)<-names(activos) # Ponemos nombres a las columnas
  #rownames(rent_activos) <- datos[,1] # Ponemos nombres a las filas.
  
  #rent_BUND <- rep(0,length(BUND)) # Genero un vector donde guardaré las rentabilidades de la renta fija.
  #rent_DAX <- rep(0,length(DAX)) # Genero un vector donde guardaré las rentabilidades del índice.
  
  #alpha_activos<-as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  #names(alpha_activos)<-names(activos) # Ponemos nombres a las columnas
  #rownames(alpha_activos) <- datos[,1] # Ponemos nombres a las filas.
  
  #calculamos las rentabilidades de los activos, de la r fija y del dax(apuntes clase sabado)
  
  rent_activos[2:dim(activos)[1],1:length(activos)] = log(activos[2:dim(activos)[1],1:length(activos)]/activos[(2:dim(activos)[1])-1,1:length(activos)])
  rent_BUND[2:dim(activos)[1]] = log(BUND[2:dim(activos)[1]]/BUND[(2:dim(activos)[1])-1])
  rent_DAX[2:dim(activos)[1]] = log(DAX[2:dim(activos)[1]]/DAX[(2:dim(activos)[1])-1])
  
  
  #for (dia in 2:10){ # Calculamos la rentabilidad de los 10 primeros días (activos, Bund y Dax)
  #  for (activo in 1:length(activos)){
  #    rent_activos[dia,activo] <- log(activos[dia,activo]/activos[dia-1,activo])
  #  }
  #  rent_BUND[dia] <- log(BUND[dia]/BUND[dia-1])
  #  rent_DAX[dia] <- log(DAX[dia]/DAX[dia-1])
  #}
  
  # Teniendo ya datos históricos, calculamos los Alphas para cada día con un bucle.
  #barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = dim(activos)[1], width=300) 
  
  #Calcular la varianza del DAX
  window = 30
  varianza_DAX = roll_var(rent_DAX,width = window)
  
  
  #calculamos la covarianza
  
  for (activo in 1:length(activos)){
    cov_act_indi[,activo] = roll_cov(rent_DAX,rent_activos[,activo],width = window)  #sin el bucle no se puede
  }
  
    # Calculamos la rentabilidad de los activos, la renta fija y el índice para una fecha concreta.
   # for (activo in 1:length(activos)){
   #   rent_activos[dia,activo] <- log(activos[dia,activo]/activos[dia-1,activo])
   # }
   # rent_BUND[dia] <- log(BUND[dia]/BUND[dia-1])
   # rent_DAX[dia] <- log(DAX[dia]/DAX[dia-1])
    
    # Calculamos la varianza del DAX
    #varianza_ind<-var(rent_DAX[2:dia]) # Excluimos la 1ª línea porque contiene ceros. 
    
    #for (activo in 1:length(activos)){
      # Calculamos la covarianza entre el activo y el índice
      #cov_act_ind <- cov(rent_activos[2:dia,activo],rent_DAX[2:dia])
      
  # Sacamos la Beta del activo. β=cov(Rc,Rm)/σRm
  
  beta[2:dim(activos)[1],1:length(activos)] = cov_act_indi[2:dim(activos)[1],1:length(activos)]/varianza_DAX[2:length(rent_DAX)]    
  
      #if (varianza_ind==0){
      #  beta <-0
      #}else{
      #  beta <- cov_act_ind/varianza_ind
      #}
      
  # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
  alpha_activos[2:dim(activos)[1],1:length(activos)] = rent_activos[2:dim(activos)[1],1:length(activos)]-(rent_BUND[2:dim(activos)[1]]+beta[2:dim(activos)[1],1:length(activos)]*(rent_DAX[2:dim(activos)[1]]-rent_BUND[2:dim(activos)[1]]))
  
    
    #setWinProgressBar(barra_progreso, dia, title=paste(round(dia/dim(activos)[1]*100,0), "% realizado"))
  
  #close(barra_progreso)
  
  print(proc.time()-tiempo)
})
