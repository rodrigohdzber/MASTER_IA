# Importa los datos del fichero DAX.csv y calcula el Alpha de Jensen para todos los activos del índice. 
# Verás que en el mismo archivo encontrarás también los datos del índice (DAX) y de la renta fija (Bund) alemán.

  # α=Rentabilidad_cartera-(Rentabildad_activo_libre_riesgo+ β (Rentabilidad_mercado- Rentabildad_activo_libre_riesgo))
  # β= covarianza (Rentabilidad_cartera, Rentabilidad_mercado) / varianza (Rentabilidad_mercado)

# Rentabilidad_mercado => DAX

# El alfa de Jensen es una medida de calidad sobre la gestión realizada (ya sea en un fondo, cartera de activos, o una única empresa). 
# Indica el exceso de rentabilidad obtenida para un nivel de riesgo determinado. 
# El Alfa explica la diferencia entre la rentabilidad esperada, es decir, la que corresponde al riesgo sistemático asumido, 
  # y la realmente obtenida por el gestor. En función de que el gestor supere, iguale o esté por debajo 
  # del rendimiento esperado tendrá un Alfa positivo, neutro o negativo.

library(profvis)
profvis({
  
  tiempo = proc.time()
  
  # Cargamos los datos y extraemos por un lado los activos, la renta fija y el índice Dax
  datos = read.csv(
    file = 'DAX.csv',
    header = TRUE,
    sep = ';')
  
  # Prueba
  #datos = datos[1:25,]
  
  n_activos = length(datos)-2
  activos = datos[,2:n_activos] # Extraemos los activos quitando las dos últimas columnas y la primera.
  
  # Prueba
  #activos = activos[,1:5]
  #n_activos = 5
  
  BUND = datos[,length(datos)] # Extraemos la renta fija.
  DAX = datos[,length(datos)-1] # Extraemos el índice.
  rango_fechas = as.Date(datos[,1], format = "%d/%m/%Y") # Formato de fecha
  
  # Preparamos las estructuras de datos
  rent_activos = as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(rent_activos) = names(activos) # Ponemos nombres a las columnas
  rownames(rent_activos) = datos[,1] # Ponemos nombres a las filas.
  
  rent_BUND = rep(0,length(BUND)) # Genero un vector donde guardaré las rentabilidades de la renta fija.
  rent_DAX = rep(0,length(DAX)) # Genero un vector donde guardaré las rentabilidades del índice.
  
  alpha_activos = as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(alpha_activos) = names(activos) # Ponemos nombres a las columnas
  rownames(alpha_activos) = datos[,1] # Ponemos nombres a las filas.
  
  beta_activos = as.data.frame(matrix(0,nrow=dim(activos)[1],ncol=length(activos),byrow=F)) 
  names(beta_activos) = names(activos) # Ponemos nombres a las columnas
  rownames(beta_activos) = datos[,1] # Ponemos nombres a las filas.
  
  # Calculamos la rentabilidad de los activos, Bund y Dax
  rent_activos[(2:dim(activos)[1]), 1:length(activos)] = log(activos[(2:dim(activos)[1]), 1:length(activos)]/activos[1:(dim(activos)[1]-1),1:length(activos)])
  rent_BUND[(2:length(BUND))] = log(BUND[(2:length(BUND))]/BUND[(1:length(BUND))-1])
  rent_DAX[(2:length(DAX))] = log(DAX[(2:length(DAX))]/DAX[(1:length(DAX))-1])
  
  # Cálculos con ventana móvil
  library(roll)
  windowSize = 30
  
  # Prueba
  #windowSize = 2
  
  # Calculamos varianza usando ventana movil de 30 muestras
  # Solo tomamos las filas donde tenemos datos (del 1 al día actual)
  varianza_ind = roll_var(
    x = rent_DAX[(1:length(DAX))], 
    width = windowSize)
  varianza_ind = as.data.frame(varianza_ind)
  rownames(varianza_ind) = datos[1:(dim(datos)[1]),1] # Ponemos nombres a las filas.
  
  # Calculamos covarianza usando ventana movil de 30 muestras
  # Solo tomamos las filas donde tenemos datos (del 1 al día actual)
  cov_act_ind = roll_cov(
    x = as.matrix(rent_activos[(2:dim(rent_activos)[1]),1:length(rent_activos)]),
    y = as.vector(rent_DAX[2:length(DAX)]), 
    width = windowSize)
  cov_act_ind = as.data.frame(cov_act_ind[1:length(rent_activos),,1:(dim(rent_activos)[1]-1)])
  cov_act_ind = as.data.frame(t(cov_act_ind))
  cov_act_ind[is.na(cov_act_ind)] = 0 # Ponemos a 0 los NA
  rownames(cov_act_ind) = datos[1:(dim(datos)[1]-1),1] # Ponemos nombres a las filas.

  # Para poder hacer el cálculo de la beta de todos los días para todos los activos
  # de una sola vez, nos aprovechamos del producto matricial

  # Preparamos para calcular la Beta convitiendo la varianza del índice en matriz diagonal
  # beta = cov_act_ind / varianza_ind
  varianza_ind_diag = varianza_ind[,1] ** (-1) # Calculamos el inverso de cada elemento (1 / elem)
  varianza_ind_diag[varianza_ind_diag==Inf | is.na(varianza_ind_diag)] = 0 # Los infinitos y NA los hacemos ceros
  varianza_ind_diag = diag(varianza_ind_diag) # Matriz con los valores en la diagonal y el resto ceros
  
  # Calculamos la beta de todos los activos para todos los días
  cov_act_ind_t = t(cov_act_ind) # Trasponemos para adecuar las dimensiones a la operación matricial
  cov_act_ind_m = as.matrix(cov_act_ind_t) # A matriz para poder hacer producto matricial
  varianza_ind_diag_m = as.matrix(varianza_ind_diag[1:(dim(varianza_ind_diag)[1]-1), 1:(dim(varianza_ind_diag)[2]-1)]) # Seleccionamos los datos que nos interesan
  beta_activos = cov_act_ind_m %*% varianza_ind_diag_m # Producto matricial
  beta_activos = as.data.frame(t(beta_activos)) # Ponemos activos como columnas
  rownames(beta_activos) = datos[1:(dim(datos)[1]-1),1] # Ponemos nombres a las filas.
  
  # Calculamos el alpha de cada activo
  dias = dim(beta_activos)[1]
  for (activo in 1:length(activos))
  {
    # Calculamos el Alpha del activo. 
    # α=Rc-(Rf+β(Rm-Rf))
    # α=Rentabilidad_cartera-(Rentabildad_activo_libre_riesgo+ β (Rentabilidad_mercado- Rentabildad_activo_libre_riesgo))
    alpha_activos[1:dias,activo] = rent_activos[1:dias,activo] - (rent_BUND[1:dias] + beta_activos[1:dias,activo] * (rent_DAX[1:dias] - rent_BUND[1:dias]))
  }

  print(proc.time()-tiempo)
})

#write.csv(alpha_activos, "alpha_activos_v2.1.csv")