# Realmente ya hemos hecho lo más difícil. Ahora solo queda aplicar lo aprendido al resto de series que nos hemos descargado. 

# Objetivos: 	
  
  # Utiliza el código que programaste para obtener los datos de renta fija.
  # Detecta y limpia los datos (Null, NA…)
  # Encuentra y anula los split y contrasplit.
  #	Guarda los datos finales en un excel.

# Tiempo objetivo: 30 minutos

library(RCurl)
library(stringr)
library(stringi)
library(rvest)
library(zoo) 
library(xlsx)

# Establecemos la ventana temporal
ventana<-60

# Nos bajamos los datos de Renta fija (ester y eonia) del Banco de España.
url<- "http://www.bde.es/webbde/es/estadis/infoest/series/ti_1_7.csv"

renta_fija <- read.csv(file=url, stringsAsFactors = FALSE, header = F)

renta_fija<-renta_fija[7:dim(renta_fija)[1],] # Eliminamos las filas de títulos que no nos sirven.
renta_fija<-renta_fija[,c(1:3)] # Nos quedamos solo con las columnas que nos interesan (fecha y eonia mundial)

# Utilizo el eonia mundial y no los T-Bill o Bund porque, en mi opinión, reflejan mucho mejor la rentabilidad de un activo libre de riesgo.
# Alemania quebró dos veces en el siglo pasado y EEUU evitó la suya saliendo de la paridad dólar-oro.

colnames(renta_fija)<-c("Date", "Ester", "Eonia") # Ponemos nombres a las columnas.

# Que las fechas estén con formato Español y los meses en texto va a suponer mucho trabajo manual.
meses_texto <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
meses_dig <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for (mes in 1:12){
  renta_fija[,1]<-gsub(meses_texto[mes],meses_dig[mes], renta_fija[,1])
}

renta_fija[,1]<- as.Date(renta_fija[,1], format = "%d %m %Y") # Convertimos el formato español a fecha internacional.

# Filtramos por el rango de fechas que nos interesan, eliminando las que no.
fecha_inicio<-Sys.Date()-ventana
fecha_fin<-Sys.Date()

renta_fija = renta_fija[!is.na(renta_fija$Date),] # Quitamos los datos con NA

renta_fija<-renta_fija[renta_fija$Date>=fecha_inicio,]
renta_fija<-renta_fija[renta_fija$Date<=fecha_fin,]

# Vemos que el DF tiene sábados y domingos, por lo que hace falta homogeneizar 
XXXXXXXXXXXXXXXXXXXXXXXXXX

# Identificamos los NA del DF. En este caso el Banco de España pone un guión bajo _
XXXXXXXXXXXXXXXXXXX

# Ordeno los datos de la renta fija igual que los de renta variable
XXXXXXXXXXXXXXXXXXXX

# Completamos los datos con NA (días en los que no hay registrada una cotización, pero que otras empresas sí cotizaron)
XXXXXXXXXXXXXXXXXXXXXX

# Si los NA están en las primeras filas no hemos solucionado el problema. 
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# convertimos a numero la renta fija para poder operar con ella
XXXXXXXXXXXXXXXXXXXXXXXX

# Quitamos la columna de fecha para hacer el data frame homogéneo con el resto de datos descargados.
XXXXXXXXXXXXXXXXXXXXXXXX 

# Ponemos los nombres a las filas para igualar el formato de todos los DF.
XXXXXXXXXXXXXXXXXXXX

# Guardamos la renta fija en un excel
write.xlsx(renta_fija, "renta_fija.xlsx")

