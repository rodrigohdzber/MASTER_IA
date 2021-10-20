# Consigue el histórico de las divisas de los últimos 60 días
# Recomendación: Banco de España  https://www.bde.es/webbde/es/estadis/infoest/tipos/tipos.html
# Tiempo objetivo: 30 minutos

# Euros
# Dólares de EEUU
# Yenes Japoneses
# Francos Suizos
# Libras Esterlinas
# Coronas Suecas
# Coronas Noruegas
# Reales Brasileños
# Dólares de Hong Kong
# Rupias Indonesias
# Peso Argentino

# Establecemos la ventana temporal
ventana<-60

# Nos bajamos las divisas del Banco de España.
url<-"http://www.bde.es/webbde/es/estadis/infoest/series/tc_1_1.csv"
divisas<-read.csv(file=url, stringsAsFactors = FALSE, header = F)

colnames(divisas)<-divisas[5,] # Ponemos el encabezado más coherente.
divisas<-divisas[7:dim(divisas)[1],] # Eliminamos las filas de títulos que no nos sirven.
divisas<-divisas[1:(dim(divisas)[1]-2),] # Eliminamos las dos últimas filas, que no nos sirven
divisas<-divisas[,c(1,2,3,4,5,13,15,20,23,24)] # Nos quedamos solo con las divisas que nos interesan.
# OJO. No tenemos todas las divisas que estamos buscando. Este sistema no nos vale al 100%

# Que las fechas estén con formato Español y los meses en texto va a suponer mucho trabajo manual.
# Nos queremos quedar con las divisas que estén entre hoy y la ventana con la que trabajmos.
meses_texto <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
meses_dig <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for (mes in 1:12){
  divisas[,1]<-gsub(meses_texto[mes],meses_dig[mes], divisas[,1])
}

divisas[,1]<- as.Date(divisas[,1], format = "%d %m %Y") # Convertimos el formato español a fecha internacional.

fecha_inicio<-Sys.Date()-ventana
fecha_fin<-Sys.Date()

# Filtramos por el rango de fechas que nos interesan, eliminando las que no.
divisas<-divisas[divisas$`DESCRIPCIÓN DE LAS UNIDADES`>=fecha_inicio,]
divisas<-divisas[divisas$`DESCRIPCIÓN DE LAS UNIDADES`<=fecha_fin,] 
colnames(divisas)<-c("Date","USD", "JPY", "CHF", "GBP", "SEK", "NOK", "BRL", "HKD", "IDR")




