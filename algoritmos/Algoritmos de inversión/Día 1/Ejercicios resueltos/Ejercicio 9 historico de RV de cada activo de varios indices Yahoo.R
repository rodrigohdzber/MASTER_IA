# Obtén el histórico (60 días) de cada activo de varios índices (usa los resultados del paso anterior)
# Recomendación: Yahoo Finance 
# Tiempo objetivo: 45 minutos
# Índices: "%5EHSI", "%5EIBEX"

library(RCurl)
library(stringr)
library(stringi)
library(rvest)

tickers_de_indices<-c("%5EHSI", "%5EIBEX")

obtener_datos_activo<-function(activo, fecha_inicial, fecha_fin, columna_fechas){
  
  # Obtengo las cookies de su web (curl) 
  curl = getCurlHandle(cookiejar="", verbose = F)
  res = getURI(paste("https://es.finance.yahoo.com/quote/",activo,"?ltr=1", sep=""), curl = curl) 
  
  # Buscamos CrumbStore dentro del código que nos hemos bajado.
  posicion<-unlist(str_locate_all(pattern ='CrumbStore', res))
  crumb<-unlist(strsplit(substr(res, posicion[1], posicion[2]+30), "\""))
  crumb<-crumb[nchar(crumb)==max(nchar(crumb))]
  crumb<-stri_escape_unicode(crumb) # Algunas veces, el crumb tiene una función de escape (un salto de página). Lo eliminamos
  
  # Construimos la url para bajarnos los datos
  url<-paste("https://query1.finance.yahoo.com/v7/finance/download/",activo,"?period1=",fecha_inicial,"&period2=",fecha_fin,"&interval=1d&events=history&crumb=",crumb, sep="")
  
  # En la llamada tenemos que pasar la url con el crumb y la cookie.
  info_activo = getURLContent(url, curl = curl, verbose = F)
  info_activo <- read.csv ( textConnection ( info_activo ) , header = T )
  
  return(info_activo)
}



# Establecemos el rango de fechas que queremos importar.
# Yahoo utiliza formato de fecha poxis - time unix
# (cantidad de segundos transcurridos desde la medianoche UTC del 1 de enero de 1970, teniendo cada día 86400 segundos)
# Convertimos la fecha a formato poxis

ventana <- 60
tiempo<-as.character(Sys.time())
substr(tiempo, 12, 24) <- "00:00:00"
tiempo<-as.POSIXct(tiempo)
fecha_fin <-ceiling(as.numeric(as.POSIXct(tiempo)))
fecha_inicial <- ceiling(as.numeric(as.POSIXct(tiempo - ventana*86400)))

for(indice in tickers_de_indices){
  
  # Construimos la url que contiene la información de los activos de cada índice.
  web<-read_html(paste("https://es.finance.yahoo.com/quote/",indice,"/components/", sep=""))
  
  datos_web<- web %>%
    html_nodes("td") %>% 
    html_text() 
  datos_web<-as.data.frame(datos_web) # Obtenemos cada celda de la tabla por separado. 
  datos_web<-as.data.frame(datos_web[1:(dim(datos_web)[1]-2),1]) # Eliminamos las últimas filas.
  
  if (dim(datos_web)[1]>1){ # Comprobamos si Yahoo publica algún activo del índice. Si la longitud es 1, no.
    
    # Reconstruimos la tabla. Tiene 6 columnas (simbolo, nombre de la empresa, último precio, cambio, cambio % y volumen.)
    # Nos interesa obtener el símbolo y el nombre de la empresa.
    # Ojo, queremos importar únicamente los activos que tengan cotización.
    
    activos_con_datos<-as.data.frame(datos_web[seq(3, dim(datos_web)[1], by=6),1])  
    activos_con_datos<-as.vector(t(activos_con_datos))
    indice_datos<-activos_con_datos!="" & activos_con_datos!="0,00"
    
    activos_a_importar<-as.data.frame(datos_web[seq(1, dim(datos_web)[1], by=6),1])
    activos_a_importar<-as.vector(t(activos_a_importar))
    activos_a_importar<-activos_a_importar[indice_datos]
    
    nombres_empresas<-as.data.frame(datos_web[seq(2, dim(datos_web)[1], by=6),1])
    nombres_empresas<-as.vector(t(nombres_empresas))
    nombres_empresas<-nombres_empresas[indice_datos]
    
    num_activos<-length(activos_a_importar)
    
    # Extraemos la divisa en la que cotizan los activos.
    datos_web<- web %>%
      html_nodes("span") %>% 
      html_text() 
    datos_web<-as.data.frame(datos_web)
    
    divisa<-""
    for (linea in 1:dim(datos_web)[1]){
      if (divisa==""){
        # Buscamos la subcadena "Divisa en" dentro del contenido de la web
        posicion<-unlist(str_locate_all(pattern ="Divisa en ", datos_web[linea,1]))
        
        if (length(posicion)>0){
          divisa <- substr(datos_web[linea,1], posicion[2] + 1, posicion[2] + 3)
          print(divisa)
        }
      }
    }
    
  } else {
    print(paste("El índice",indice,"no tiene activos descargables"))
  }  
  
  # Importamos los activos que hemos seleccionado.
  for (activo in activos_a_importar){
    print(paste("Descargando ",activo, sep= ""))
    info_activo<- obtener_datos_activo(activo, fecha_inicial, fecha_fin, columna_fechas)
    assign(activo,info_activo)
  }
}
