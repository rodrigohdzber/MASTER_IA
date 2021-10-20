# Obtén el histórico (60 días) de cada activo de varios índices (usa el código del día anterior)
# Recomendación: Yahoo Finance 
# Tiempo objetivo: 45 minutos
# La clave está en qué tenemos que hacer cuando un activo de error en la descarga...

library(RCurl)
library(stringr)
library(stringi)
library(rvest)

tickers_de_indices<-c("%5EBFX","%5EBVSP", "%5EDJI", "%5EFCHI", "%5EFTSE", "%5EGDAXI", "%5EHSI", "%5EIBEX", 
                      "%5EMXX", "%5EJKSE", "%5EMERV", "%5EOMXSPI", "%5EOSEAX", "%5ESSMI", "%5ESTI")

ventana <- 60

# Extraemos la función que intenta obtener los datos del índice.
obtener_indice<-function(url, curl){

  benchmark = XXXXXXXXXXXXX
    
  return(benchmark)
}

# Función que intenta obtener los datos de cada activo
obtener_datos_activo<-function(activo, fecha_inicial, fecha_fin, columna_fechas){

    # Obtengo las cookies de su web (curl) 
    curl <- XXXXXXXXXXXXX
    res <- XXXXXXXXXXXXXX
      
    # Buscamos CrumbStore dentro del código que nos hemos bajado.
    crumb<- XXXXXXXXXXXXXXX
      
    # Construimos la url para bajarnos los datos
    url<- XXXXXXXXXXXXXXXXXXXXXXX
      
    # En la llamada tenemos que pasar la url con el crumb y la cookie.
    info_activo <- XXXXXXXXXXXXXXXXXXXXX
      
  return(info_activo)
}


# Ejecutamos el algoritmo
for(indice in tickers_de_indices){
  
  print(indice)
  
  # Extraemos el índice del listado de índices
  benchmark<-indice
  
  # Establecemos el rango de fechas que queremos importar.
  tiempo<-as.character(Sys.time())
  substr(tiempo, 12, 24) <- "00:00:00"
  tiempo<-as.POSIXct(tiempo)
  fecha_fin <-ceiling(as.numeric(as.POSIXct(tiempo)))
  fecha_inicial <- ceiling(as.numeric(as.POSIXct(tiempo - ventana*86400)))
  
  # Obtengo las cookies de su web (curl) 
  curl = getCurlHandle(cookiejar="", verbose = F)
  res = getURI(paste("https://es.finance.yahoo.com/quote/",benchmark,"?ltr=1", sep=""), curl = curl) 
  
  # Buscamos CrumbStore dentro del código que nos hemos bajado.
  posicion<-unlist(str_locate_all(pattern ='CrumbStore', res))
  crumb<-unlist(strsplit(substr(res, posicion[1], posicion[2]+30), "\""))
  crumb<-crumb[nchar(crumb)==max(nchar(crumb))]
  crumb<-stri_escape_unicode(crumb) # Algunas veces, el crumb tiene una función de escape (un salto de página). Lo eliminamos.  
  
  # Construimos la url para bajarnos los datos
  url<-paste("https://query1.finance.yahoo.com/v7/finance/download/",benchmark,"?period1=",fecha_inicial,"&period2=",fecha_fin,"&interval=1d&events=history&crumb=",crumb, sep="")
  
  # En la llamada tenemos que pasar la url con el crumb y la cookie.
  benchmark<-obtener_indice(url, curl)
  benchmark <- read.csv ( textConnection ( benchmark ) , header = T )
  
  # Cogemos la columna de fechas del índice para usarla cuando no nos consigamos bajar algún activo.
  XXXXXXXXXXXXXXXXXXXXXXXXXXX
  
  # Obtenemos los activos que contiene cada índice.
  web<-read_html(paste("https://es.finance.yahoo.com/quote/",indice,"/components/", sep=""))
  datos_web<- web %>%
    html_nodes("td") %>% 
    html_text() 
  datos_web<-as.data.frame(datos_web) # Obtenemos cada celda de la tabla por separado. 
  datos_web<-as.data.frame(datos_web[1:(dim(datos_web)[1]-2),1]) # Eliminamos las últimas filas.
  
  if (dim(datos_web)[1]>1){ # Comprobamos si Yahoo publica algún activo del índice. Si la longitud es 1, no.
    
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
        }
      }
    }
    
  } else {
    print("El índice no tiene activos descargables")
  }  
  
  # Importamos los activos.
  for (activo in activos_a_importar){
    
    info_activo<- obtener_datos_activo(activo, fecha_inicial, fecha_fin, columna_fechas)
    assign(activo,info_activo)
  }
}
