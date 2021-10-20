# Realmente ya hemos hecho lo más difícil. Ahora solo queda aplicar lo aprendido al resto de series que nos hemos descargado. 

# Objetivos: 	
  
  # Descargate los datos de las divisas desde Yahoo (no desde el Banco de España)
  # Detecta y limpia los datos (Null, NA…)
  # Encuentra y anula los split y contrasplit.
  #	Guarda los datos finales en un excel.

# Tiempo objetivo: 45 minutos
# Recuerda que la divisa la detectábamos cuando obteníamos los activos que cotizaban el en índice.


library(RCurl)
library(stringr)
library(stringi)
library(rvest)
library(zoo) 
library(xlsx)

tickers_de_indices<-c("%5EBVSP", "%5EDJI", "%5EFTSE", "%5EHSI", "%5EMXX", "%5EJKSE", "%5EMERV", "%5EOMXSPI", "%5EOSEAX", "%5ESSMI")

ventana <- 500

# Extraemos la función homogeneizar datos
homogeneizar<-function(datos, ventana){
  
  fechas<-as.Date(c(Sys.Date():(Sys.Date()-ventana)))# Creamos un vector con todas las fechas entre el día inicial y el final.
  dias_laborables<-!weekdays(fechas) %in% c("sábado", "domingo") # No nos interesan los sábados y domingos. Los localizamos.
  fechas<-fechas[dias_laborables] # Filtramos la serie de datos usando el índice booleano anterior.
  fechas<-as.data.frame(fechas)
  names(fechas)<-"Date"
  
  # Volcamos los datos en un DF homogeneizado, utilizando la columna de fechas como índice.
  datos_homogeneizados<-merge(fechas,datos,by=c("Date"),all.x = T)
  
  # Ordenamos los datos por fecha (al hacer el merge se ha invertido el orden)
  orden<-order(datos_homogeneizados$Date,decreasing = T)
  datos_homogeneizados<-datos_homogeneizados[orden,]
  
  # Localizamos los datos con NULL y los transformamos a NA
  datos_homogeneizados<-as.matrix(datos_homogeneizados)
  datos_homogeneizados[datos_homogeneizados=="null"] <- NA
  datos_homogeneizados<-as.data.frame(datos_homogeneizados)
  
  # Completamos los datos con NA (días en los que no hay registrada una cotización, pero que otras empresas sí cotizaron)
  # na.locf localiza los NA del DF y los sustituye por el valor de la fila anterior. Si la fila con NA es la 1ª deja el NA sin dar un error.
  datos_homogeneizados<-na.locf(datos_homogeneizados,na.rm = F)
  
  # Si los NA están en las primeras filas no hemos solucionado el problema. En principio no debería de haber más que dos (sábado y domingo), pero una empresa podría no cotizar lunes, martes... por lo que no conocemos el nº de potenciales NA a resolver.
  # Para resolver el problema, invertimos el orden del DF, aplicamos na.locf de nuevo y devolvemos el DF a su posición original.
  orden<-order(datos_homogeneizados$Date,decreasing = F)
  datos_homogeneizados<-datos_homogeneizados[orden,]
  datos_homogeneizados<-na.locf(datos_homogeneizados,na.rm = F)
  orden<-order(datos_homogeneizados$Date,decreasing = T)
  datos_homogeneizados<-datos_homogeneizados[orden,]
  
  return(datos_homogeneizados)
}

# Extraemos la función que intenta obtener los datos del índice.
obtener_indice<-function(url, curl){
  benchmark <- tryCatch(    
    {
      benchmark = getURLContent(url, curl = curl, verbose = F)  
    },
    
    error=function(cond) {
      message("No nos hemos podido descargar el indice")
      message("Este es el mensaje de error que ha dado:")
      message(cond)
      
      return("Sin datos")
    },
    
    finally={
    }
  )    
  return(benchmark)
}


# Función que ejecuta el algoritmo al completo.
algoritmo_lite<-function(indice,ventana=60){

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
  intentos<-0
  benchmark<-"Sin datos"
  while(benchmark=="Sin datos"){
    
    benchmark<-obtener_indice(url, curl)
    
    intentos<-intentos+1
    if(intentos>3){
      benchmark=="No descargable"
      print(c("Tras varios intentos no nos hemos podido descargar el índice ",tickers_de_indices[6]))
      return()
    }
    
    if(benchmark=="Sin datos"){
      print("Error en la descarga del benckmark, reintentamos")
      Sys.sleep(30)
    }
  }
  
  benchmark <- read.csv ( textConnection ( benchmark ) , header = T )
  
  # Hay veces que el último día se descarga dos veces en el DF. La segunda vez, la columna de volumen está a cero. 
  if (benchmark$Date[length(benchmark$Date)]==benchmark$Date[length(benchmark$Date)-1]){
    benchmark<-benchmark[1:(dim(benchmark)[1]-1),]
  }
  
  # Cogemos la columna de fechas del índice para usarla cuando no nos consigamos bajar algún activo.
  columna_fechas<-benchmark$Date
  columna_fechas<-as.vector(t(columna_fechas))
  
  # Obtenemos los activos que contiene cada índice.
  intentos<-0
  descarga<-"Sin datos"
  while(descarga=="Sin datos"){
    
    tryCatch(    
      {
        # Construimos la url que contiene la información de los activos de cada índice.
        web<-read_html(paste("https://es.finance.yahoo.com/quote/",indice,"/components/", sep=""))
        descarga<-"OK"
      },
      error=function(cond) {
        
        intentos<-intentos+1
        
        if(intentos>3){
          print("Tras varios intentos no nos hemos podido descargar los activos del índice. Dejamos de intentarlo.")
          return()
        }
        
        if(descarga=="Sin datos"){
          print("Error en la descarga de los activos del índice, reintentamos")
          Sys.sleep(30)
        }
        
      },
      finally={}
    ) 
  }
  
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
        }
      }
    }
    
  } else {
    print("El índice no tiene activos descargables")
  }  
  
  print(divisa)

  # Nos bajamos los datos de la divisa.
  divisa_a_bajar <- paste("EUR",divisa, sep="") 
  
  # Obtengo las cookies de su web (curl) 
  curl = XXXXXXXXXXXXXXXXXXXX
  res = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  
  # Buscamos CrumbStore dentro del código que nos hemos bajado.
  crumb<-XXXXXXXXXXXXXXXXXX
  
  # Construimos la url para bajarnos los datos
  url<-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  
  # En la llamada tenemos que pasar la url con el crumb y la cookie.
  intentos<-0
  descarga<-"Sin datos"
  while(descarga=="Sin datos"){
    
    tryCatch(    
      {
        info_divisa = XXXXXXXXXXXXXXXXXX
        info_divisa <- XXXXXXXXXXXXXXXXXXXXXXXXX
        descarga<-"OK"
      },
      error=function(cond) {
        
        intentos<-intentos+1
        
        if(intentos>3){
          print("Tras varios intentos no nos hemos podido descargar la divisa. Dejamos de intentarlo.")
          return()
        }
        
        if(descarga=="Sin datos"){
          print("Error en la descarga de la divisa, reintentamos")
          Sys.sleep(30)
        }
        
      },
      finally={}
    ) 
  }
  
  # Homogeneizamos los datos de la divisa
  XXXXXXXXXXXXXXXXXXXXXXX
  
  # Desmanipula las divisas
  XXXXXXXXXXXXXXXXXXXXXXX
  
  # Nos quedamos únicamente con la columna de cierre ajustado
  XXXXXXXXXXXXXXXXXXXXXXX
  
  # Guardamos el DF homogeneizado en su variable original.
  divisa<- datos_homogeneizados
  
  # Guardamos el DF de las divisas
  write.xlsx(divisa, "divisa.xlsx")
  
  return()
}

for(indice in tickers_de_indices){
  # Invocamos a la función pasándola los índices uno a uno.
  recomendacion<-algoritmo_lite(indice, ventana=60)
}

