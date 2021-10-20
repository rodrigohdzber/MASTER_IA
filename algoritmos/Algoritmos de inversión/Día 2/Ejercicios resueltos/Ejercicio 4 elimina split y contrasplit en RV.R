# Ya tenemos los datos descargados, homogeneizados y limpios. Sin embargo es probable que tengamos que ajustar la serie. 
  # Debemos buscar los split y contrasplit, calcular su efecto y ajustar la serie temporal, tantas veces como hayan sucedido.
  # Una empresa puede valer 100€ teniendo 10 acciones de 10€ o teniendo 100 acciones de 1€. El precio de cotización varía drásticamente, pero el valor de la compañía no. 
  # Por desgracia, con los datos que nos hemos bajado, no disponemos ni del número de acciones, ni conocemos cuando han sucedido los split o contrasplit.
  # Por otro lado, dentro de los datos que nos bajamos, sí que tenemos el cierre ajustado por dividendos; por lo que este problema nos lo dan resuelto. 
  # Deberemos trabajar con ese cierre ajustado.

# Objetivos: 	
  #	Encuentra y anula los split y contrasplit
  #	Programa una función a la que pasar un índice y que guarde en excel los datos del índice y de sus activos (homogeneizados, limpios y ajustados). 
  # Es decir 6 excels: Uno para el índice y 5 (High, Low, Volume, Adj_close y Open) para los activos, donde se agrupan los datos de todos los activos del índice.  

# Tiempo objetivo: 45 minutos


library(RCurl)
library(stringr)
library(stringi)
library(rvest)
library(zoo) 
library(xlsx) # requiere rJava (usamos esta librería para guardar un excel). Si tienes problemas con su instalación, guarda la información en un CSV.

tickers_de_indices<-c("%5EIBEX")#"%5EBFX","%5EBVSP", "%5EDJI", "%5EFCHI", "%5EFTSE", "%5EGDAXI", "%5EHSI", "%5EIBEX", 
                      #"%5EMXX", "%5EJKSE", "%5EMERV", "%5EOMXSPI", "%5EOSEAX", "%5ESSMI", "%5ESTI")

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

# Función que intenta obtener los datos de cada activo
obtener_datos_activo<-function(activo, fecha_inicial, fecha_fin, columna_fechas){
  info_activo <- tryCatch(
    {
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
      
      # No es necesario que indique el valor de retorno a través de `return ()` en el código.
    },
    
    error=function(cond) {
      message(paste("No nos hemos podido descargar el activo :", activo))
      message("Este es el mensaje de error que ha dado:")
      message(cond)
      
      # Generamos un DF a ceros, con el tamaño adecuado, para que los siguientes pasos no den error.
      info_activo<-as.data.frame(matrix(0,nrow=length(columna_fechas),ncol=7,byrow=F)) 
      info_activo$V1<-columna_fechas
      colnames(info_activo)<-c("Date", "Open", "High", "Low", "Close", "Adj.Close", "Volume")
      
      return(info_activo)
    },
    
    warning=function(cond) {
      message(paste("Nos ha dado un warning con el siguiente activo :", activo))
      message("Este el el mensaje de warning que ha dado:")
      message(cond)
      
      # Generamos un DF a ceros, con el tamaño adecuado, para que los siguientes pasos no den error.
      info_activo<-as.data.frame(matrix(0,nrow=ventana,ncol=7,byrow=F)) 
      colnames(info_activo)<-c("Date", "Open", "High", "Low", "Close", "Adj.Close", "Volume")
      
      return(info_activo)
    },
    finally={
    }
  )    
  return(info_activo)
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
    # return(NA)
  }  
  
  # Importamos los activos que hemos seleccionado.
  for (activo in activos_a_importar){
    
    intentos<-0
    descarga<-"Sin datos"
    while(descarga=="Sin datos"){
      
      info_activo<- obtener_datos_activo(activo, fecha_inicial, fecha_fin, columna_fechas)
      
      # Comprobamos que nos hemos podido descargar el activo.
      if(sum(colSums(info_activo[,2:dim(info_activo)[2]] != 0))>0){
        descarga<-"OK"
      }
      
      intentos<-intentos+1
      
      if(intentos>3){
        print(c("Tras varios intentos no nos hemos podido descargar",activo,"Dejamos de intentarlo."))
        descarga<-"OK"
      }
      
      if(descarga=="Sin datos"){
        print(c("Error en la descarga del activo", activo, "reintentamos"))
        Sys.sleep(10)
      }
    }
    
    assign(activo,info_activo)
  }
  
  # Homogeneizamos los datos (que todos los DF tengan el mismo número de filas / días y que estén puestos en las mismas líneas)
  for (activo in activos_a_importar){
    
    datos<-get(activo)    
      # Comprobamos si nos hemos podido descargar el activo, o si nos hemos descargado más de un único día.
      if (dim(datos)[1]>1){
        # Hay veces que el último día se descarga dos veces en el DF. La segunda vez, la columna de volumen está a cero. 
        if (datos$Date[length(datos$Date)]==datos$Date[length(datos$Date)-1]){
          datos<-datos[1:(dim(datos)[1]-1),]
        }
                 
        # Hay veces que hay datos duplicados entre medias del vector. Eliminamos las filas duplicadas.
        if(length(duplicated(datos$Date)[duplicated(datos$Date)==TRUE])>0){
          datos<-datos[!duplicated(datos$Date),]
        }
      }
               
      datos[,1]<-as.Date(datos[,1])
      datos_homogeneizados<-homogeneizar(datos, ventana)
               
      # Guardamos el DF homogeneizado en su variable original.
      assign(activo,datos_homogeneizados)
  }
  
  # Creamos los DF donde guardaremos los resultados (excel puede contener algo más de 16.000 columnas)
  Open<-as.data.frame(matrix(0,nrow=dim(datos_homogeneizados)[1],ncol=length(activos_a_importar),byrow=F)) 
    names(Open)<-activos_a_importar[1:length(activos_a_importar)]
  High<-as.data.frame(matrix(0,nrow=dim(datos_homogeneizados)[1],ncol=length(activos_a_importar),byrow=F))
    names(High)<-activos_a_importar[1:length(activos_a_importar)]
  Low<-as.data.frame(matrix(0,nrow=dim(datos_homogeneizados)[1],ncol=length(activos_a_importar),byrow=F))
    names(Low)<-activos_a_importar[1:length(activos_a_importar)]
  Volume<-as.data.frame(matrix(0,nrow=dim(datos_homogeneizados)[1],ncol=length(activos_a_importar),byrow=F))
    names(Volume)<-activos_a_importar[1:length(activos_a_importar)]
  Adj_close<-as.data.frame(matrix(0,nrow=dim(datos_homogeneizados)[1],ncol=length(activos_a_importar),byrow=F)) 
    names(Adj_close)<-activos_a_importar[1:length(activos_a_importar)]
  
  # Guardamos los datos en los DF finales
  contador<-1
  for (activo in activos_a_importar){
    datos <-get(activo)
    Open[,contador]<-datos[,2] # usamos el contador porque open$activo <- activo$open no funciona, ni siquiera usando asign con gsub.
    High[,contador]<-datos[,3]
    Low[,contador]<-datos[,4]
    Adj_close[,contador]<-datos[,6] 
    Volume[,contador]<-datos[,7]
    
    contador<-contador+1
  }
  
  # Nos quedamos con el cierre ajustado, el volumen, el alto y bajo para trabajar.
  # Transformamos la información de string a numérica para poder realizar cálculos con ella.
  Adj_close<-data.frame(lapply(Adj_close,as.numeric))
  rownames(Adj_close) <- datos_homogeneizados[,1] 
  Volume<-data.frame(lapply(Volume,as.numeric))
  rownames(Volume) <- datos_homogeneizados[,1] 
  High<-data.frame(lapply(High,as.numeric))
  rownames(High) <- datos_homogeneizados[,1] 
  Low<-data.frame(lapply(Low,as.numeric))
  rownames(Low) <- datos_homogeneizados[,1] 
  Open<-data.frame(lapply(Open,as.numeric))
  rownames(Open) <- datos_homogeneizados[,1]
  
  # Los datos de cierre ajustados no están bien desmanipulados. 
  # calculamos la rentabilidad de los activos
  rentabilidades<-as.data.frame(matrix(0,nrow=dim(Adj_close)[1],ncol=dim(Adj_close)[2],byrow=F)) 
  colnames(rentabilidades)<-colnames(Adj_close)
  rownames(rentabilidades) <- datos_homogeneizados[,1] 
  
  # Ojo, los activos que no nos hemos podido descargar tienen precio de 0 €. La rentabilidad 0/0 dará un error que tenemos que evitar.
  rentabilidades[1:(dim(Adj_close)[1]-1),1:dim(Adj_close)[2]] <- log(Adj_close[1:(dim(Adj_close)[1]-1),1:dim(Adj_close)[2]]/Adj_close[2:dim(Adj_close)[1],1:dim(Adj_close)[2]])
  rentabilidades[dim(rentabilidades)[1],]<-rentabilidades[dim(rentabilidades)[1]-1,] 
  rentabilidades[,colSums(Adj_close)==0]<-0
  
  # Buscamos split y contrasplit, de existir, hay que "desmanipular" la serie de precios para hacerlos homogéneos entre sí.
  # Si la rent log es superior al 0.69 el precio se ha duplicado en un día, probablemente han realizado un contrasplit de la acción.
  # Si la rent log es inferior al -0.69 el precio se ha dividido entre do en un día, probablemente han realizado un split de la acción.
  
  #             Precio  Rentabilidad  Multiplicador       Precio desmanipulado
  # 25/8/16    100        -2.302        1                   100
  # 24/8/16   1000         2.302        100/1000            100
  #	23/8/16    100                      100/1000*1000/100   100
  
  for (activo in 2:dim(rentabilidades)[2]){
    
    # Comprobamos si alguna rentabilidad de la serie es mayor o menor a 0.69 y calculamos los multiplicadores.
    indice_booleano<-rentabilidades[,activo]>0.69 | rentabilidades[,activo]<(-0.69)
    rentabilidades_excesivas<-rentabilidades[indice_booleano,activo]
    
    if (length(rentabilidades_excesivas)>0){ # Si la serie temporal no tiene ningún split o contra split no hay nada que hacer. 
      multiplicador<-as.data.frame(matrix(1,nrow=dim(Adj_close)[1],ncol=1,byrow=F))
      
      for(dia in 2:dim(Adj_close)[1]){ # Recorremos las rentabilidades diarias para construir los multiplicadores.
        if (rentabilidades[dia-1,activo]>0.69){ # Han hecho un contra split. Ajustamos el multiplicador.
          multiplicador[dia,1]<-multiplicador[dia-1,1]*(Adj_close[dia-1,activo]/Adj_close[dia,activo])
        } else if(rentabilidades[dia-1,activo]<(-0.69)){ # Han hecho un split. Ajustamos el multiplicador.
          multiplicador[dia,1]<-multiplicador[dia-1,1]*(Adj_close[dia-1,activo]/Adj_close[dia,activo])
        } else { # No han hecho nada, el multiplicador no varía.
          multiplicador[dia,1]<-multiplicador[dia-1,1]
        }
      }
      # Ajustamos los datos utilizando los multiplicadores. 
      Adj_close[,activo]<-Adj_close[,activo]*multiplicador
      High[,activo]<-High[,activo]*multiplicador
      Low[,activo]<-Low[,activo]*multiplicador
      Open[,activo]<-Open[,activo]*multiplicador
    }
  }
  
  # Guardamos los DF finales en excels.
  write.xlsx(High, "High.xlsx")
  write.xlsx(Low, "Low.xlsx")
  write.xlsx(Volume, "Volume.xlsx")
  write.xlsx(Adj_close, "Adj_close.xlsx")
  write.xlsx(Open, "Open.xlsx")
  
  return()
}

for(indice in tickers_de_indices){
  # Invocamos a la función pasándola los índices uno a uno.
  recomendacion<-algoritmo_lite(indice, ventana=60)
}

