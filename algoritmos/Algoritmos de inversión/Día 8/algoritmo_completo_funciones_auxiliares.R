
# Obtenemos la composición del índice
library(rvest)

composicion_indice<-function(url){
  
  web<-read_html(url)
  
  # Comprobamos si los activos solo están en la primera hoja.
  datos_web<- web %>%
    html_nodes(".pagination")%>% 
    html_text() 
  num_paginas<-datos_web
  
  # Descargamos los nombres de los activos de la página.
  datos_web<- web %>%
    html_nodes(".plusIconTd") %>% 
    html_text() 
  nombre_activos<-as.vector(datos_web)
  
  if (length(num_paginas)>0) {
    num_paginas<-max(as.numeric(num_paginas))
    for (pagina in 2:num_paginas){
      
      url_adaptada<-paste(url,"/", pagina,sep="")
      web<-read_html(url_adaptada)
      
      datos_web<- web %>%
        html_nodes(".plusIconTd") %>% 
        html_text() 
      nombre_activos<-c(nombre_activos, as.vector(datos_web))
    }
  }
  
  return(nombre_activos)
}


# Obtenemos la url de cada activo
library(rvest)
library(pipeR) 

obtener_url_activos<-function(indice, nombre_activos){
  
  datos_web<-read_html(indice)%>>% html_nodes("a") %>>% html_attr("href") # %>>% will be faster than %>%
  
  # Encontramos el primer activo (en minúsculas)
  datos_web<-as.data.frame(datos_web)
  nombre<-tolower(unlist(strsplit(nombre_activos[1], " "))[1])
  
  inicio<-which(grepl(nombre, datos_web[,1]) == TRUE)[1]
  fin<-inicio+length(nombre_activos)-1
  
  links<-as.character(datos_web[inicio:fin,1])
  links<-as.data.frame(paste("https://es.investing.com", links, "-historical-data", sep="")) #https://es.investing.com/equities/aena-aeropuertos-sa-historical-data
  colnames(links)<-"Links"
  
  return(links)   
}


# Obtenemos el ticker del activo.
obtener_ticker<-function(datos_web){
  ticker<- datos_web %>%
    html_nodes("h1") %>% 
    html_text() 
  
  inicio<-regexpr("\\(", ticker)[1]
  fin<-regexpr("\\)", ticker)[1]
  ticker<-substr(ticker, inicio+1, fin-1)
  
  return(ticker)
}


# Obtenemos la divisa del activo.
obtener_divisa<-function(datos_web){
  divisa<- datos_web %>%
    html_nodes("div.bottom.lighterGrayFont.arial_11") %>% 
    html_text() 
  
  inicio<-regexpr("Valores en ", divisa)[1]+11
  fin<-regexpr(" ", substr(divisa, inicio, nchar(divisa)))[1]+inicio
  divisa<-substr(divisa, inicio, fin-2)
  
  return(divisa)
}


# Obtenemos el ISIN de activo.
obtener_isin<-function(datos_web){
  isin<- datos_web %>%
    html_nodes("div.right") %>% 
    html_text() 
  
  isin<-gsub("[\r\n]", "", isin) # Limpio lo que me he descargado.
  isin<-gsub(" ", "", isin)[1]
  
  inicio<-regexpr("ISIN:", isin)[1]+5
  fin<-inicio+11 # El ISIN siempre tiene 12 caracteres.
  isin<-substr(isin, inicio, fin)
  
  return(isin)
}


# Obtenemos las cotizaciones diarias de Investing
library(httr)
library(rvest)
library(stringr)

descargar_cotizaciones_diarias_investing<-function(url_activo, fecha_inicio, fecha_fin){
  
  fecha_inicio<-as.Date(fecha_inicio, "%d/%m/%Y")
  fecha_fin<-as.Date(fecha_fin, "%d/%m/%Y")
  
  uri <-"https://es.investing.com/instruments/HistoricalDataAjax" # Es el endpoint con la información a la que atacar (PHPSESSID y StickySession)
  
  headers = c(
    'Origin' = "https://es.investing.com",
    'User-Agent' = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36",
    'Content-Type' = "application/x-www-form-urlencoded",
    'Accept' = "text/plain, */*;",
    'Referer' = url_activo,
    'X-Requested-With' = "XMLHttpRequest"
  )
  
  # Abrimos la sesión.
  r <- GET(url_activo)
  info_sesion<-cookies(r)
  PHPSESSID<-info_sesion$value[info_sesion$name=="PHPSESSID"]
  StickySession<-info_sesion$value[info_sesion$name=="StickySession"]
  
  cotizaciones<-tryCatch({
    
    # Buscamos los ID del activo para descargarnos el correcto.
    datos_web<-read_html(url_activo)
    posicion<-unlist(str_locate_all(pattern ='smlId', datos_web))
    smlId<-unlist(strsplit(substr(datos_web, posicion, posicion+18), "\"")) 
    smlId<-as.numeric(str_extract_all(smlId, "[0-9]+")[[1]])
    
    #posicion<-unlist(str_locate_all(pattern ='pairId', datos_web))
    pairId<-unlist(strsplit(substr(datos_web, posicion-28, posicion-1), "\"")) 
    pairId<-as.numeric(str_extract_all(pairId, "[0-9]+")[[1]])
    
    # Construimos el payload metiendo pairId y smlID (que identifica el activo) y las fechas de inicio y fin de la descarga.
    payload = paste("curr_id=",pairId,"&smlID=",smlId,"&st_date=",substr(fecha_inicio, 9, 10),"%2F",substr(fecha_inicio, 6, 7),"%2F",substr(fecha_inicio, 1, 4),"&end_date=",substr(fecha_fin, 9, 10),"%2F",substr(fecha_fin, 6, 7),"%2F",substr(fecha_fin, 1, 4),"&interval_sec=Daily&action=historical_data", sep="")
    
    res <- POST(uri,
                set_cookies(PHPSESSID = PHPSESSID, 
                            StickySession = StickySession), 
                add_headers(.headers = headers),
                body = payload
    ) 
    
    datos_web<-read_html(res)
    cotizaciones<- datos_web %>%
      html_nodes("#curr_table") %>% 
      html_table()
    cotizaciones<-as.data.frame(cotizaciones)

  }, error=function(cond) {
    cotizaciones<-NA
    
  }, finally = {
  })
  
  return(cotizaciones)
}


# Reconstruimos la tabla de información de los activos (nombre, ticker, ISIN, url)
construimos_tabla_activos<-function(activo,info_activos, indice, url){
  
  Sys.sleep(sample(1:5, 1))
  tryCatch({
    datos_web<-read_html(url) 
    
    # Obtenemos el ticker de cada activo.
    ticker<-obtener_ticker(datos_web)
    
    # Obtenemos la divisa de cada activo.
    divisa<-obtener_divisa(datos_web)
    
    # Obtenemos el ISIN de cada activo.
    isin<-obtener_isin(datos_web)
    
    # Reconstruimos la tabla de los activos y la guardamos.
    nuevo_activo<-data.frame(Nombre=activo, Ticker= ticker, Isin=isin, Divisa=divisa, Url= url)
    info_activos <- rbind(info_activos, nuevo_activo)
    
  }, warning = function(cond) {
    return(info_activos)
  }, error=function(cond) {
    return(info_activos)
  }, finally = {
  })
  
  return(info_activos)
}


# Obtiene la información de los activos a descargar.
obtener_info_activos<-function(indice){
  
  # Obtenemos los activos que componen cada índice.
  nombre_activos<-composicion_indice(indice)
  info_activos<-data.frame(Nombre="", Ticker="", Isin="", Divisa="", Url="")
  
  # Obtenemos las url de los activos.
  url_activos<-obtener_url_activos(indice, nombre_activos)
  
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = length(nombre_activos), width=300)
  
  # Reconstruimos la información de la tabla de activos.
  for(activo in nombre_activos){
    
    setWinProgressBar(barra_progreso, which(activo==nombre_activos), title=paste(round(which(activo==nombre_activos)/length(nombre_activos)*100,0), "% del maestro de valores generado"))
    
    url<-as.character(url_activos[which(activo==nombre_activos),1])
    info_activos<-construimos_tabla_activos(activo,info_activos, indice, url)
  }
  info_activos<-info_activos[!info_activos$Ticker=="",]
  
  close(barra_progreso)
  
  # Guardamos info_activos con el nombre del índice correspondiente.
  #nombre_archivo<-paste(tabla_indices$tickers_indices[which(indice==tabla_indices$urls_indices)],"_info_activos.csv",sep="")
  #write.csv(info_activos, file = nombre_archivo, row.names = FALSE)
  
  return(info_activos)
}


# Limpiamos los datos descargados
limpiar<-function(cotizaciones){
  
  cotizaciones<-cotizaciones[,1:(dim(cotizaciones)[2]-1)] # Quitamos la columna de rentabilidad
  
  # Localizamos los volúmenes en miles "K"
  indice_miles = grepl("K", cotizaciones$Vol.)
  cotizaciones$Vol.[indice_miles]<-gsub("K", "", cotizaciones$Vol.[indice_miles])
  
  # Localizamos los volúmenes en millones "M"
  indice_millones = grepl("M", cotizaciones$Vol.)
  cotizaciones$Vol.[indice_millones]<-gsub("M", "", cotizaciones$Vol.[indice_millones])
  
  cotizaciones[,2:dim(cotizaciones)[2]]<- apply(cotizaciones[,2:dim(cotizaciones)[2]], 2, gsub, patt="\\.", replace="") # Si el activo cotiza por encima de 1.000, hay que quitar los puntos.
  cotizaciones[,2:dim(cotizaciones)[2]]<-apply(apply(cotizaciones[,2:dim(cotizaciones)[2]], 2, gsub, patt=",", replace="."), 2, as.numeric)
  cotizaciones$Vol.[indice_miles]<-cotizaciones$Vol.[indice_miles]*1000
  cotizaciones$Vol.[indice_millones]<-cotizaciones$Vol.[indice_millones]*1000000
  
  cotizaciones$Fecha<-as.Date(cotizaciones$Fecha, "%d.%m.%Y")
  
  return(cotizaciones)
}


# Homogeneizamos los datos descargados
library(zoo)
homogeneizar<-function(cotizaciones, fecha_inicio, fecha_fin){
  
  fecha_inicio<-as.Date(fecha_inicio, "%d/%m/%Y")
  fecha_fin<-as.Date(fecha_fin, "%d/%m/%Y")
  
  fechas<-seq(fecha_inicio, fecha_fin, by="days")# Creamos un vector con todas las fechas entre el día inicial y el final.
  dias_laborables<-!weekdays(fechas) %in% c("sábado", "domingo") # No nos interesan los sábados y domingos. Los localizamos.
  fechas<-fechas[dias_laborables] # Filtramos la serie de datos usando el índice booleano anterior.
  fechas<-as.data.frame(fechas)
  names(fechas)<-"Fecha"
  
  # Volcamos los datos en un DF homogeneizado, utilizando la columna de fechas como índice.
  datos_homogeneizados<-merge(fechas,cotizaciones,by=c("Fecha"),all.x = T)
  
  # Ordenamos los datos por fecha (al hacer el merge se ha invertido el orden)
  orden<-order(datos_homogeneizados$Fecha,decreasing = T)
  datos_homogeneizados<-datos_homogeneizados[orden,]
  
  # Localizamos los datos con NULL y los transformamos a NA
  datos_homogeneizados<-as.matrix(datos_homogeneizados)
  datos_homogeneizados[datos_homogeneizados=="null"] <- NA
  datos_homogeneizados<-as.data.frame(datos_homogeneizados)
  
  # Completamos los datos con NA (días en los que no hay registrada una cotización, pero que otras empresas sí cotizaron)
  datos_homogeneizados<-na.locf(datos_homogeneizados,na.rm = T) # na.locf localiza los NA del DF y los sustituye por el valor de la fila anterior. Si la fila con NA es la 1ª deja el NA sin dar un error.
  
  # Si los NA están en las primeras filas no hemos solucionado el problema. En principio no debería de haber más que dos (sábado y domingo), pero una empresa podría no cotizar lunes, martes... por lo que no conocemos el nº de potenciales NA a resolver.
  # Para resolver el problema, invertimos el orden del DF, aplicamos na.locf de nuevo y devolvemos el DF a su posición original.
  orden<-order(datos_homogeneizados$Fecha,decreasing = F)
  datos_homogeneizados<-datos_homogeneizados[orden,]
  datos_homogeneizados<-na.locf(datos_homogeneizados,na.rm = T)
  orden<-order(datos_homogeneizados$Fecha,decreasing = T)
  datos_homogeneizados<-datos_homogeneizados[orden,]
  
  return(datos_homogeneizados)
}


# Buscamos y eliminamos los split y contrasplit
split_contrasplit<-function(cotizaciones){
  
  # Es probable que haya existido un split o contrasplit con una variación superior al 85% (en un mismo día) log(1.85)=0.615 
  
  #           Precio  Rentabilidad  Multiplicador       Precio desmanipulado
  # 25/8/16   100     -2.302         1                   100
  #	24/8/16   1000     2.302         100/1000            100
  # 23/8/16   100                    100/1000*1000/100   100
  
  # Doble check: Consideramos que el volumen efectivo debe ser similar antes y después, por lo que el número de títulos negociados debe mostrar un 
  # comportamiento opuesto al de la cotización
  
  cotizaciones[,2:6]<-mutate_all(cotizaciones[,2:6], function(x) as.numeric(as.character(x)))
  
  rentabilidades<-as.data.frame(matrix(0,nrow=dim(cotizaciones)[1],ncol=1,byrow=F)) 
  rentabilidades[1:(dim(rentabilidades)[1]-1),1] <- log(cotizaciones[1:(dim(cotizaciones)[1]-1),2]/cotizaciones[2:dim(cotizaciones)[1],2])
  
  volumen<-as.data.frame(matrix(0,nrow=dim(cotizaciones)[1],ncol=1,byrow=F)) 
  volumen[1:(dim(volumen)[1]-1),1] <- log(cotizaciones[1:(dim(cotizaciones)[1]-1),6]/cotizaciones[2:dim(cotizaciones)[1],6])
  
  # Comprobamos si alguna rentabilidad de la serie (en precio o en volumen) es mayor o menor a 0.615. De ser así calculamos los multiplicadores.
  indice_booleano<-(rentabilidades>0.615 & volumen<(-0.615)) | (rentabilidades<(-0.615) & volumen>0.615)
  rentabilidades_excesivas<-rentabilidades[indice_booleano]
  
  if (length(rentabilidades_excesivas)>0){ # Si la serie temporal no tiene ningún split o contra split no hay nada que hacer. 
    multiplicador<-as.data.frame(matrix(1,nrow=dim(cotizaciones)[1],ncol=1,byrow=F))
    
    for(dia in 2:dim(cotizaciones)[1]){ # Recorremos las rentabilidades diarias para construir los multiplicadores.
      
      # Los split o contra split se hacen por la noche. Para eliminarlos debemos comparar el precio de cierre con el de apertura. Ambos deben coincidir exáctamente.
      if ((rentabilidades[dia-1,1]>0.615) & (volumen[dia-1,1]<(-0.615))){ # Comprobamos la rentabilidad y el volumen. Han hecho un contra split. Ajustamos el multiplicador.
        
        # Calculamos la relación entre el precio de cierre de ayer y la apertura de hoy.
        multiplicador[dia,1]<-multiplicador[dia-1,1]*(cotizaciones$Último[dia-1]/cotizaciones$Apertura[dia])
        
      } else if((rentabilidades[dia-1,1]<(-0.615)) & (volumen[dia-1,1]>(0.615))){ # Comprobamos la rentabilidad y el volumen. Han hecho un split. Ajustamos el multiplicador.
        
        # Calculamos la relación entre el precio de cierre de ayer y la apertura de hoy.
        multiplicador[dia,1]<-multiplicador[dia-1,1]*(cotizaciones$Último[dia-1]/cotizaciones$Apertura[dia])
        
      } else { # No han hecho nada, el multiplicador no varía.
        multiplicador[dia,1]<-multiplicador[dia-1,1]
      }
    }
    
    # Ajustamos los datos utilizando los multiplicadores.
    cotizaciones$Último<-cotizaciones$Último*multiplicador$V1
    cotizaciones$Apertura<-cotizaciones$Apertura*multiplicador$V1
    cotizaciones$Máximo<-cotizaciones$Máximo*multiplicador$V1
    cotizaciones$Mínimo<-cotizaciones$Mínimo*multiplicador$V1
    cotizaciones$Vol.<-cotizaciones$Vol./multiplicador$V1
  }
  
  return(cotizaciones)
}


# Generamos los DF globales de todos los activos (apertura, cierre, máximo, mínimo y volumen)
generar_df_activos<-function(info_activos, fecha_inicio, fecha_fin){
  
  print("Iniciando el proceso de descarga de datos")
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = dim(info_activos)[1], width=300) 
  
  for (activo in info_activos$Url){
    
    setWinProgressBar(barra_progreso, which(activo==info_activos$Url), title=paste(round(which(activo==info_activos$Url)/dim(info_activos)[1]*100,0), "% de la obtención de datos realizada"))
    
    # print(activo)
    Sys.sleep(sample(1:5, 1))
    activo<-as.character(activo)
    
    # Descargamos las cotizaciones de los activos.
    cotizaciones<-descargar_cotizaciones_diarias_investing(url_activo=activo, fecha_inicio, fecha_fin)
    
    if (all(!is.na(cotizaciones))){ 
      
      # Limpiamos los datos descargados.
      cotizaciones<-limpiar(cotizaciones)
      
      # Homogeneizamos los datos descargados.
      cotizaciones<-homogeneizar(cotizaciones, fecha_inicio, fecha_fin)
      
      # Eliminamos los split y contrasplit.
      cotizaciones<-split_contrasplit(cotizaciones)
      
      # Añadimos la información del activo a los DF finales. 
      if (activo==info_activos$Url[1]){
        # Generamos los DF.
        apertura<-as.data.frame(as.character(cotizaciones$Apertura))
          colnames(apertura)[1]<-as.character(info_activos$Nombre[1])
          rownames(apertura)<-cotizaciones$Fecha
        
        cierre<-as.data.frame(as.character(cotizaciones$Último))
          colnames(cierre)[1]<-as.character(info_activos$Nombre[1])
          rownames(cierre)<-cotizaciones$Fecha
        
        maximo<-as.data.frame(as.character(cotizaciones$Máximo))
          colnames(maximo)[1]<-as.character(info_activos$Nombre[1])
          rownames(maximo)<-cotizaciones$Fecha
        
        minimo<-as.data.frame(as.character(cotizaciones$Mínimo))
          colnames(minimo)[1]<-as.character(info_activos$Nombre[1])
          rownames(minimo)<-cotizaciones$Fecha
        
        volumen<-as.data.frame(as.character(cotizaciones$Vol.))
          colnames(volumen)[1]<-as.character(info_activos$Nombre[1])
          rownames(volumen)<-cotizaciones$Fecha
        
      }else{
        # Incluimos la información en los DF existentes.
        apertura<-cbind(apertura,as.data.frame(as.character(cotizaciones$Apertura)))
          colnames(apertura)[dim(apertura)[2]]<-as.character(info_activos$Nombre[which(activo==info_activos$Url)])
        
        cierre<-cbind(cierre,as.data.frame(as.character(cotizaciones$Último)))
          colnames(cierre)[dim(cierre)[2]]<-as.character(info_activos$Nombre[which(activo==info_activos$Url)])
        
        maximo<-cbind(maximo,as.data.frame(as.character(cotizaciones$Máximo)))
          colnames(maximo)[dim(maximo)[2]]<-as.character(info_activos$Nombre[which(activo==info_activos$Url)])
        
        minimo<-cbind(minimo,as.data.frame(as.character(cotizaciones$Mínimo)))
          colnames(minimo)[dim(minimo)[2]]<-as.character(info_activos$Nombre[which(activo==info_activos$Url)])
        
        volumen<-cbind(volumen,as.data.frame(as.character(cotizaciones$Vol.)))
          colnames(volumen)[dim(volumen)[2]]<-as.character(info_activos$Nombre[which(activo==info_activos$Url)])
      }
    }
  }
  
  close(barra_progreso)
  
  # Quitamos los activos que tengan NA en la descarga.
  #apertura<-apertura[,colSums(is.na(apertura))==0]
  #cierre<-cierre[,colSums(is.na(cierre))==0]
  #maximo<-maximo[,colSums(is.na(maximo))==0]
  #minimo<-minimo[,colSums(is.na(minimo))==0]
  #volumen<-volumen[,colSums(is.na(volumen))==0]
  
  datos_activos<- list(apertura, cierre, maximo, minimo, volumen)
  return(datos_activos)
}

# Nos bajamos los datos de la renta fija (eonia) del Banco de España.
library(dplyr)
obtener_info_renta_fija<-function(fecha_inicio, fecha_fin){
  
  fecha_inicio<-as.Date(fecha_inicio, "%d/%m/%Y")
  fecha_fin<-as.Date(fecha_fin, "%d/%m/%Y")
  
  # Nos bajamos los datos de Renta fija (eonia) del Banco de España.
  url<- "http://www.bde.es/webbde/es/estadis/infoest/series/ti_1_7.csv"
  
  renta_fija <- read.csv(file=url, stringsAsFactors = FALSE, header = F)
  
  renta_fija<-renta_fija[5:dim(renta_fija)[1],] # Eliminamos las filas de títulos que no nos sirven.
  renta_fija<-renta_fija[,c(1,3)] # Nos quedamos solo con las columnas que nos interesan (fecha y eonia mundial)
  colnames(renta_fija)<-c("Fecha", "Eonia") # Ponemos nombres a las columnas.
  
  # Que las fechas estén con formato Español y los meses en texto va a suponer mucho trabajo manual.
  meses_texto <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  meses_dig <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  
  for (mes in 1:12){
    renta_fija[,1]<-gsub(meses_texto[mes],meses_dig[mes], renta_fija[,1])
  }
  
  renta_fija[,1]<- as.Date(renta_fija[,1], format = "%d %m %Y") # Convertimos el formato español a fecha internacional.
  
  # Filtramos por el rango de fechas que nos interesan, eliminando las que no.
  renta_fija<-renta_fija[renta_fija$Fecha>=fecha_inicio,]
  renta_fija<-renta_fija[renta_fija$Fecha<=fecha_fin,]
  
  # Identificamos y eliminamos los NA del DF. En este caso el Banco de España pone un guión bajo _
  indice_booleano <- renta_fija[2] == "_" 
  renta_fija<-renta_fija[!indice_booleano,]
  
  # Ordeno los datos de la renta fija igual que los de renta variable
  orden<-order(renta_fija$Fecha,decreasing = T)
  renta_fija<-renta_fija[orden,]
  
  # Vemos que el DF tiene sábados y domingos, por lo que hace falta homogeneizar (es curioso porque publican las divisas sin sábados ni domingos)
  renta_fija<-homogeneizar(renta_fija, fecha_inicio, fecha_fin)
  
  # Ponemos los nombres a las filas para igualar el formato de todos los DF.
  rownames(renta_fija)<-renta_fija$Fecha
  
  # Transformamos la rentabilidad a logarítmica y diaria
  rent_log_diaria<-renta_fija[2] %>% mutate_all(as.numeric)
  rent_log_diaria<-log((1+rent_log_diaria)^(1/365))
  renta_fija[2]<-rent_log_diaria
  
  # Quitamos la columna de fecha para hacer el data frame homogéneo con el resto de datos descargados.
  datos_renta_fija<-list(renta_fija[2])
  
  return(datos_renta_fija)
}


# Obtenemos la información del índice
obtener_info_indice<-function(indice, fecha_inicio, fecha_fin){
  
  # Descargamos el histórico del índice.
  url_indice<-gsub("-components", "-historical-data", indice) 
  cotizaciones_indice<-descargar_cotizaciones_diarias_investing(url_activo = url_indice, fecha_inicio, fecha_fin)
  
  # Limpiamos los datos descargados.
  cotizaciones_indice<-limpiar(cotizaciones=cotizaciones_indice)
  
  # Homogeneizamos los datos descargados.
  cotizaciones_indice<-homogeneizar(cotizaciones=cotizaciones_indice, fecha_inicio, fecha_fin)
  rownames(cotizaciones_indice)<-cotizaciones_indice$Fecha
  cotizaciones_indice<-list(cotizaciones_indice[,2:dim(cotizaciones_indice)[2]])
  
  return(cotizaciones_indice)
}


# Obtenemos la información de la divisa
obtener_info_divisa<-function(indice, divisa, fecha_inicio, fecha_fin){
  
  if(divisa=="eur"){
    
    inicio<-c(fecha_inicio,1,1,1,1,0)
    fin<-c(fecha_fin,1,1,1,1,0)
    cotizaciones_divisa<-as.data.frame(rbind(fin,inicio))
    colnames(cotizaciones_divisa)<-c("Fecha", "Último", "Apertura", "Máximo", "Mínimo", "X..var.")
    cotizaciones_divisa$Fecha<-as.Date(cotizaciones_divisa$Fecha, format = "%d/%m/%Y")
    
  }else{
    url_divisa<-paste("https://es.investing.com/currencies/eur-",divisa,"-historical-data", sep="")
    cotizaciones_divisa<-descargar_cotizaciones_diarias_investing(url_activo = url_divisa, fecha_inicio, fecha_fin)
  }
  
  # Limpiamos los datos descargados.
  cotizaciones_divisa<-limpiar(cotizaciones=cotizaciones_divisa)
  
  # Homogeneizamos los datos descargados.
  cotizaciones_divisa<-homogeneizar(cotizaciones=cotizaciones_divisa, fecha_inicio, fecha_fin)
  rownames(cotizaciones_divisa)<-cotizaciones_divisa$Fecha
  cotizaciones_divisa<-list(cotizaciones_divisa[,2:dim(cotizaciones_divisa)[2]])
  
  return(cotizaciones_divisa)
}


# Hayamos la fecha desde la que nos tenemos que descargar los datos para poder trabajar con la ventana que realmente queremos.
  encontrar_fecha_anterior_inicio<-function(fecha_inicio,ventana){
    fecha_inicio <- as.Date(fecha_inicio, "%d/%m/%Y")    
    anterior_inicio <- fecha_inicio-ventana*3
    
    dias_posibles<-as.data.frame(matrix(0,nrow=length(seq(anterior_inicio, fecha_inicio, "days")),ncol=2,byrow=F))
    colnames(dias_posibles)<-c("Dias posibles", "Dia de la semana")
    dias_posibles$`Dias posibles`<-seq(anterior_inicio, fecha_inicio, "days")
    dias_posibles$`Dia de la semana`<-weekdays(dias_posibles$`Dias posibles`)
    
    indice<-dias_posibles$`Dia de la semana` %in% c("sábado", "domingo")
    dias_posibles<-dias_posibles[!indice,]
    
    dias_posibles<-dias_posibles[dim(dias_posibles)[1]:1,]
    
    fecha_anterior_inicio<-format(dias_posibles$`Dias posibles`[ventana*2+1], "%d/%m/%Y")

    return(fecha_anterior_inicio)
  }


# Descarga, limpieza, homogeneización y desmanipulación de los datos
descarga_limpieza_homogeneizacion_desmanipulacion<-function(indice,fecha_inicio, fecha_fin, ventana){  
  
  print("Generando el maestro de valores")
  
  # Modificamos la fecha desde la que nos descargamos los datos para poder realizar las operaciones financieras.
  fecha_inicio<-encontrar_fecha_anterior_inicio(fecha_inicio,ventana)
  
  # Cargamos la información de los activos de cada índice.
  info_activos<-obtener_info_activos(indice)
  
  # Generamos los DF globales de todos los activos (apertura, cierre, máximo, mínimo y volumen)
  datos_descargados<- generar_df_activos(info_activos, fecha_inicio, fecha_fin)
  
  # Obtenemos la información del índice.
  cotizaciones_indice<-obtener_info_indice(indice, fecha_inicio, fecha_fin)
  datos_descargados<- c(datos_descargados, cotizaciones_indice)
  
  # Obtenemos la información de la divisa
  divisa<-tolower(as.character(info_activos$Divisa[1]))
  cotizaciones_divisa<-obtener_info_divisa(indice, divisa, fecha_inicio, fecha_fin)
  datos_descargados<- c(datos_descargados, cotizaciones_divisa)
  
  # Obtenemos la renta fija
  datos_renta_fija<-obtener_info_renta_fija(fecha_inicio, fecha_fin)
  datos_descargados<- c(datos_descargados, datos_renta_fija)
  
  # Metemos el maestro de valores (info activos) como dato final.
  info_activos<-list(info_activos)
  datos_descargados<- c(datos_descargados, info_activos)
  
  return(datos_descargados)
}


# Calculamos el Alpha de Jensen actual para cada activo.
library(taRifx)
library(RollingWindow) # install_github("andrewuhl/RollingWindow") # https://github.com/andrewuhl/RollingWindow

calculamos_alpha<-function(datos_descargados, ventana){
  
  # Los datos descargados están en formato de lista. Recuperamos los que necesitamos como un DF, elimino los factores y los convierto a numéricos.
  cierre<-remove.factors(datos_descargados[[2]])
  fechas<-rownames(cierre)
  cierre<-cierre %>% mutate_all(as.numeric)
    rownames(cierre)<-fechas
  indice<-remove.factors(datos_descargados[[6]])
  indice<-indice %>% mutate_all(as.numeric)
    rownames(indice)<-fechas
  renta_fija<-remove.factors(datos_descargados[[8]])
  renta_fija<-renta_fija %>% mutate_all(as.numeric)
    rownames(renta_fija)<-fechas
  
  # Calculamos la rentabilidad de los activos.
  rent_activos<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F)) 
  colnames(rent_activos)<-colnames(cierre)
  rownames(rent_activos)<-fechas
  rent_activos[1:(dim(cierre)[1]-1),1:dim(cierre)[2]] <- log(cierre[1:(dim(cierre)[1]-1),1:dim(cierre)[2]]/cierre[2:dim(cierre)[1],1:dim(cierre)[2]])
  
  # Rentabilidad del benchmark
  indice<-indice[1]
  rent_indice <- as.data.frame(rep(0,dim(indice)[1]))
  rownames(rent_indice)<-fechas
  rent_indice[1:(dim(indice)[1]-1),1] <- log(indice[1:(dim(indice)[1]-1),1]/indice[2:dim(indice)[1],1])
  
  # Rentabilidad de la renta fija.
  rent_renta_fija<-renta_fija/100 # El dato descargado del eonia ya es una rentabilidad, pero está en tanto por cien. Lo ajustamos.
  rownames(rent_renta_fija)<-fechas
  
  # Calculamos la varianza del benchmark 
  rent_indice<-rent_indice[nrow(rent_indice):1,] # Invertimos los datos para poder usar RollingVar sin perder los datos más recientes. 
  
  varianza_indice <-rep(0,dim(indice)[1]) 
  varianza_indice<-RollingVar(rent_indice,window = ventana) # Genera NA
  varianza_indice<-rev(varianza_indice) # Doy la vuelta al vector. Tengo NA al final del vector
  varianza_indice<-as.data.frame(na.locf(varianza_indice,na.rm = T)) # na.locf localiza los NA del DF y los sustituye por el valor de la fila anterior.
  colnames(varianza_indice)<-"Varianza indice"
  rownames(varianza_indice)<-fechas
  
  # Calculamos la covarianza entre el activo y el benchmark
  rent_activos<-rent_activos[nrow(rent_activos):1,] # Invertimos el orden de la matriz para poder usar RollingCov
  
  covarianza_activos_benchmark<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
  colnames(covarianza_activos_benchmark)<-colnames(cierre)
  
  for (activo in 1:dim(cierre)[2]){ # RollingCov solo acepta vectores, por lo que tenemos que recorrer los activos con un for,
    covarianza_activos_benchmark[,activo] <- RollingCov(rent_indice, rent_activos[,activo], window = ventana) 
  }
  
  rent_indice<-as.data.frame(rev(rent_indice)) # Pongo del derecho la información.
  colnames(rent_indice)<-"rent indice"
  rownames(rent_indice)<-fechas
  rent_activos<-rent_activos[nrow(rent_activos):1,]# Devolvemos la matriz a su posición.
  
  covarianza_activos_benchmark<-covarianza_activos_benchmark[nrow(covarianza_activos_benchmark):1,] # Ponemos del derecho la matriz
  covarianza_activos_benchmark<-na.locf(covarianza_activos_benchmark,na.rm = T) # Elimino los NA
  rownames(covarianza_activos_benchmark)<-fechas
  
  # Sacamos la Beta de cada activo. β=cov(Rc,Rm)/σRm
  beta<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F)) 
  colnames(beta)<-colnames(cierre)
  rownames(beta)<-fechas
  beta[1:dim(beta)[1],1:dim(beta)[2]] <- covarianza_activos_benchmark[1:dim(covarianza_activos_benchmark)[1],1:dim(covarianza_activos_benchmark)[2]]/varianza_indice[1:dim(rent_indice)[1],1]
  
  # Calculamos el Alpha de cada activo. α=Rc-(Rf+β(Rm-Rf))
  alpha_activos<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F)) 
  colnames(alpha_activos)<-colnames(cierre)
  rownames(alpha_activos)<-fechas
  
  alpha_activos[1:dim(alpha_activos)[1],1:dim(alpha_activos)[2]]<- rent_activos[1:dim(rent_activos)[1],1:dim(rent_activos)[2]]-(rent_renta_fija[1:dim(rent_renta_fija)[1],1]+beta[1:dim(beta)[1],1:dim(beta)[2]]*(rent_indice[1:dim(rent_indice)[1],1]-rent_renta_fija[1:dim(rent_renta_fija)[1],1]))
  
  return(alpha_activos)
}


# Calculamos el precio que iguala el Alpha de Jensen objetivo α=Rc-(Rf+β(Rm-Rf)) Es decir, obtenemos los precios de compra y venta objetivos
  precio_objetivo<-function(alpha_objetivo, cotizaciones_activo, cotizaciones_indice, cotizaciones_renta_fija){
    
    # Calculamos la rentabilidad de los activos, su benchmark y el eonia.
    rent_activos <- log(cotizaciones_activo[1:(length(cotizaciones_activo)-1)]/cotizaciones_activo[2:length(cotizaciones_activo)])
    rent_activos<-c(rent_activos,rent_activos[length(rent_activos)]) # Igualamos el tamaño repitiendo el último dato.
    
    rent_benchmark <- log(cotizaciones_indice[1:(length(cotizaciones_indice)-1)]/cotizaciones_indice[2:length(cotizaciones_indice)])
    rent_benchmark<-c(rent_benchmark,rent_benchmark[length(rent_benchmark)])
    
    # Calculamos la varianza del benchmark
    varianza_benchmark<-var(rent_benchmark)
    
    # Calculamos la covarianza entre el activo y el benchmark
    covarianza_activos_benchmark <- cov(rent_benchmark, rent_activos) 
    
    # Sacamos la Beta de cada activo. β=cov(Rc,Rm)/σRm
    beta <- covarianza_activos_benchmark / varianza_benchmark
    
    # Calculo la rentabilidad esperada RE= RF+Beta(RM-RF)
    rent_esperada<-cotizaciones_renta_fija[1]+beta*(rent_benchmark[1]-cotizaciones_renta_fija[1])
    
    # Calculo la rentabilidad del precio de compra o venta. RPC = RE + Alpha entrada o RPV = RE + Alpha salida.
    rent_precio<-alpha_objetivo+rent_esperada
    
    # Calculo el precio objetivo de compra o venta. Precio objetivo = Precio hoy * exp(Rent PC)
    precio_objetivo<-cotizaciones_activo[1]*exp(rent_precio)
    
    return(precio_objetivo)
  }  


# Calculo la variación del percentil dinámico de entrada, en función de la recomendación de ayer y los precios de hoy. 
  percentil_entrada_dinamico<-function(precio_objetivo_compra, precio_objetivo_venta, maximo, minimo, entrada){
    
    # Si incremento el percentil, incremento el precio (subo la banda)
    # Si reduzco el percentil, bajo el precio (bajo la banda)
    
    # Si la el precio de compra, recomendado para ayer, comparándolo con los precios de hoy:
    #	No toca la vela y el mínimo está por encima, incrementamos la configuración de compra de ayer.
    #	No toca la vela y el máximo está por debajo, reducimos la configuración de compra de ayer.
    #	Si toca la vela pero la banda de venta no la toca, reducimos la configuración de compra.
    #	Si toca la vela y la banda de venta la toca, volvemos un paso a la configuración inicial de compra (15%).
    
    vector_percentiles_entrada<-c(0.0381, 0.0424, 0.0471, 0.0523, 0.0581, 0.0646, 0.0717, 0.0797, 0.0886, 0.0984, 0.1094,
                                  0.1215, 0.1350, 0.1500, 0.1650, 0.1815, 0.1997, 0.2196, 0.2416, 0.2657, 0.2923, 0.3215,
                                  0.3537, 0.3891, 0.4280, 0.4708) # Usamos un vector de configuraciones xq los incrementos % son diferentes al ir en una direcicón y volver.
    
    # Comprobamos la posición del percentil dentro del vector.
    posicion<-which(entrada == vector_percentiles_entrada)
    
    # Comprobamos si el precio recomendado ha tocado la vela.
    if(precio_objetivo_compra <= maximo & precio_objetivo_compra >= minimo){
      
      # Comprobamos si la banda de venta toca la vela.
      if(precio_objetivo_venta <= maximo & precio_objetivo_venta >= minimo){
        
        #	Si toca la vela y la banda de venta la toca, volvemos un paso a la configuración inicial de compra (15%).
        if (posicion > 14){ 
          
          entrada<-vector_percentiles_entrada[posicion-1]
          
        } else if (posicion < 14){ 
          
          entrada<-vector_percentiles_entrada[posicion+1]
        }
        
      } else {
        
        # Si toca la vela y la banda de venta no la toca, reducimos la configuración de compra.
        if (posicion > 1){
          entrada<-vector_percentiles_entrada[posicion-1]
        }
      }
      
    } else if(precio_objetivo_compra < minimo & posicion < length(vector_percentiles_entrada)){
      
      # No toca la vela y el mínimo está por encima, incrementamos la configuración de compra de ayer.
      entrada<-vector_percentiles_entrada[posicion+1]
      
    } else if(precio_objetivo_compra > maximo & posicion > 1){
      
      #	No toca la vela y el máximo está por debajo, reducimos la configuración de compra de ayer.
      entrada<-vector_percentiles_entrada[posicion-1]
    }
    
    return(entrada)
  }
  
  
# Calculo la variación del percentil dinámico de entrada, en función de la recomendación de ayer y los precios de hoy. 
  percentil_salida_dinamico<-function(precio_objetivo_compra, precio_objetivo_venta, maximo, minimo, salida){
    
    # Si incremento el percentil, incremento el precio (subo la banda)
    # Si reduzco el percentil, bajo el precio (bajo la banda)
    
    #	Si la banda de venta, en la recomendación anterior:
    #	No toca la vela y el máximo está por debajo, reducimos la configuración de venta.
    #	No toca la vela y el mínimo está por encima, incrementamos la configuración de venta.
    #	Si toca la vela pero la banda de compra no la toca, incrementamos la configuración de venta.
    #	Si toca la vela y la banda de compra la toca, volvemos un paso a la configuración de venta de (85%).
    
    vector_percentiles_salida<-c(0.5909, 0.6029, 0.6152, 0.6278, 0.6406, 0.6537, 0.6670, 0.6806, 0.6945, 0.7087, 0.7231,
                                 0.7379, 0.7530, 0.7683, 0.7840, 0.8000, 0.8163, 0.8330, 0.8500, 0.8670, 0.8843, 0.9020,
                                 0.9201, 0.9385, 0.9572, 0.9764) # Usamos un vector de configuraciones xq los incrementos % son diferentes al ir en una direcicón y volver.
    
    # Comprobamos la posición del percentil dentro del vector.
    posicion<-which(salida == vector_percentiles_salida)
    
    # Comprobamos si el precio recomendado ha tocado la vela.
    if(precio_objetivo_venta <= maximo & precio_objetivo_venta >= minimo){
      
      # Comprobamos si la banda de compra toca la vela.
      if(precio_objetivo_compra <= maximo & precio_objetivo_compra >= minimo){
        
        #	Si toca la vela y la banda de compra la toca, volvemos un paso a la configuración de venta (85%).
        if (posicion > 19){ 
          
          salida<-vector_percentiles_salida[posicion-1]
          
        } else if (posicion < 19){ 
          
          salida<-vector_percentiles_salida[posicion+1]
        }
        
      } else {
        
        #	Si toca la vela pero la banda de compra no la toca, incrementamos la configuración de venta.
        if (posicion < length(vector_percentiles_salida))
          salida<-vector_percentiles_salida[posicion+1]
      }
      
    } else if(precio_objetivo_venta < minimo & posicion < length(vector_percentiles_salida)){
      
      #	No toca la vela y el mínimo está por encima, incrementamos la configuración de venta.
      salida<-vector_percentiles_salida[posicion+1]
      
    } else if(precio_objetivo_venta > maximo & posicion > 1){
      
      #	No toca la vela y el máximo está por debajo, reducimos la configuración de venta.
      salida<-vector_percentiles_salida[posicion-1]
    }
    
    return(salida)
  }
  

# Criterio de selección de activos (qué y cuando)
  seleccion_activos <- function(datos_descargados, ventana, entrada, salida, percentil_dinamico, fecha_inicio){
    print("Iniciando el proceso de selección de activos")
    
    # Calculamos el Alpha de Jensen
    alpha_actual<-calculamos_alpha(datos_descargados, ventana)
    
    # Calculamos los alphas de entrada y salida, así como los precios objetivo de entrada y salida para cada activo, cada día.
    alpha_entrada<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(alpha_entrada)<-colnames(alpha_actual)
      rownames(alpha_entrada)<-rownames(alpha_actual)
    alpha_salida<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(alpha_salida)<-colnames(alpha_actual)
      rownames(alpha_salida)<-rownames(alpha_actual)
    precio_objetivo_compra<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(precio_objetivo_compra)<-colnames(alpha_actual)
      rownames(precio_objetivo_compra)<-rownames(alpha_actual)
    precio_objetivo_venta<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(precio_objetivo_venta)<-colnames(alpha_actual)
      rownames(precio_objetivo_venta)<-rownames(alpha_actual)
    seleccion_activos<-as.data.frame(matrix(0,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(seleccion_activos)<-colnames(alpha_actual)
      rownames(seleccion_activos)<-rownames(alpha_actual)
    percentil_entrada<-as.data.frame(matrix(entrada,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(percentil_entrada)<-colnames(alpha_actual)
      rownames(percentil_entrada)<-rownames(alpha_actual)
    percentil_salida<-as.data.frame(matrix(salida,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(percentil_salida)<-colnames(alpha_actual)
      rownames(percentil_salida)<-rownames(alpha_actual)
    ventana_dinamica<-as.data.frame(matrix(ventana,nrow=dim(alpha_actual)[1],ncol=dim(alpha_actual)[2],byrow=F))
      colnames(ventana_dinamica)<-colnames(alpha_actual)
      rownames(ventana_dinamica)<-rownames(alpha_actual)
    
    apertura<-remove.factors(datos_descargados[[1]])
      fechas<-rownames(apertura)
      apertura<-apertura %>% mutate_all(as.numeric)
      rownames(apertura)<-fechas
    cierre<-remove.factors(datos_descargados[[2]])
      cierre<-cierre %>% mutate_all(as.numeric)
      rownames(cierre)<-fechas
    maximo<-remove.factors(datos_descargados[[3]])
      maximo<-maximo %>% mutate_all(as.numeric)
      rownames(maximo)<-fechas
    minimo<-remove.factors(datos_descargados[[4]])
      minimo<-minimo %>% mutate_all(as.numeric)
      rownames(minimo)<-fechas
    volumen<-remove.factors(datos_descargados[[5]])
      volumen<-volumen %>% mutate_all(as.numeric)
      rownames(volumen)<-fechas
    indice<-remove.factors(datos_descargados[[6]])
      indice<-indice %>% mutate_all(as.numeric)
      rownames(indice)<-fechas
    divisa<-remove.factors(datos_descargados[[7]])
      divisa<-divisa %>% mutate_all(as.numeric)
      rownames(divisa)<-fechas
    renta_fija<-remove.factors(datos_descargados[[8]])
      renta_fija<-renta_fija %>% mutate_all(as.numeric)
      rownames(renta_fija)<-fechas
    
    for (dia in (dim(alpha_actual)[1]-ventana):1){
      for (activo in 1:dim(alpha_actual)[2]){
        
        if (percentil_dinamico==T){
          if(dia != dim(alpha_actual)[1]-ventana){

            # Consultamos si hemos tocado la vela el día anterior y variamos los percentiles de entrada y salida en función de ello. 
            percentil_entrada[dia, activo]<-percentil_entrada_dinamico(precio_objetivo_compra[dia+1,activo], precio_objetivo_venta[dia+1,activo], maximo[dia,activo], minimo[dia,activo], percentil_entrada[dia+1,activo])
            percentil_salida[dia, activo]<-percentil_salida_dinamico(precio_objetivo_compra[dia+1,activo], precio_objetivo_venta[dia+1,activo], maximo[dia,activo], minimo[dia,activo], percentil_salida[dia+1,activo])
            
            # Cambiamos la ventana en función únicamente del POC (lo que queremos evitar es que el POC esté siempre por debajo de las siguientes velas en una fuerte subida).
            if(percentil_entrada[dia, activo]>0.15){
              if(percentil_entrada[dia, activo]>percentil_entrada[dia+1, activo]){
                
                # Nos estamos alejando del percentil estandar, acortamos la ventana
                ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]/1.35,0)
                
              }else if (percentil_entrada[dia, activo]<percentil_entrada[dia+1, activo]){
                
                # Regresamos al percentil estandar, ampliamos la ventana
                ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]*1.35,0)
                
                if (ventana_dinamica[dia, activo]>ventana){
                  ventana_dinamica[dia, activo]<-ventana
                }
                
              }else{
                
                # Mantenemos la ventana anterior
                ventana_dinamica[dia, activo]<-ventana_dinamica[dia+1, activo]
              }
            }else if(percentil_entrada[dia, activo]<0.15){
              
              if(percentil_entrada[dia, activo]<percentil_entrada[dia+1, activo]){
               
                # Nos estamos alejando del percentil estandar, acortamos la ventana
                ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]/1.35,0)
                 
              }else if(percentil_entrada[dia, activo]>percentil_entrada[dia+1, activo]){
                
                # Regresamos al percentil estandar, ampliamos la ventana
                ventana_dinamica[dia, activo]<-round(ventana_dinamica[dia+1, activo]*1.35,0)
                
                if (ventana_dinamica[dia, activo]>ventana){
                  ventana_dinamica[dia, activo]<-ventana
                }
                
              }else{
                
                # Mantenemos la ventana anterior
                ventana_dinamica[dia, activo]<-ventana_dinamica[dia+1, activo]
              }
              
            }else{
              
              # Estamos en el percentil estandar, regresamos a la normalidad
              ventana_dinamica[dia, activo]<-ventana
            }
          }
        }
        
        # Comprobamos que el tamaño de la ventana dinámica no baja nunca de un umbral mínimo.
        if(ventana_dinamica[dia, activo]<5){
          ventana_dinamica[dia, activo]<-5
        }
        
        # Calculamos el Alpha de entrada (punto a partir del que compraríamos)
        alpha_entrada[dia,activo]<-quantile(alpha_actual[dia:(dia+ventana_dinamica[dia, activo]),activo], probs = c(percentil_entrada[dia, activo]))
        
        # Calculamos el Alpha de salida (punto a partir del que venderíamos)
        alpha_salida[dia,activo]<-quantile(alpha_actual[dia:(dia+ventana_dinamica[dia, activo]),activo], probs = c(percentil_salida[dia, activo]))
        
        # Calculamos el precio objetivo de compra para el periodo siguiente
        precio_objetivo_compra[dia, activo]<-precio_objetivo(alpha_objetivo=alpha_entrada[dia,activo], cotizaciones_activo=cierre[dia:(dia+ventana_dinamica[dia, activo]), activo], cotizaciones_indice=indice[dia:(dia+ventana_dinamica[dia, activo]),1], cotizaciones_renta_fija=renta_fija[dia:(dia+ventana_dinamica[dia, activo]),1])
        
        # Calculamos el precio objetivo de venta para el periodo siguiente
        precio_objetivo_venta[dia, activo]<-precio_objetivo(alpha_objetivo=alpha_salida[dia,activo], cotizaciones_activo=cierre[dia:(dia+ventana_dinamica[dia, activo]), activo], cotizaciones_indice=indice[dia:(dia+ventana_dinamica[dia, activo]),1], cotizaciones_renta_fija=renta_fija[dia:(dia+ventana_dinamica[dia, activo]),1])
      }
    }
      
    # Acortamos el tamaño de los DF para ajustarlos a la fecha_inicio y fecha_fin   
    fechas<-rownames(cierre)
    indice_booleano<-as.Date(fechas)>=as.Date(fecha_inicio, "%d/%m/%Y") 
    percentil_entrada<-percentil_entrada[indice_booleano,] 
    percentil_salida<-percentil_salida[indice_booleano,] 
    alpha_entrada<-alpha_entrada[indice_booleano,] 
    alpha_salida<-alpha_salida[indice_booleano,] 
    precio_objetivo_compra<-precio_objetivo_compra[indice_booleano,] 
    precio_objetivo_venta<-precio_objetivo_venta[indice_booleano,] 
    ventana_dinamica<-ventana_dinamica[indice_booleano,] 
      
    activos_seleccionados<- list(percentil_entrada, percentil_salida, alpha_entrada, alpha_salida, precio_objetivo_compra, precio_objetivo_venta, ventana_dinamica)

    # Acortamos el tamaño de los DF de la lista de datos_descargados para hacer cálculos coherentes en el resto del código.
    apertura<-apertura[indice_booleano,]
      datos_descargados[[1]]<<-apertura
    cierre<-cierre[indice_booleano,]
      datos_descargados[[2]]<<-cierre
    maximo<-maximo[indice_booleano,]
      datos_descargados[[3]]<<-maximo
    minimo<-minimo[indice_booleano,]
      datos_descargados[[4]]<<-minimo
    volumen<-volumen[indice_booleano,]
      datos_descargados[[5]]<<-volumen
    indice<-indice[indice_booleano,]
      datos_descargados[[6]]<<-indice
    divisa<-divisa[indice_booleano,]
      datos_descargados[[7]]<<-divisa
    renta_fija<-renta_fija[indice_booleano,]
      datos_descargados[[8]]<<-renta_fija
    
    return(activos_seleccionados)
  }
  

# Calculamos el órden en el que los activos recibirían capital disponible día a día.
  ranking_de_asignacion_recursos<-function(datos_descargados, activos_seleccionados, ventana, comision_minima){
    
    cierre<-remove.factors(datos_descargados[[2]])
      fechas<-rownames(cierre)
      cierre<-cierre %>% mutate_all(as.numeric)
      rownames(cierre)<-fechas
    maximo<-remove.factors(datos_descargados[[3]])
      maximo<-maximo %>% mutate_all(as.numeric)
      rownames(maximo)<-fechas
    minimo<-remove.factors(datos_descargados[[4]])
      minimo<-minimo %>% mutate_all(as.numeric)
      rownames(minimo)<-fechas
    volumen<-remove.factors(datos_descargados[[5]])
      volumen<-volumen %>% mutate_all(as.numeric)
      rownames(volumen)<-fechas
    divisa<-remove.factors(datos_descargados[[7]])
      divisa<-divisa %>% mutate_all(as.numeric)
      rownames(divisa)<-fechas
    precio_objetivo_compra<-remove.factors(activos_seleccionados[[5]])
      precio_objetivo_compra<-precio_objetivo_compra %>% mutate_all(as.numeric)
      rownames(precio_objetivo_compra)<-fechas
    precio_objetivo_venta<-remove.factors(activos_seleccionados[[6]])
      precio_objetivo_venta<-precio_objetivo_venta %>% mutate_all(as.numeric)
      rownames(precio_objetivo_venta)<-fechas
    
    volumen_minimo<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(volumen_minimo)<-colnames(cierre)
      rownames(volumen_minimo)<-rownames(cierre)
    volumen_maximo<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(volumen_maximo)<-colnames(cierre)
      rownames(volumen_maximo)<-rownames(cierre)
    frecuencia_condicion_compra<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(frecuencia_condicion_compra)<-colnames(cierre)
      rownames(frecuencia_condicion_compra)<-rownames(cierre)
    ultima_condicion_compra<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(ultima_condicion_compra)<-colnames(cierre)
      rownames(ultima_condicion_compra)<-rownames(cierre)
    frecuencia_condicion_venta<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(frecuencia_condicion_venta)<-colnames(cierre)
      rownames(frecuencia_condicion_venta)<-rownames(cierre)
    ultima_condicion_venta<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(ultima_condicion_venta)<-colnames(cierre)
      rownames(ultima_condicion_venta)<-rownames(cierre)
    ranking<-as.data.frame(matrix(0,nrow=dim(cierre)[1],ncol=dim(cierre)[2],byrow=F))
      colnames(ranking)<-colnames(cierre)
      rownames(ranking)<-rownames(cierre)
    
    for (dia in (dim(cierre)[1]-ventana):1){
      for (activo in 1:dim(cierre)[2]){
        
        # Calculamos el volumen mínimo a comprar (aquel que cubre las comisiones). redondear(comision/(precio_venta - precio_compra))
        volumen_minimo[dia,activo]<-floor(comision_minima*2/(precio_objetivo_venta[dia,activo]/divisa[dia,1]-precio_objetivo_compra[dia,activo]/divisa[dia,1]))+1
        
        # Calculamos el volumen máximo a comprar (umbral de arrastre). 0,5% del volumen medio de las últimas sesiones.
        volumen_maximo[dia,activo]<-floor(mean(volumen[dia:(dia+ventana),activo])*0.005)
        
        # Calculamos el porcentaje de veces que se ha cumplido la condición de compra en la ventana temporal.
        frecuencia_condicion_compra[dia,activo]<-sum(minimo[dia:(dia+ventana),activo]<=precio_objetivo_compra[dia,activo])/(ventana+1)
        
        # Calculamos la última vez que se ha cumplido la condición de compra en la ventana temporal.
        if(frecuencia_condicion_compra[dia,activo]>0){ 
          ultima_condicion_compra[dia,activo]<-min(which((minimo[dia:(dia+ventana),activo]<=precio_objetivo_compra[dia,activo]) == TRUE))
        }else{
          ultima_condicion_compra[dia,activo]<-ventana*2
        }
        
        # Calculamos el porcentaje de veces que se ha cumplido la condición de venta en la ventana temporal.
        frecuencia_condicion_venta[dia,activo]<-sum(maximo[dia:(dia+ventana),activo]>=precio_objetivo_venta[dia,activo])/(ventana+1)
        
        # Calculamos la última vez que se ha cumplido la condición de venta en la ventana temporal.
        if(frecuencia_condicion_venta[dia,activo]>0){
          ultima_condicion_venta[dia,activo]<-min(which((maximo[dia:(dia+ventana),activo]>=precio_objetivo_venta[dia,activo]) == TRUE))
        }else{
          ultima_condicion_venta[dia,activo]<-ventana*2
        }
        
        # Fabricamos un ranking de asignación de recursos en función de la probabilidad de ocurrencia.
        ranking[dia,activo]<-(frecuencia_condicion_venta[dia,activo]/ultima_condicion_venta[dia,activo])*(frecuencia_condicion_compra[dia,activo]/ultima_condicion_compra[dia,activo])
      }
      
      # Transformamos la probabilidad de ocurrencia en un ranking (el activo conmayor puntuación será el primero que recibirá recursos disponibles).
      ranking[dia,]<-rank(ranking[dia,])
    }
    
    datos_ranking_asignacion<- list(volumen_minimo, volumen_maximo, frecuencia_condicion_compra, ultima_condicion_compra, frecuencia_condicion_venta, ultima_condicion_venta, ranking)
    
    return(datos_ranking_asignacion)
  }
  
  
# Generamos la recomendación para el día siguiente
  library(xlsx)
  generar_recomendacion<-function(datos_descargados, activos_seleccionados, datos_ranking_asignacion, beneficio_objetivo_por_operacion, stop_loss, comision, comision_minima){
    
    print("Generando la recomendación para mañana")
    
    cierre<-remove.factors(datos_descargados[[2]])
      cierre<-cierre %>% mutate_all(as.numeric)
    volumen<-remove.factors(datos_descargados[[5]])
      volumen<-volumen %>% mutate_all(as.numeric)
    divisa<-remove.factors(datos_descargados[[7]])
      divisa<-divisa %>% mutate_all(as.numeric)
    precio_objetivo_compra<-remove.factors(activos_seleccionados[[5]])
      precio_objetivo_compra<-precio_objetivo_compra %>% mutate_all(as.numeric)
    precio_objetivo_venta<-remove.factors(activos_seleccionados[[6]])
      precio_objetivo_venta<-precio_objetivo_venta %>% mutate_all(as.numeric)
    volumen_minimo<-remove.factors(datos_ranking_asignacion[[1]])
      volumen_minimo<-volumen_minimo %>% mutate_all(as.numeric)
    volumen_maximo<-remove.factors(datos_ranking_asignacion[[2]])
      volumen_maximo<-volumen_maximo %>% mutate_all(as.numeric)
    frecuencia_condicion_compra<-remove.factors(datos_ranking_asignacion[[3]])
      frecuencia_condicion_compra<-frecuencia_condicion_compra %>% mutate_all(as.numeric)
    frecuencia_condicion_venta<-remove.factors(datos_ranking_asignacion[[5]])
      frecuencia_condicion_venta<-frecuencia_condicion_venta %>% mutate_all(as.numeric)
    ranking<-remove.factors(datos_ranking_asignacion[[7]])
      ranking<-ranking %>% mutate_all(as.numeric)
    
    recomendacion_mañana<-as.data.frame(matrix(0,nrow=15,ncol=dim(cierre)[2],byrow=F))
      recomendacion_mañana[1,]<-colnames(cierre) # Nombre de los activos.
      recomendacion_mañana[2,]<-cierre[1,] # Precio de cierre de ayer.
      recomendacion_mañana[3,]<-precio_objetivo_compra[1,] # Precio objetivo de compra.
      recomendacion_mañana[4,]<-precio_objetivo_venta[1,] # Precio objetivo de venta.
      recomendacion_mañana[5,]<-precio_objetivo_venta[1,]-precio_objetivo_compra[1,]# Horquilla.
      recomendacion_mañana[6,]<-(precio_objetivo_venta[1,]-precio_objetivo_compra[1,])/precio_objetivo_compra[1,]# Rentabilidad esperada
      recomendacion_mañana[7,]<- frecuencia_condicion_compra[1,]*frecuencia_condicion_venta[1,] # Probabilidad de ocurrencia
      recomendacion_mañana[8,]<-precio_objetivo_compra[1,]-(precio_objetivo_venta[1,]-precio_objetivo_compra[1,])*stop_loss# Stop loss
      recomendacion_mañana[9,]<- beneficio_objetivo_por_operacion # Beneficio objetivo por operación
    
    for (accion in 1:dim(recomendacion_mañana)[2]){
      
      # Bº=(pv-pc)*nacc-com
        # 100=(12-11)*nacc-(0,008*nacc*12)-(0,008*nacc*11)
        # 100=1nacc-0,096nacc-0,088nacc
        # 100=1nacc-0,096nacc-0,088nacc
        # 100=0,816nacc
        # nacc=100/0,816 --> 122,54 acciones a comprar
      
      if ((precio_objetivo_venta[1,accion]/divisa[1,1]-precio_objetivo_compra[1,accion]/divisa[1,1]-comision*precio_objetivo_venta[1,accion]/divisa[1,1]-comision*precio_objetivo_compra[1,accion]/divisa[1,1])>0){
        
        recomendacion_mañana[10,accion]<- round(beneficio_objetivo_por_operacion/(precio_objetivo_venta[1,accion]/divisa[1,1]-precio_objetivo_compra[1,accion]/divisa[1,1]-comision*precio_objetivo_venta[1,accion]/divisa[1,1]-comision*precio_objetivo_compra[1,accion]/divisa[1,1]),digits = 0)+1# Num de acciones
        recomendacion_mañana[11,accion]<-as.numeric(as.character(recomendacion_mañana[10,accion]))*precio_objetivo_compra[1,accion]/divisa[1,1]*comision+as.numeric(as.character(recomendacion_mañana[10,accion]))*precio_objetivo_venta[1,accion]/divisa[1,1]*comision # Comisiones
        
        # Comprobamos que la comisión es mayor que la comisión mínima.
        if (as.numeric(as.character(recomendacion_mañana[11,accion]))<comision_minima*2){
          
          recomendacion_mañana[10,accion]<-round((beneficio_objetivo_por_operacion+comision_minima*2)/(precio_objetivo_venta[1,accion]/divisa[1,1]-precio_objetivo_compra[1,accion]/divisa[1,1]),digits = 0)
          recomendacion_mañana[11,accion]<-comision_minima*2
        }
        
      }else{ # Las comisiones son superiores a los beneficios. No compensa hacer la operación.
        recomendacion_mañana[10,accion]<-0
        recomendacion_mañana[11,accion]<-0
      }
    }
    
    recomendacion_mañana[12,]<-(precio_objetivo_compra[1,]/divisa[1,1])*as.numeric(as.character(recomendacion_mañana[10,]))# Capital invertido
    recomendacion_mañana[13,]<-as.numeric(as.character(recomendacion_mañana[10,]))/colMeans(volumen)# % sobre volumen diario
    recomendacion_mañana[14,]<-volumen_minimo[1,] # Volumen mínimo
    recomendacion_mañana[15,]<-volumen_maximo[1,]# Volumen máximo
    
    # Ordenamos las columnas por su probabilidad de asignación de capital.
    indice<-order(ranking[1,],decreasing = T)
    recomendacion_mañana<-recomendacion_mañana[,indice]
    
    recomendacion_mañana<-as.data.frame(t(recomendacion_mañana))
    colnames(recomendacion_mañana)<-c("Activo", "Ultimo cierre (en div)", "Precio obj compra (en div)", "Precio obj venta (en div)", "Horquilla", "Rentabilidad esperada", "Probabilidad ocurrencia",
                                      "Stop loss (en div)", "Bº obj por operación (en eur)", "Nº de acc a comprar", "Comisiones (en eur)", "Capital invertido (en eur)", "% sobre volumen diario", "Vol mínimo comprar (nº acc)", "Vol máximo (nº acc)")
    
    for (columna in 2:dim(recomendacion_mañana)[2]){
      recomendacion_mañana[,columna] <- round(as.numeric(as.character(recomendacion_mañana[,columna])), digits=3)
    }
    
    write.xlsx(recomendacion_mañana, "recomendacion_mañana.xlsx")
    
    return(recomendacion_mañana)
  }

   
# Calculamos el histórico de operaciones.
  library('scales')
  calcular_historico_operaciones_capital_dinamico<-function(datos_descargados,activos_seleccionados, datos_ranking_asignacion, stop_loss, asignacion_maxima, comision, comision_minima, beneficio_objetivo_por_operacion){
    
    print("Calculando histórico de operaciones")
    
    cierre<-remove.factors(datos_descargados[[2]])
      fechas<-rownames(cierre)
      cierre<-cierre %>% mutate_all(as.numeric)
      rownames(cierre)<-fechas
      cierre<-cierre[dim(cierre)[1]:1,]
    prec_apertura<-remove.factors(datos_descargados[[1]])
      prec_apertura<-prec_apertura %>% mutate_all(as.numeric)
      rownames(prec_apertura)<-fechas
      prec_apertura<-prec_apertura[dim(prec_apertura)[1]:1,]
    cot_max<-remove.factors(datos_descargados[[3]])
      cot_max<-cot_max %>% mutate_all(as.numeric)
      rownames(cot_max)<-fechas
      cot_max<-cot_max[dim(cot_max)[1]:1,]
    cot_min<-remove.factors(datos_descargados[[4]])
      cot_min<-cot_min %>% mutate_all(as.numeric)
      rownames(cot_min)<-fechas
      cot_min<-cot_min[dim(cot_min)[1]:1,]
    divisa<-remove.factors(datos_descargados[[7]])
      divisa<-divisa %>% mutate_all(as.numeric)
      rownames(divisa)<-fechas
      divisa<-divisa[dim(divisa)[1]:1,]
    maestro_valores<-remove.factors(datos_descargados[[9]])
    
    rec_precio_compra<-remove.factors(activos_seleccionados[[5]])
      rec_precio_compra<-rec_precio_compra %>% mutate_all(as.numeric)
      rownames(rec_precio_compra)<-fechas
      rec_precio_compra<-rec_precio_compra[dim(rec_precio_compra)[1]:1,]
      rec_precio_compra[2:dim(rec_precio_compra)[1],]<-rec_precio_compra[1:(dim(rec_precio_compra)[1]-1),] # La recomendación de precio es para el día siguiente. Ponemos la recomendación en su día correspondiente.
      
    rec_precio_venta<-remove.factors(activos_seleccionados[[6]])
      rec_precio_venta<-rec_precio_venta %>% mutate_all(as.numeric)
      rownames(rec_precio_venta)<-fechas
      rec_precio_venta<-rec_precio_venta[dim(rec_precio_venta)[1]:1,]
      rec_precio_venta[2:dim(rec_precio_venta)[1],]<-rec_precio_venta[1:(dim(rec_precio_venta)[1]-1),]
      
    frecuencia_condicion_compra<-remove.factors(datos_ranking_asignacion[[3]])
      frecuencia_condicion_compra<-frecuencia_condicion_compra %>% mutate_all(as.numeric)
      rownames(frecuencia_condicion_compra)<-fechas
      frecuencia_condicion_compra<-frecuencia_condicion_compra[dim(frecuencia_condicion_compra)[1]:1,]
    frecuencia_condicion_venta<-remove.factors(datos_ranking_asignacion[[5]])
      frecuencia_condicion_venta<-frecuencia_condicion_venta %>% mutate_all(as.numeric)
      rownames(frecuencia_condicion_venta)<-fechas
      frecuencia_condicion_venta<-frecuencia_condicion_venta[dim(frecuencia_condicion_venta)[1]:1,]
    
    # Comprobamos qué recomendaciones de compra se ejecutaron. Comparamos el precio de compra recomendado con el precio mínimo y máximo del día. Esto me indica las compras realizadas, pero no me indica ni qué, ni cuando.
    indice<-rec_precio_compra>cot_min & rec_precio_compra<cot_max
    indice[indice==T]<-1
    indice[indice==F]<-0
    indice<-as.data.frame(indice)
    compras_ejecutadas<-rec_precio_compra*indice
    
    ventas_ejecutadas<-as.data.frame(matrix(0,nrow=dim(compras_ejecutadas)[1],ncol=dim(compras_ejecutadas)[2],byrow=F))
    colnames(ventas_ejecutadas)<-colnames(compras_ejecutadas)
    rownames(ventas_ejecutadas)<-rownames(compras_ejecutadas)  
    
    # Las recomendaciones de precio de venta están ligadas al precio de compra. Aislamos los precios de venta objetivo.
    precio_venta_objetivo<-rec_precio_venta*indice
    
    # Para cada operación, queremos hacer un seguimiento de cuantos días se ha tardado en alcanzar la venta y, por lo tanto, de su rentabilidad.
    # Localizamos la posición relativa de las compras ejecutadas (fila y columna) para obtener el activo comprado y su fecha de compra.
    compras_ejecutadas<-as.matrix(compras_ejecutadas)
    posicion_relativa<-which(compras_ejecutadas!=0,arr.ind = T)
    
    # Creamos un DF donde guardaremos el histórico de las operaciones.
    resumen<-as.data.frame(matrix(0,nrow=dim(posicion_relativa)[1],ncol=16,byrow=F)) 
    colnames(resumen)<-c("Activo comprado", "Ticker", "Precio compra", "F compra", "Precio venta", "F venta", "Dias", "Rent", "Precio actual", "Rdo poten", "EUR asig", "Nº acciones","Comisiones", "Resultado", "Prob ocurrencia", "Horquilla")
    
    # Guardamos el nombre de los activos comprados.
    resumen$`Activo comprado`<-colnames(rec_precio_compra)[c(as.vector(posicion_relativa[,2]))]
    
    # Guardamos el Ticker de los activos. Hay que quitar del maestro de valores aquellos activos que no nos hemos podido descargar.
    indice<-maestro_valores$Nombre %in% colnames(cierre)
    maestro_valores<-maestro_valores[indice,]
    resumen$Ticker[1:length(resumen$Ticker)]<-maestro_valores$Ticker[c(as.vector(posicion_relativa[,2]))]
    resumen$Ticker<-unlist(resumen$Ticker)
    
    # Guardamos los precios de compra.
    resumen$`Precio compra`<-rec_precio_compra[rec_precio_compra>cot_min & rec_precio_compra<cot_max]
    
    # Guardamos la fecha de compra
    resumen$`F compra`<-rownames(rec_precio_compra)[c(as.vector(posicion_relativa[,1]))]
    
    # Guardamos el precio de venta.
    resumen$`Precio venta`<-rec_precio_venta[rec_precio_compra>cot_min & rec_precio_compra<cot_max]
    
    # Calculamos la horquilla.
    resumen$Horquilla<-resumen$`Precio venta`-resumen$`Precio compra`
    
    divisa_a_aplicar<-as.data.frame(resumen$`EUR asig`) 
    for (dia in 1:dim(divisa_a_aplicar)[1]){
      divisa_a_aplicar[dia,1]<-divisa[resumen$`F compra`[dia]==rownames(divisa),1]
    }
    
    # Calculamos la fecha de venta de cada operación realizada.
    if (dim(posicion_relativa)[1]>0){
      for (elemento in 1:dim(posicion_relativa)[1]){
        
        # El precio de venta debe buscarse entre la fecha de compra y el final del periodo a analizar.
          # Buscamos las fechas (posiciones de las fechas), donde se cumple la condición de que el max del día supera al precio objetivo de venta.
          # Si el precio de venta es superior al mínimo vendemos al precio objetivo. Si es inferior al mínimo, vendemos al precio de apertura.
          
        posicion_fechas_venta<-which(cot_max[,posicion_relativa[elemento,2]]>=resumen[elemento,5],arr.ind = T)
        
        # Buscamos la primera fecha que sea superior o igual al día de compra. Localizamos la fecha de venta, pero esto no nos indica el precio de venta.
        resumen$`F venta`[elemento]<-rownames(rec_precio_compra)[posicion_fechas_venta[posicion_relativa[elemento,1]<posicion_fechas_venta][1]]
        
        # Asignamos la venta en el día y activo correspondiente.
          # La fila (fecha) es la que sea igual a resumen$`F venta`[elemento]
          # La columna (activo) es la que sea igual a resumen$`Activo comprado`[elemento]
          # El valor que queremos meter (precio de venta) depende:
          # Comparamos el precio de venta objetivo con el mínimo del día. Si está dentro del rango diario es: resumen$`Precio venta`[elemento]
          # Pero si está fuera del rango, vendemos al precio de apertura. Es decir, si el objetivo de venta era a 10.51 € y el mercado abre en 10.80 €, vendemos, pero al precio del mercado.
          
        # Comprobamos que la venta se ha ejecutado.
        if(!is.na(resumen$`F venta`[elemento])){
          
          if (resumen$`Precio venta`[elemento]<prec_apertura[rownames(prec_apertura)==resumen$`F venta`[elemento],colnames(prec_apertura)==resumen$`Activo comprado`[elemento]]){
            
            # Modificamos el precio de venta.
            resumen$`Precio venta`[elemento]<-prec_apertura[rownames(prec_apertura)==resumen$`F venta`[elemento],colnames(prec_apertura)==resumen$`Activo comprado`[elemento]]
          }
          
          ventas_ejecutadas[rownames(ventas_ejecutadas)==resumen$`F venta`[elemento],colnames(ventas_ejecutadas)==resumen$`Activo comprado`[elemento]]<-resumen$`Precio venta`[elemento]
        }
        
        # Calculamos el capital asignado
          # Bº=(pv-pc)*nacc-com
          # 100=(12-11)*nacc-(0,008*nacc*12)-(0,008*nacc*11)
          # 100=1nacc-0,096nacc-0,088nacc
          # 100=1nacc-0,096nacc-0,088nacc
          # 100=0,816nacc
          # nacc=100/0,816 --> 122,54 acciones a comprar
        
        # Comprobamos que los beneficios son superiores a las comisiones
        if ((resumen$`Precio venta`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]-resumen$`Precio compra`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1])>0){
          
          resumen$`Nº acciones`[elemento]<- (round(beneficio_objetivo_por_operacion/(resumen$`Precio venta`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]-resumen$`Precio compra`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]-comision*resumen$`Precio venta`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]-comision*resumen$`Precio compra`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]),digits = 0)+1)# Num de acciones
          resumen$`EUR asig`[elemento]<-resumen$`Nº acciones`[elemento]*resumen$`Precio compra`[elemento]
          resumen$Comisiones[elemento]<-round(resumen$`Nº acciones`[elemento]*resumen$`Precio compra`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]*comision+resumen$`Nº acciones`[elemento]*resumen$`Precio venta`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]*comision,digits = 2) # Comisiones
          
          # Comprobamos que la comisión es mayor que la comisión mínima.
          if (as.numeric(as.character(resumen$Comisiones[elemento]))<comision_minima*2){
            
            resumen$`Nº acciones`[elemento]<-round((beneficio_objetivo_por_operacion+comision_minima*2)/(resumen$`Precio venta`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]-resumen$`Precio compra`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1]),digits = 0) # Este cálculo se hace en la fecha de compra, desconocemos la cotización de la divisa el dia de la venta, porque no sabemos cuando ocurrirá.
            resumen$`EUR asig`[elemento]<-resumen$`Nº acciones`[elemento]*resumen$`Precio compra`[elemento]
            resumen$Comisiones[elemento]<-round(comision_minima*2,digits = 2)
          }
          
        }else{ # Las comisiones son superiores a los beneficios. No compensa hacer la operación.
          resumen$`Nº acciones`[elemento]<-0
          resumen$`EUR asig`[elemento]<-0
          resumen$Comisiones[elemento]<-0
        }
        
        # Comprobamos si ha saltado el stop loss
        # Para cada compra comprobamos si ha saltado el stop loss de precio.
        # Si se ha realizado la venta, comprobamos si hay algún precio que hubiera hecho saltar el stop loss entre la fecha de compra y venta.
        # Si no se ha realizado la venta, comprobamos si hay algún precio que hubiera hecho saltar el stop loss entre la fecha de compra y hoy.
        
        if(!is.na(resumen$`F venta`[elemento])){
          fecha_limite<-resumen$`F venta`[elemento]
        }else{
          fecha_limite<-rownames(cot_min)[dim(cot_min)[1]]
        }
        
        # Buscamos si ha habido algún precio mínimo por debajo del stop loss entre la fecha de compra y la fecha límite. Buscamos la primera fecha que cumpla esta condición.
        precio_stop_loss<-resumen$`Precio compra`[elemento]-(resumen$`Precio venta`[elemento]-resumen$`Precio compra`[elemento])*stop_loss
        posicion_fecha_compra<- which(resumen$`F compra`[elemento]==rownames(cot_min),arr.ind = T)
        posicion_fecha_venta<- which(fecha_limite==rownames(cot_min),arr.ind = T)
        
        # Buscamos la primera fecha que sea inferior o igual al día de compra. Localizamos la fecha pero esto no nos indica el precio de venta.
        indice_stop<-cot_min[posicion_fecha_compra:posicion_fecha_venta,resumen$`Activo comprado`[elemento]==colnames(cot_min)]<precio_stop_loss & cot_min[posicion_fecha_compra:posicion_fecha_venta,resumen$`Activo comprado`[elemento]==colnames(cot_min)]>0
        
        if(length(indice_stop[indice_stop==TRUE])>0){
          
          fecha_stop<-posicion_fecha_compra + min(which(indice_stop == TRUE))-1
          
          # Modificamos la fecha de venta.
          resumen$`F venta`[elemento]<-rownames(cot_min)[fecha_stop]
          
          # Modificamos el precio de venta.
          # Comprobamos que el precio del stop loss se ha dado en la sesión (menor que el precio de apertura). Si no es así, y abre por debajo, vendemos al precio de apertura.
          if (precio_stop_loss<prec_apertura[fecha_stop,resumen$`Activo comprado`[elemento]==colnames(prec_apertura)]){
            
            resumen$`Precio venta`[elemento]<-precio_stop_loss
            
          }else{
            
            # El precio está fuera de rango. Vendemos al precio de apertura.
            resumen$`Precio venta`[elemento]<-prec_apertura[fecha_stop,resumen$`Activo comprado`[elemento]==colnames(prec_apertura)]
          }
        }
        
        # Guardamos la probabilidad de ocurrencia de la operación.
        resumen$`Prob ocurrencia`[elemento]<-frecuencia_condicion_compra[resumen$`F compra`[elemento]==rownames(frecuencia_condicion_compra),resumen$`Activo comprado`[elemento]==colnames(frecuencia_condicion_compra)]*frecuencia_condicion_venta[resumen$`F compra`[elemento]==rownames(frecuencia_condicion_compra),resumen$`Activo comprado`[elemento]==colnames(frecuencia_condicion_compra)]
      }
    }
    
    # Calculamos el número de días que han transcurrido entre la compra y la venta.
    resumen$Dias<-as.Date(resumen$`F venta`)-as.Date(resumen$`F compra`)
    
    # Buscamos las operaciones que se han producido en el mismo día. En ese caso, indicamos que la duración es de 1 día (y no de 0) para evitar infinitos.
    indice_ceros<-resumen$Dias==0
    resumen$Dias[indice_ceros]<-1
    
    # Calculamos la rentabilidad de la operación
    resumen$Rent<-(resumen$`Precio venta`-resumen$`Precio compra`)/resumen$`Precio compra`
    
    # Ordenamos el DF por las fechas de compra.
    resumen <- resumen[order(resumen$`F compra`),] 
    
    # Buscamos asignaciones de dinero nulas (las comisiones eran superiores a los beneficios). No realizamos esas compras.
    indice_nulos<-!resumen$`EUR asig`==0
    resumen<-resumen[indice_nulos,]
    
    # Buscamos asignaciones de dinero infinito (NO realizamos esas compras). 
    indice_infinitos<-!is.infinite(resumen$`EUR asig`)
    resumen<-resumen[indice_infinitos,]
    
    # Eliminamos las operaciones que superen la asignación máxima.
    indice_operaciones_a_eliminar<-!(as.vector(resumen$`EUR asig`)>=asignacion_maxima)
    resumen<-resumen[indice_operaciones_a_eliminar,]
    
    # Metemos el precio actual de cotización en las operaciones abiertas.
    posiciones_abiertas<-as.numeric(rownames(resumen)[is.na(resumen$`F venta`)])
    
    # Comprobamos si existen posiciones abiertas.
    if (length(posiciones_abiertas)>0){
      
      # Obtenemos el precio actual
      for (operacion in posiciones_abiertas){
        resumen$`Precio actual`[as.numeric(rownames(resumen))==operacion]<-cierre[dim(cierre)[1],resumen$`Activo comprado`[as.numeric(rownames(resumen))==operacion]==colnames(cierre)]
      } 
      
      resumen$`Precio actual`<-as.numeric(unlist(resumen$`Precio actual`))
      
      for (operacion in posiciones_abiertas){
        # Calculamos el resultado potencial si vendiésemos ahora mismo las posiciones abiertas.
        resumen$`Rdo poten`[as.numeric(rownames(resumen))==operacion]<-round((resumen$`Precio actual`[as.numeric(rownames(resumen))==operacion]/divisa[dim(divisa)[1],1]-resumen$`Precio compra`[as.numeric(rownames(resumen))==operacion]/divisa[rownames(divisa)==resumen$`F compra`[as.numeric(rownames(resumen))==operacion],1])*resumen$`Nº acciones`[as.numeric(rownames(resumen))==operacion]-resumen$Comisiones[as.numeric(rownames(resumen))==operacion], digits = 2)
      }
      
      # Calculamos la rentabilidad actual de las operaciones abiertas
      resumen$Rent[is.na(resumen$`F venta`)]<-(resumen$`Precio actual`[is.na(resumen$`F venta`)]-resumen$`Precio compra`[is.na(resumen$`F venta`)])/resumen$`Precio compra`[is.na(resumen$`F venta`)]
    }
    
    if (dim(resumen)[1]>0){
      for (elemento in 1:dim(resumen)[1]){
        if(!is.na(resumen$`F venta`)[elemento]){
          # Calculamos el resultado de las operaciones cerradas (las abiertas se pueden ver en el resultado potencial)
          resumen$Resultado[elemento]<-round((resumen$`Precio venta`[elemento]/divisa[rownames(divisa)==resumen$`F venta`[elemento],1]-resumen$`Precio compra`[elemento]/divisa[rownames(divisa)==resumen$`F compra`[elemento],1])*resumen$`Nº acciones`[elemento]-resumen$Comisiones[elemento],digits = 2)
        }  
      }
    }
    
    # Ponemos la rentabilidad y la probabilidad de ocurrencia en formato de porcentaje.
    resumen$Rent<-percent(resumen$Rent)
    resumen$`Prob ocurrencia`<-percent(resumen$`Prob ocurrencia`)
    
    # Redondeamos los resultados.
    resumen$`Precio compra`<-round(resumen$`Precio compra`, digits = 3)
    resumen$`Precio venta`<-round(resumen$`Precio venta`, digits = 3)
    resumen$Horquilla<-round(resumen$Horquilla, digits = 3)
    
    # Hacemos globales estas variables (las necesitaresmos cuando hagamos los gráficos de velas)
    compras_ejecutadas<<-as.data.frame(compras_ejecutadas)
    ventas_ejecutadas<<-ventas_ejecutadas
    
    return(resumen)
  }


# Calculamos los flujos de tesorería.
  calcular_flujos_tesoreria<-function(historico_operaciones_capital_dinamico, datos_descargados){
    
    divisa<-remove.factors(datos_descargados[[7]])
      fechas<-rownames(divisa)
      divisa<-divisa %>% mutate_all(as.numeric)
      rownames(divisa)<-fechas
      divisa<-divisa[dim(divisa)[1]:1,]
    resumen<-historico_operaciones_capital_dinamico
    
    print("Calculando los flujos de tesorería")
    
    # Cremos un DF donde guardaremos los datos de los flujos.
    flujos<-as.data.frame(matrix(0,nrow=dim(resumen)[1]*2,ncol=14,byrow=F)) 
      colnames(flujos)<-c("Activo", "Fecha", "Sentido", "PC en div", "Nº acc", "PV en div", "Dias", "Flujo EUR", "Dispo EUR","Inver EUR", "Bº EUR", "Bº acu EUR", "Cot EUR/Div", "Comisiones EUR")
    
    # Separamos las compras de las ventas en el DF.
    flujos[1:(dim(flujos)[1]/2), 1]<-resumen$`Activo comprado`
    flujos[1:(dim(flujos)[1]/2), 2]<-resumen$`F compra`
    flujos[1:(dim(flujos)[1]/2), 3]<-"Compra"
    flujos[1:(dim(flujos)[1]/2), 4]<-resumen$`Precio compra`
    flujos[1:(dim(flujos)[1]/2), 5]<-resumen$`Nº acciones`
    flujos[1:(dim(flujos)[1]/2), 14]<-resumen$Comisiones/2
    
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 1]<-resumen$`Activo comprado`
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 2]<-resumen$`F venta`
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 3]<-"Venta"
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 4]<-resumen$`Precio compra`
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 5]<-resumen$`Nº acciones`
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 6]<-resumen$`Precio venta`
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 7]<-resumen$Dias
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 11]<-resumen$Resultado
    flujos[((dim(flujos)[1]/2)+1):dim(flujos)[1], 14]<-resumen$Comisiones/2
    
    # Añadimos las divisas.
    for (dia in 1:dim(flujos)[1]){
      if(!is.na(flujos$Fecha[dia])){
        flujos[dia,13]<-divisa[flujos$Fecha[dia]==rownames(divisa),1]
      }
    }
    # En los días en los que no se ha producido la venta, no conocemos la cotización de la divisa. La igualamos a la última conocida.
    flujos[flujos[,13]==0,13]<-divisa[dim(divisa)[1],1]
    
    # Ordenamos el DF por fecha y por sentido (primero las compras y luego las ventas).
    flujos<-flujos[with(flujos, order(flujos$Fecha,desc(flujos$Sentido))), ]
    
    # Calculamos el beneficio acumulado.
    flujos$`Bº acu EUR`<-Reduce("+", flujos$`Bº EUR`, accumulate = TRUE) 
    
    # Calculamos los flujos de cada operación
    indice<-flujos$Sentido=="Venta"
    flujos$`Flujo EUR`[indice]<- (flujos[indice,6]*flujos$`Nº acc`[indice])/flujos[indice,13]-flujos[indice,14] # Calculamos las ventas.
    flujos$`Flujo EUR`[!indice]<- ((flujos[!indice,4]*flujos$`Nº acc`[!indice]*-1))/flujos[!indice,13]-flujos[!indice,14] # Calculamos las compras.
    
    # Calculamos el capital disponible.
    flujos$`Dispo EUR`[1]<-(flujos[1,4]*flujos$`Nº acc`[1]/flujos[1,13]+flujos$`Comisiones EUR`[1])*-1 # (PC * nacc + comisión)*-1
    flujos<-calcular_disponible(flujos)
    
    # Calculamos el capital inicial de la tesorería y volvemos a calcular el capital disponible.
    tesorería_inicial<-min(flujos$`Dispo EUR`)*-1
    flujos$`Dispo EUR`[1]<-flujos$`Bº EUR`[1]+flujos$`Flujo EUR`[1]+tesorería_inicial
    flujos<-calcular_disponible(flujos)
    
    # Calculamos el capital invertido (PC * N acc). Nunca se tiene en cuenta el PV para hacer este cálculo (ni las comisiones).
    flujos$`Inver EUR`[1]<-flujos[1,4]*flujos$`Nº acc`[1]/flujos[1,13]
    for (dia in 2:dim(flujos)[1]){
      if(flujos$Sentido[dia]=="Compra"){
        flujos$`Inver EUR`[dia]<-flujos$`Inver EUR`[dia-1]+(flujos[dia,4]*flujos$`Nº acc`[dia])/flujos[dia,13]
      }else{
        flujos$`Inver EUR`[dia]<-flujos$`Inver EUR`[dia-1]+(flujos[dia,4]*flujos$`Nº acc`[dia]*-1)/flujos[dia,13]
        
        # Comprobamos si no tenemos ninguna posición abierta.
        if(length(flujos$Sentido[1:dia][flujos$Sentido[1:dia]=="Compra"])==length(flujos$Sentido[1:dia][flujos$Sentido[1:dia]=="Venta"])){
          flujos$`Inver EUR`[dia]<-0
        }
      }
    }
    
    return(flujos)
  }
  
  
# Calculamos el capital disponible en cada instante.
  calcular_disponible<-function(flujos){
    
    for (dia in 2:dim(flujos)[1]){
      
      if(flujos$Sentido[dia]=="Compra"){
        # Si es una compra el disponible N = disponible N-1 + (PC * nacc + comisión)*-1
        flujos$`Dispo EUR`[dia]<-flujos$`Dispo EUR`[dia-1]+(flujos[dia,4]*flujos$`Nº acc`[dia]/flujos[dia,13]+flujos[dia,14])*-1
        
      }else{
        # Si es una venta el disponible N = disponible N-1 + (PV-PC)*Nº acc - comision + nacc* PC (lo invertido)
        flujos$`Dispo EUR`[dia]<-flujos$`Dispo EUR`[dia-1]+((flujos[dia,6]-flujos[dia,4])*flujos$`Nº acc`[dia])/flujos[dia,13]-flujos[dia,14]+(flujos[dia,4]*flujos$`Nº acc`[dia])/flujos[dia,13]
      }
    }
    return(flujos)
  }

  
# Fabricamos un resumen de los resultados del algoritmos
  resumen_operaciones_flujos<-function(historico_operaciones_capital_dinamico, flujos_tesoreria){
    
    operaciones<-historico_operaciones_capital_dinamico
    
    resumen_resultados<-as.data.frame(matrix(0,nrow=11,ncol=1,byrow=F))
    colnames(resumen_resultados)<-c("Capital con techo dinámico")
    rownames(resumen_resultados)<-c("Intervalo de fechas", "Pérdida máxima y tiempo de recuperación", "Capital máximo necesario", "Capital medio invertido",
                                    "Capital invertido actualmente", "Bº acumulado vs Rdo potencial operaciones abiertas", "Duración media de cada operación", 
                                    "Horquilla media", "Nº operaciones cerradas con Bº vs Pda", "Bº medio por operación vs pda media por operación","Nº de operaciones abiertas actualmente")
    
    resumen_resultados$`Capital con techo dinámico`[1]<-paste(fecha_inicio,fecha_fin,sep=" - ") # Intervalo de fechas
    
    # Pérdida máxima y tiempo de recuperación
    if (min(flujos_tesoreria$`Bº acu EUR`)<0){
      
      minimo<-min(flujos_tesoreria$`Bº acu EUR`)
      fecha_perdida<-flujos_tesoreria$Fecha[flujos_tesoreria$`Bº acu EUR`==minimo]
      minimo<-round(minimo)
      fecha_recuperacion<-flujos_tesoreria$Fecha[min(which(flujos_tesoreria$`Bº acu EUR` > 0 & flujos_tesoreria$Fecha > fecha_perdida))]
      resumen_resultados$`Capital con techo dinámico`[2]<- paste(minimo, " en ", fecha_perdida, " recuperado el ", fecha_recuperacion, sep="") 
      
    }else{
      resumen_resultados$`Capital con techo dinámico`[2]<- paste("El algoritmo no entra en pérdidas") 
    }
    
    resumen_resultados$`Capital con techo dinámico`[3]<-formatC(round(flujos_tesoreria$`Dispo EUR`[1], digits = 2), format="f", big.mark = ",", digits=2) # Capital máximo necesario
    resumen_resultados$`Capital con techo dinámico`[4]<-formatC(round(mean(flujos_tesoreria$`Inver EUR`[!is.na(flujos_tesoreria$Fecha)]), digits = 2), format="f", big.mark = ",", digits=2) # Capital medio invertido
    resumen_resultados$`Capital con techo dinámico`[5]<-formatC(round(sum(flujos_tesoreria$`Nº acc`[is.na(flujos_tesoreria$Fecha)]*flujos_tesoreria$`PC en div`[is.na(flujos_tesoreria$Fecha)]/flujos_tesoreria$`Cot EUR/Div`[is.na(flujos_tesoreria$Fecha)]), digits = 2), format="f", big.mark = ",", digits=2) # Capital invertido actualmente
    resumen_resultados$`Capital con techo dinámico`[6]<-paste(formatC(round(flujos_tesoreria$`Bº acu EUR`[dim(flujos_tesoreria[1])], digits = 2), format="f", big.mark = ",", digits=2) ,formatC(round(sum(operaciones$`Rdo poten`), digits = 2), format="f", big.mark = ",", digits=2),sep= " vs ") # Bº acumulado vs Rdo potencial operaciones abiertas
    resumen_resultados$`Capital con techo dinámico`[7]<-paste(round(mean(operaciones$Dias[!is.na(operaciones$`F venta`)]), digits = 0), "dias",sep= " ")# Duración media operaciones
    resumen_resultados$`Capital con techo dinámico`[8]<-round(mean(operaciones$Horquilla[!is.na(operaciones$`F venta`)]), digits = 2)  # Horquilla media
    resumen_resultados$`Capital con techo dinámico`[9]<-paste(length(operaciones$Resultado[operaciones$Resultado>0]), length(operaciones$Resultado[operaciones$Resultado<0]),sep= " vs ")  # Nº operaciones cerradas con Bº vs Pda
    resumen_resultados$`Capital con techo dinámico`[10]<-paste(round(mean(operaciones$Resultado[operaciones$Resultado>0]), digits = 2), round(mean(operaciones$Resultado[operaciones$Resultado<0]), digits = 2),sep= " vs ") # Bº medio por operación vs pda media por operación.
    resumen_resultados$`Capital con techo dinámico`[11]<-length(flujos_tesoreria$`Inver EUR`[is.na(flujos_tesoreria$Fecha)]) # "Nº de operaciones abiertas actualmente"
    
    return(resumen_resultados)
  }

