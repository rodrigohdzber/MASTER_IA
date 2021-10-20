
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
