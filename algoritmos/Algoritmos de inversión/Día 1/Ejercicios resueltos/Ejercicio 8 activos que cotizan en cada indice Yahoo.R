# Obtén los activos que cotizan en cada índice y en qué divisa están cotizando
# Recomendación: Yahoo Finance 
# Tiempo objetivo: 45 minutos
# Los componentes de cada índice se renuevan cada x meses. La mejora manera de descargarnos datos sobre un índice no es consultar una lista fija de activos, sino consultar previamente qué activos están cotizando en cada índice antes de hacer la consulta activo por activo. 

library(RCurl)
library(stringr)
library(stringi)
library(rvest)

tickers_de_indices<-c("%5EBFX","%5EBVSP", "%5EFCHI", "%5EFTSE", "%5EGDAXI", "%5EHSI", "%5EIBEX", "%5EMXX", "%5EJKSE", "%5EMERV", "%5EOMXSPI", "%5EOSEAX", "%5ESSMI", "%5ESTI")

for(indice in tickers_de_indices){
  
  # Construimos la url que contiene la información de los activos de cada índice.
  web<-read_html(paste("https://es.finance.yahoo.com/quote/",indice,"/components/", sep = ""))
  
  datos_web<- web %>%
    html_nodes("td") %>% 
    html_text() 
  datos_web<-as.data.frame(datos_web) # Obtenemos cada celda de la tabla por separado. 
  datos_web<-as.data.frame(datos_web[1:(dim(datos_web)[1]-2),1]) # Eliminamos las últimas filas.
  
  if (dim(datos_web)[1]>1){ # Comprobamos si Yahoo publica algún activo del índice. Si la longitud es 1, no.
    
    # Reconstruimos la tabla. Tiene 6 columnas (símbolo, nombre de la empresa, último precio, cambio, cambio % y volumen.)
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
          
          # Mostramos la divisa por pantalla.
          print(divisa)
        }
      }
    }
    
  } else {
    print(paste("El índice",indice,"no tiene activos descargables"))
  }  
  
  print(nombres_empresas)
  print(divisa)
  
}


























