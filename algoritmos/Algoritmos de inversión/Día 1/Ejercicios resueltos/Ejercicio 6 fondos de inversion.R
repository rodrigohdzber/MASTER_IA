# Consigue la rentabilidad los fondos de inversión en el año actual
# Recomendación: quefondos  https://www.quefondos.com/es/fondos/ranking/anual/index.html?cardCount=1&cardSize=100
# Tiempo objetivo: 45 minutos
# Objetivo extra: Programa la gestión de errores. Si da error al descargar, reinténtalo varias veces.

library(rvest)

obtener_fondos<-function(){
  
  # Consultamos cuantos fondos están listados en la web.
  url<-paste("https://www.quefondos.com/es/fondos/ranking/anual/index.html?cardCount=1&cardSize=100", sep = "")
  datos_web<-read_html(url, encoding = "UTF-8")
  num_fondos<- datos_web %>%
    html_nodes("#report") %>% 
    html_text
  frase<-unlist(strsplit(num_fondos, " "))
  num_fondos<-as.numeric(frase[length(frase)-1])
  
  # Redondeamos el número para que sea divisible entre 100.
  num_fondos<-(round(num_fondos/100,0))*100
  
  # Creamos una barra de progreso para conocer el avance del proceso.
  barra_progreso <- winProgressBar(title= "Barra de progreso", min = 0, max = num_fondos/100, width=300) # width es el nº de pixeles de la barra.
  
  # Descargamos los fondos de inversión.
  for (pagina in 1:(num_fondos/100)){
    
    setWinProgressBar(barra_progreso, pagina, title=paste(round(pagina/(num_fondos/100)*100,0), "% realizado"))
    
    url<-paste("https://www.quefondos.com/es/fondos/ranking/anual/index.html?cardCount=",pagina,"&cardSize=100", sep="") 
    
    intentos<-0
    descarga<-"Sin datos"
    while(descarga=="Sin datos"){
      
      tryCatch(    
        {
          datos_web<-read_html(url, encoding = "UTF-8")
          descarga<-"OK"
        },
        
        error=function(cond) {
          
          intentos<-intentos+1
          
          if(intentos>3){
            print("Tras varios intentos no nos hemos podido descargar los fondos. Dejamos de intentarlo.")
            return()
          }
          
          if(descarga=="Sin datos"){
            print("Error en la descarga de los fondos, reintentamos")
            Sys.sleep(30)
          }
          
        },
        finally={}
      ) 
    }
    
    fondos<- datos_web %>%
      html_nodes(".fondo") %>% 
      html_text
    fondos <- as.data.frame(fondos) 
    
    if(pagina == 1){
      fondos_acumulados<-fondos
    }else{
      fondos_acumulados<-rbind(fondos_acumulados,fondos)
    }
    
    categoria<- datos_web %>%
      html_nodes(".np70") %>% 
      html_text
    categoria <- as.data.frame(categoria) 
    if(pagina == 1){
      categorias_acumuladas<-categoria
    }else{
      categorias_acumuladas<-rbind(categorias_acumuladas,categoria)
    }
    
    rentabilidad<- datos_web %>%
      html_nodes(".current") %>% 
      html_text
    rentabilidad <- as.data.frame(rentabilidad) 
    tamaño<-dim(fondos)[1]
    rentabilidad<-as.data.frame(rentabilidad[(dim(rentabilidad)[1]-tamaño+1):dim(rentabilidad)[1],1])
    
    if(pagina == 1){
      rentabilidades_acumuladas<-rentabilidad
    }else{
      rentabilidades_acumuladas<-rbind(rentabilidades_acumuladas,rentabilidad)
    }
    
    print(paste(pagina*100," fondos descargados de ",num_fondos))
  }
  
  close(barra_progreso)
  
  # Construimos la tabla de fondos de inversión.
  tabla_fondos<-cbind(fondos_acumulados,categorias_acumuladas,rentabilidades_acumuladas)
  colnames(tabla_fondos)<-c("Nombre fondo","Categoría","Rentabilidad año en curso")
  
  return(tabla_fondos)
}

tabla_fondos<-obtener_fondos()



















