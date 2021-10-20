# -*- coding: utf-8 -*-
"""
Created on Sat Jun 19 13:00:29 2021

@author: Rodrigo
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import requests
import math
import multiprocessing as mp
import itertools
import sys
import seaborn as sns
from datetime import datetime 
import matplotlib.dates as mdates
from mpl_finance import candlestick_ohlc
import utils
from utils import plot_candle

#1.1Utilizando la API de IEX vista en clase descarga los precios de cierre del ETF
#del ındice SP500 con ticker SPY. Guarda los datos en un csv para no tener que realizar
#la descarga cada vez (recuerda que tienes un limite).

close_spy = pd.read_csv("/MASTER IA/Python/Teoria/python-mia-master/module_6/SPY2.csv")
close_spy.set_index("date", inplace = True)

#1.2 Calcula las bandas de Bollinger. Recuerda que las bandas Bollinger se calculan
#como MA − Kσ y MA + Kσ, donde MA es la media movil de 20 muestras sobre el
#precio, σ es la desviacion tpica movil de 20 muestras sobre el precio y K = 2.

periodos = 30
sigma =2

close_spy['mv_avg']=close_spy.close.rolling(periodos).mean()
close_spy['std_ret']=close_spy.close.rolling(periodos).std()
close_spy['bandasup']=close_spy.mv_avg+sigma*close_spy.std_ret
close_spy['bandainf']=close_spy.mv_avg-sigma*close_spy.std_ret

#1.3 Realiza una figura del apartado anterior.

close_spy[["close",'mv_avg','bandasup','bandainf']].plot(linewidth=0.5)
plt.ylabel('Precio')
plt.xlabel('Fecha')

#1.4  Usando las bandas Bollinger, calcula cu´ando comprar y vender el activo. 
#Compramos cuando el precio del activo sale y vuelve a entrar en la banda inferior, y
#vendemos cuando toca la banda superior. Por simplificaci´on, solo operamos a largo y
#si estamos comprados no podemos volver a comprar otra vez.

close_spy["comprar"] = 0
close_spy["vender"] = 0

for row in range(len(close_spy.close)-1):
    #primera condicion = compruebas la primera celda
    #segunda condicion = compruebas la siguiente a la anterior que debe ser mayor ya que asi nos lo dicen
    
    if(close_spy['close'][row]<close_spy['bandainf'][row]) and (close_spy['close'][row+1]   #la siguiente
                                                                   >close_spy['bandainf'][row+1]):
        close_spy['comprar'][row+2]=1  #da la señal de compra si se cumple la segunda condicion es decir ha comprado la primera y la sgunda fila
    elif(close_spy['bandasup'][row] <= close_spy['close'].iloc[row]) and (close_spy['close'][row-1]
                                                                     <close_spy['bandasup'][row-1]):
        close_spy['vender'].iloc[row+1]=-1   #row+1


close_spy["precio_comprar"] = close_spy["close"] * close_spy["comprar"]
close_spy["precio_vender"] = abs(close_spy["close"] * close_spy["vender"])

close_spy["posicion"] = close_spy["comprar"] + close_spy["vender"]
close_spy["posicion"][close_spy["posicion"] == 0] = None  #de la columna posicion coge las filas que sean igual a 0 en posicion
close_spy["posicion"].fillna(method =  "ffill", inplace = True)
close_spy["posicion"][close_spy["posicion"]== -1] = 0
close_spy["posicion"][close_spy["posicion"].isnull()] = 0

close_spy["senal_comprar_vender"] = 0

for i in range(len(close_spy["close"])-1):
    if close_spy["posicion"][i] != close_spy["posicion"][i+1]:
        close_spy["senal_comprar_vender"][i+1] = 1

#1.5 Realiza una figura del apartado anterior. Intenta obtener algo parecido a la
#Figura 1.


#primera forma
close_spy["retornos_x"] = close_spy["posicion"]
for i in range(len(close_spy["close"])-1):
    close_spy["retornos_x"][i+1] = close_spy["posicion"][i] #bajamos todas una posicion

close_spy["retornos_log"] = np.log(close_spy["mv_avg"].diff())
close_spy["retornos"] = close_spy["retornos_log"] * close_spy["retornos_x"]
close_spy["retornos"] = close_spy["retornos"].cumsum()
close_spy["retornos_euros"] = math.exp(1)**close_spy["retornos"]-1

num_operaciones = 0

for i in range(len(close_spy["close"])-1):
    if (close_spy["posicion"][i] != 1) and (close_spy["posicion"][i+1] ==1):
        num_operaciones += 1

retorno_medio = close_spy["retornos_euros"][len(close_spy["retornos_euros"])-1]/num_operaciones

rent = retorno_medio * 100

#si quito este codigo hasta el plot saldria como la figura 1
#con estas lineas lo que hago es que solo ponga un punto cuando compro y
#hasta que no vendo no pone otro
#de la otra forma nos diria todos los puntos donde se podria comprar y donde se podria vender
close_spy["precio_comprar2"] = float(0)
close_spy["precio_vender2"] = float(0)

for i in range(len(close_spy["close"])-1):
    if (close_spy["posicion"][i] == close_spy["senal_comprar_vender"][i]) and (close_spy["posicion"][i] != 0):
        close_spy["precio_comprar2"][i] = float(close_spy["precio_comprar"][i])
    elif (close_spy["posicion"][i] != close_spy["senal_comprar_vender"][i]) and (close_spy["posicion"][i] != 1):
        close_spy["precio_vender2"][i] = float(close_spy["precio_vender"][i])
    
    

        

close_spy[["close",'mv_avg','bandasup','bandainf']].plot(linewidth=2,figsize=(30, 10))
plt.scatter(close_spy.index,[close_spy['precio_vender2']], c='g',marker= '^', label='Vender',s=75)
plt.scatter(close_spy.index,[close_spy['precio_comprar2']], c='r',marker= 'v', label='Comprar',s=75)
plt.scatter(close_spy.index,[close_spy['senal_comprar_vender']],facecolors='none',edgecolors='b', label='Posicion',s=220)
plt.ylabel('Precio')
plt.xlabel('Fecha')


list_compra = []
list_venta = []

for i in range(len(close_spy["close"])-1):
    if close_spy["precio_comprar2"][i] != 0:
        list_compra.append(close_spy["precio_comprar2"][i])

for i in range(len(close_spy["close"])-1):
    if close_spy["precio_vender2"][i] != 0:
        list_venta.append(close_spy["precio_vender2"][i])




seri1 = pd.Series(list_compra)
seri2 = pd.Series(list_venta)

ffff = ((seri2/seri1)-1)*100
aaaa = seri2 - seri1

b = aaaa / seri1

z = b.cumsum()

rentttt = round((z[7]/8) * 100, 1)



#EJERCICIO 2 BOOTSTRAPING

#Carga los datos de cierre del Ibex con dividendos que est´an en el fichero:
#ibex div data close.csv.

ibex_div = pd.read_csv("/MASTER IA/Python/practica_python/ibex_div_data_close.csv")

#Calcula los retornos logar´ıtmicos de la serie

returns_log = np.log(ibex_div["close"]).diff()

#Realiza una figura de la distribuci´on de los retornos (puedes usar la funci´on
#distplot de seaborn). ¿Qu´e observas?.

fig_ruturns_log = sns.displot(returns_log)

#M´etodo I: Simulaci´on de Montecarlo.

#Calcula la media y la desviaci´on t´ıpica de los retornos obtenidos en el apartado2

media = np.mean(returns_log)
std = np.std(returns_log)

#Genera un dataframe con 1000 columnas, que ser´an cada una de las simulaciones
#y donde las filas se extender´an un plazo de 5 a˜nos, desde el ´ultimo de d´ıa de la
#serie original.

fecha_final1 = ibex_div.iloc[len(ibex_div)-1,0]         #seleccionamos la ultima fecha del df ibex_div
fecha_final1 = pd.to_datetime(fecha_final1)             #convertirmos la fecha formato datetime
fecha_final_n = fecha_final1 + pd.DateOffset(years = 5) #creamos una nueva fecha el ultimo dia del ibex_div pasados 5 años
fechas = pd.date_range(fecha_final1,fecha_final_n,freq="B") #hacemos un range de las dos anteriores y nos da todos los dias de los 5 años

# Los datos del dataframe se generan de forma aleatoria siguiendo una
#distribuci´on normal con la media y la desviaci´on t´ıpica obtenida anteriormente

metodo1 =pd.DataFrame(np.random.normal(loc = media, scale = std, size = (len(fechas),1000)),
                                       columns = np.arange(1,1001), index = fechas)




#M´etodo II: Bootstrapping simple.
#En el Bootstrapping simple en vez de generar los retornos de forma aleatoria,
#realizamos un muestreo aleatorio de los retornos de la serie original.

#def myboot(mysample):
#    resample = np.random.choice(mysample, size = len(mysample))
#    return resample

#re = myboot(returns_log)

#metodo2 = pd.DataFrame(returns_log.sample(n = len(fechas), replace = True),columns = np.arange(1,1001), index = fechas)


returns_log = returns_log[returns_log.notnull()]
#reemplazando
metodo3=pd.DataFrame(data=1,columns=np.arange(0,100),index=fechas)

for a in np.arange(0,1000):
    metodo3.loc[ : ,a]=np.array(returns_log.sample(n=len(fechas),replace=True))
    #REVISAR Q AUNQUE ESTA BIEN PUEDE SER CON ILOC


#asi pone siempre el mismo.. buscar otra forma
metodo2 = pd.DataFrame(np.random.choice(returns_log, size = (len(fechas),1000)),columns = np.arange(1,1001), index = fechas)


#cada m´etodo, usando los retornos de una ´unica simulaci´on (una sola columna), realiza una figura de la distribuci´on de los retornos (puedes usar la funci´on
#distplot de seaborn). Compara la figura para cada m´etodo. ¿Qu´e concusiones puedes
#extraer?.

fig2_6,(ax1,ax2,ax3)=plt.subplots(1,3)
fig_metodo1=sns.distplot(metodo1.iloc[:,0], ax=ax1,axlabel='metodo_1')   #pintamos metodo 1 una sola columna
fig_metodo2=sns.distplot(metodo3.iloc[:,0], ax=ax2,axlabel='metodo2')   #pintamos metodo 2 una sola columna

fig_metodo1=sns.distplot(metodo1.iloc[:,0], ax=ax3,axlabel='comparacion')   #comparamos los dos metodos
fig_metodo2=sns.distplot(metodo3.iloc[:,0], ax=ax3,axlabel='comparacion')


#Para cada m´etodo utilizando los retornos simulados, calcula la evoluci´on temporal de invertir 
#una unidad monetaria en cada una de las simulaciones generadas.

m1acumulado = metodo1.cumsum()
m2acumulado = metodo3.cumsum()

retornos1 = math.exp(1)**m1acumulado
retornos2 = math.exp(1)**m2acumulado

#Para cada m´etodo utilizando el resultado del apartado anterior, obt´en una figura
#donde muestres 100 simulaciones. Deber´as obtener una figura parecida a la Figura 2.

fig_ej1_8, (ex1, ex2) = plt.subplots(2, 1, sharex=True, figsize=(6, 6))  #2 filas 1 columnau
ex1.plot(retornos1.iloc[:,0:100] ,linewidth=0.5,c='b')
ex1.set_xlabel('metodo1')
ex2.plot(retornos2.iloc[:,0:100],linewidth=0.5,c='r')
ex2.set_xlabel('metodo2')

#Para cada m´etodo usando el dataframe de simulaciones, nos quedamos para cada
#d´ıa con los retornos que ocupan los percentiles 0.05, 0.5 y 0.95.

percentil1 = retornos1.quantile(q = [0.05,0.5,0.95], axis = 1, numeric_only = True,
                                interpolation = "linear")
percentil2 = retornos2.quantile(q = [0.05,0.5,0.95], axis=1, numeric_only=True,
                                interpolation='linear')

percentil1 = percentil1.transpose()
percentil2 = percentil2.transpose()

#Para cada m´etodo utilizando el c´alculo anterior, realiza una figura parecida a la
#Figura 3.


fig,(ax1,ax2)=plt.subplots(2,1,sharex=True, figsize=(6, 6))
ax1.plot(percentil1)
ax1.fill_between(percentil1.index,percentil1.iloc[:,0],percentil1.iloc[:,2])
ax1.set_xlabel('metodo1')
ax1.legend(['0.05','0.5','0.95'])

ax2.plot(percentil2)
ax2.fill_between(percentil2.index,percentil2.iloc[:,0],percentil2.iloc[:,2])
ax2.set_xlabel('metodo2')
ax2.legend(['0.05','0.5','0.95'])

#Realiza una comparacion de los dos metodos y extrae unas conclusiones, ¿cual
#crees que es el mejor?.

#Tras comparar los resultados, se observa que el metodo 2 se adecua mas a la realidad
#debido que a la hora de hacer el bootstrapping se seleccionan los retornos mas realistas,
#en cambio en el metodo 1 conseguimos una homogeneidad de los retornos normales y a la hora
#de hacer el bootstrapping los retornos son incongruentes. 



#EJERCICIO 3 BARS

url = "http://api.kibot.com/?action=history&symbol=IVE&interval=tickbidask&bp=1&user=guest"
datosk = pd.read_csv(url,header = None)
#head = datosk.head(10)

#datosk.to_csv("datos_ticks.csv")

datos_ticks = pd.read_csv("datos_ticks.csv")
s = datosk.shape  #9M,7
head = datosk.head(10)
#si lo hago con el csv que yo he guardado me salen cosas raras
def clean_data(datosk):  #documentarla
    datos = datosk.iloc[0:1000000,[0,1,2,3,4,5]]
    datos[6] = " "                                                  #Creamos una columna como separador entre fecha y tiempo
    datos[7] = datos[0]+datos[6]+datos[1]                           #Unimos la columna fecha y tiempo separadas por la columna 6
    datos["Fecha"] = pd.to_datetime(datos[7])                       #Convertimos la fecha a formato
    #datos["Fecha"] = datos["Fecha"] + timedelta(hours = 6)          #Pasamos a hora española
    datos.index = datos["Fecha"]                                    #ponemos la fecha como indice
    datos = datos.iloc[:,[2,3,4,5]]                                     #seleccionamos las columnas de precio y volumen
    datos.columns = ["Precio","Oferta","Demanda","Volumen"]
    return datos

datos_clean = clean_data(datosk)  #no lo paso a csv porque si no no se ve como lo he limpiado...
k = datos_clean.tail(10)

#2. Calcula las velas (open, high, low, close, vol) horarias, diarias, mensuales y
#anuales.

datos_horas = datos_clean["Precio"].resample("H").ohlc().dropna()
datos_dias = datos_clean["Precio"].resample("D").ohlc().dropna()
datos_meses = datos_clean["Precio"].resample("M").ohlc().dropna()
datos_anuales = datos_clean["Precio"].resample("Y").ohlc().dropna()


datos_horas["Volumen"] = datos_clean["Volumen"].resample("H").sum()
datos_dias["Volumen"] = datos_clean["Volumen"].resample("D").sum()
datos_meses["Volumen"] = datos_clean["Volumen"].resample("M").sum()
datos_anuales["Volumen"] = datos_clean["Volumen"].resample("Y").sum()

datos_horas = datos_horas[datos_horas["Volumen"] != 0]
datos_dias = datos_dias[datos_dias["Volumen"] != 0]

#3. Haz un grafico de velas para cada apartado anterior. Puedes encontrar la funci´on
#plot candle para hacerlo en el m´odulo utils.py adjunto a esta pr´actica (es la misma
#que usamos en clase).

grafico_horas = plot_candle(datos_horas, tick_formater = "%Y-%m-%d-%h")
grafico_dias = plot_candle(datos_dias, tick_formater = "%Y-%m-%d")
grafico_meses = plot_candle(datos_meses, tick_formater = "%Y-%m")
grafico_anual = plot_candle(datos_anuales, tick_formater = "%Y")

#4. Haz una funci´on que muestre el precio cada vez que se produce una determinada
#cantidad de negociaciones, pasada por par´ametro. Estas son las denominadas ticks
#bars. Prueba la funci´on con 10000 negociaciones. Realiza una figura.

def datos_neg(num_negociaciones):
    aux1 = []
    ind = []
    for pos in np.arange(9999,len(datos_clean),num_negociaciones):
        aux1.append(datos_clean["Precio"][pos])
        ind.append(datos_clean.index[pos])
    datos_nego = pd.DataFrame(aux1, index = ind,columns = ["Precio"])
    return datos_nego

datos_negociados = datos_neg(10000)      



fig_ticks, nx1 = plt.subplots(figsize = (10,7))
nx1.plot(datos_clean["Precio"])
nx1.grid()
nx1.set_xticks(datos_negociados.index)


#datos_clean["cantidades"] = datos_clean["Volumen"]*datos_clean["Precio"]
#datos_clean["sum_cantidades"] = datos_clean["cantidades"].cumsum()
#nuevo_head  = datos_clean.head(100)
nuevo_tail = datos_clean.tail(10)
#a = (nuevo_tail.iloc[9,5]) / 100000


#5. Haz una funcion que muestre el precio cada vez que se negocia una determinada cantidad de d´olares, pasada por par´ametro. Estas son las denominadas dollar
#bars.Prueba la funcion con 100.000 dolares. Realiza una figura.

def datos_cantidad(cantidad):
    list_precios = []
    total = 0
    fechas = []
    for pos, fecha in enumerate(datos_clean.index):
        total += datos_clean.iloc[pos]["Volumen"]
        if total >= cantidad:
            total = total - cantidad
            list_precios.append(datos_clean.iloc[pos]["Precio"])
            fechas.append(datos_clean.index[pos])
    list_precios = pd.DataFrame(list_precios)
    fechas = pd.DataFrame(fechas)
    list_precios = list_precios.set_index(fechas[0])
    return(list_precios)


dollar_bar = datos_cantidad(100000)

#279
fig6, ax1 = plt.subplots(1,1)
ax1.plot(datos_clean["Precio"])
ax1.grid()
ax1.set_xticks(dollar_bar.index)

#6. Calcula los retornos diarios en el caso de las velas diarias, los retornos cada
#10.000 negociaciones y los retornos cada 1.000.000$ negociados.

datos_dias['retornos'] = np.log(datos_dias['close']).diff()
datos_dias['retornosum'] = datos_dias['retornos'].cumsum()

datos_negociados_1 = datos_neg(10000)
datos_negociados_1['retornos'] = np.log(datos_negociados_1['Precio']).diff()
datos_negociados_1["retornosum"] = datos_negociados_1["retornos"].cumsum()

datos_cantidades_1 = datos_cantidad(1000000)
datos_cantidades_1['retornos'] = np.log(datos_cantidades_1.iloc[:,0]).diff()
datos_cantidades_1['retornosum'] = datos_cantidades_1['retornos'].cumsum()

#7. Haz una figura que compare la distribuci´on de los tres retornos. ¿Que diferencias
#observas?.

fig_retornos,((zx1, zx2), (zx3, zx4)) = plt.subplots(2,2, figsize = (20,10))
fig_ret_dias = sns.distplot(datos_dias.iloc[:,5], ax = zx1)
zx1.set_xlabel("retornos diarios")
zx1.set_ylabel("frecuencia")
fig_ret_ticks = sns.distplot(datos_negociados_1.iloc[:,1], ax = zx2)
zx2.set_xlabel("retornos ticks")
zx2.set_ylabel("frecuencia")
fig_ret_dolares = sns.distplot(datos_cantidades_1.iloc[:,1], ax = zx3)
zx3.set_xlabel("retornos dollars")
zx3.set_ylabel("frecuencia")
fig_ret_dias = sns.distplot(datos_dias.iloc[:,5], ax = zx4)
fig_ret_ticks = sns.distplot(datos_negociados_1.iloc[:,1], ax = zx4)
fig_ret_dolares = sns.distplot(datos_cantidades_1.iloc[:,1], ax = zx4)
zx4.set_xlabel("retornos comparacion")
zx4.set_ylabel("frecuencia")

#cuantas mas muestras cogemos para los retornos,
#más se asemeja a una distribucion normal.
#cambiar rutas de los csvs a la carpeta de la practica para que pueda leerlo Fernando con más facilidad.

