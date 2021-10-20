# -*- coding: utf-8 -*-
"""
Created on Tue Jun  1 12:08:09 2021

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

close_spy[["close",'mv_avg','bandasup','bandainf']].plot(linewidth=2,figsize=(30, 10))
plt.scatter(close_spy.index,[close_spy['precio_vender']], c='g',marker= '^', label='Vender',s=75)
plt.scatter(close_spy.index,[close_spy['precio_comprar']], c='r',marker= 'v', label='Comprar',s=75)
plt.scatter(close_spy.index,[close_spy['senal_comprar_vender']],facecolors='none',edgecolors='b', label='Posicion',s=220)
plt.ylabel('Precio')
plt.xlabel('Fecha')

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


