import numpy as np
import pandas as pd
import pickle
import random
import datetime as dt
from time import process_time
def markowitz(retornos, nfondos, num_simulations=100):
    """
    Funcion que realiza la frontera eficiente de markowitz a partir de un dataframe retornos.
    Dicha funcion de markowitz lo que realiza es encontrar los pesos de la cartera que me proporciona la mejor cartera rentabilidad-riesgo basado en el ratio de sharpe.
    Esta funcion lo que realiza los siguientes pasos:
        - Pesos aleatorios asignados a cada instrumento pasado en la lista
        - Normalizacion de los pesos aleatorios para asegurar que suma 100%
        - Calculamos la rentabilidad de la cartera a partir de esos peso aleatorios normalizados
        - Calculamos el riesgo de la cartera usando la matriz de covarianza multiplicado por los vectores normalizados.
        - Veo la eficiencia de la cartera rentabilidad/riesgo
        - Realizo el numero de simulaciones correspondientes y elijo los mejores pesos que me dan la mejor rentabilidad/riesgo
    
    Parameters
    ----------
    Los parametros de entrada son:
        retornos: dataframe de rendimientos de los activos por fecha
        nfondos: numero de fondos que hay en el dataframe retornos para calcular markowitz
        num_sumulations: numero de simulaciones para realizar la frontera
    
    Returns
    ----------
    Los parametros de salida son:
        - best_portfolio: mejor combinacion de pesos en cuanto a rentabilidad/precio
    """
    # Calculamos la matriz de covarianzas
    cov_matrix = np.cov(retornos.T)
    
    # Realizamos un array de numeros aleatorio de 0 a 100 con n simulaciones y el numero de fondos pasado como parametros a nuestra funcion
    weights = np.random.randint(0,100,(num_simulations,nfondos))/100
    
    # Normalizamos
    weights = weights/(weights.sum(axis=1)[:,None])

    # Calculamos la rentabilidad de la cartera en función de los pesos
    temp_profit = np.inner(retornos, weights)
    portfolio_profit = temp_profit.T.sum(axis=1)

    # Calculamos el riesgo de la cartera (desviación), en función de los pesos.
    portfolio_risk = (np.dot(weights,cov_matrix)*(weights)).sum(axis=1)**.5

    # Calculamos la eficiencia de la cartera, en función de los pesos.
    portfolio_efficiency = portfolio_profit/portfolio_risk

    # Localizamos la cartera con mayor rentabilidad, menor riesgo y mayor eficiencia.
    best_portfolio = portfolio_efficiency.argmax()
    
    # Devolvemos
    return weights[best_portfolio]

def alpha_jensen(activos, datosMSCI,ventana):
    """
    Función que calcula el Alpha de Jensen para todos los activos y para todas las fechas con una ventana movil pasada como parámetro. 
    Dicha alpha de jensen se calcula de la siguiente forma:
    α=Rentabilidad_cartera-(Rentabildad_activo_libre_riesgo+ β (Rentabilidad_mercado- Rentabildad_activo_libre_riesgo))
    β= covarianza (Rentabilidad_cartera, Rentabilidad_mercado) / varianza (Rentabilidad_mercado)
    
    El alfa de Jensen es una medida de calidad sobre la gestión realizada (ya sea en un fondo, cartera de activos, o una única empresa). 
    Indica el exceso de rentabilidad obtenida para un nivel de riesgo determinado. 
    El Alfa explica la diferencia entre la rentabilidad esperada, es decir, la que corresponde al riesgo sistemático asumido, 
    y la realmente obtenida por el gestor. En función de que el gestor supere, iguale o esté por debajo 
    del rendimiento esperado tendrá un Alfa positivo, neutro o negativo.
    
    Parameters
    ----------
    activos: Dataframe con todas las cotizaciones de los activos sobre los cuales se va a realizar el alpha de jensen
    datosMSCI: Dataframe con los datos del índice sobre el cual se va calcular el alpha de jensen.
    ventana: ventana movil usada para el cálculo del alpha de jensen
    
    Returns
    -------
    Se devuelve un data frame del alpha de jensen para cada dia y cada activo.
    
    """
    # Extraemos las fechas
    rango_fechas = pd.to_datetime(activos.index, format='%d/%m/%Y')
    
    # Calculamos las rentabilidades de los activos
    rent_activos = np.log(activos.iloc[1:, :]/activos.iloc[:-1, :].values)
    
    # Calculamos las rentabilidades del indice
    rent_MSCI     = np.log(datosMSCI.iloc[1:, :]/datosMSCI.iloc[:-1, :].values)
    
    # Indexamos las fechas
    rent_activos.set_index(rango_fechas[1:], inplace = True) 
    rent_MSCI.set_index(rango_fechas[1:], inplace = True) 
    
    # Calculamos la varianza rolling con una ventana muestral de ventana datos y lo almacenamos en la variable vatianza_ind
    varianza_ind = rent_MSCI.rolling(ventana).var()
  
    # Creamos un dataframe vacio en el cual iremos añadiendo todas las alphas de jensen que calcularemos
    alpha_activos = pd.DataFrame(np.zeros((len(activos)-1, len(activos.columns))))
    
    # Ponemos nombres a las columnas
    alpha_activos.columns = activos.columns 
    
    # Ponemos nombres a las filas
    alpha_activos.set_index(rango_fechas[1:], inplace = True) 
    
    # Haremos un bucle for para cada activo, para así poder usar la función
    # rolling cov para cada activo y el índice y así calcular la covarianza de cada activo con el índice.
    # Posteriormente se calculará la beta.
    # Finalmente calculamos el alpha de Jensen.
    for activo in activos.columns:
        
        # Calculamos la covarianza entre el activo y el índice (rolling 30 dias)
        cov_act_ind = rent_activos[activo].rolling(ventana).cov(rent_MSCI)
        
        # Calculamos la beta en cada fecha, covarianza entre la varianza
        beta = cov_act_ind/varianza_ind

        # Calculamos el Alpha del activo. α=Rc-(Rf+β(Rm-Rf))
        alpha_activos.loc[:,activo] = rent_activos.loc[:,activo].values - (beta*rent_MSCI.values).values.reshape(-1)
  
    # Borramos los NA para que esteticamente el resultado sea mejor
    alpha_activos.dropna(inplace=True)
    
    # Devolvemos el dataframe y el tiempo que ha tardado la función
    return alpha_activos

def sharpeFunction(dataframe, ventana):
    """
    Función que calcula el ratio de sharpe para todos los activos y para todas las fechas con una ventana movil pasada como parámetro. 
    Dicha ratio de sharpe se calcula de la siguiente forma:
    ratioSharpe = Rentabilidad_cartera / volatilidad 
    
    El ratio de sharpe es una medida que representa la rentabilidad-riesgo
    
    Parameters
    ----------
    dataframe: Dataframe con todas las cotizaciones de los activos sobre los cuales se va a realizar el ratio de sharpe
    ventana: ventana movil usada para el cálculo del ratio de sharpe
    
    Returns
    -------
    Se devuelve un dataframe del ratio de sharpe para cada dia y cada activo.
    
    """
    # calculamos los rendimimientos
    rendimientos = np.log(dataframe).diff().dropna(axis=0,how='all')
    
    # las rentabilidades a cada fecha teniendo una ventana pasada por paramentro
    rent_movil   = rendimientos.rolling(ventana).sum()
    
    #calculamos la volatilidad rolling
    volatilidad  = rendimientos.rolling(ventana).std()
    
    # calculamos el ratio de sharpe a cada fecha
    sharpe = rent_movil / volatilidad
    
    # Eliminamos los na
    sharpe = sharpe.dropna(axis=0, how='all')
    sharpe = sharpe.dropna(axis=1, how='all')
    
    # rellenamos el resto de datos que tienen NA para tener una matriz consistente
    sharpe = sharpe.fillna(method = 'ffill')
    sharpe = sharpe.fillna(method = 'bfill')
    
    # devolvemos el dataframe de ratio de sharpe
    return sharpe