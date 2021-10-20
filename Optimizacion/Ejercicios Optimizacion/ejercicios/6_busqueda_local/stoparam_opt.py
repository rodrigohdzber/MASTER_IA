#!/usr/bin/env python
# coding: utf-8

import numpy as np 
import pandas as pd
import matplotlib.pyplot as plt
import itertools


class StochasticOsc:
    @staticmethod
    def min_max_scale(s):
        """
        Calcula para un vector s su normalización en el rango [0, 1]
        """
        return (s[-1] - np.min(s)) / (np.max(s) - np.min(s))

    @staticmethod
    def stochastic_osc(s, win, lag):
        """
        Calcula el valor del oscilador estocástico
         1. calcula la posición relativa del precio en una ventana temporal
         2. Suaviza la señal anterior haciendo una media móvil, con tamaño inferior al de la ventana
        """
        so_raw = s.rolling(window=win).apply(StochasticOsc.min_max_scale, raw=True)
        so = so_raw.rolling(window=lag).mean()
        return so  

    @staticmethod
    def stochastic_osc_signal(s, win=14, lag=3, obought=0.8, osold=0.2):
        """
        Calcula el estado de inversión "dentro" (1) o "fuera" (0) siguiendo
        la señal del oscilador estocástico. Esto es:
        - Se compra cuando se cruza de forma ascendente el umbral de sobreventa
        - Se vende cuando se cruza de forma descendente el umbral de sobrecompra
        """
        states = pd.Series(np.zeros(s.shape[0]), index=s.index)
        so = StochasticOsc.stochastic_osc(s, win, lag)
        if so.iloc[0] > osold:
            states.iloc[:1] = 1
            curr = 1
        else:
            curr = 0

        for i in range(s.shape[0] - 2):
            if so.iloc[i] < osold and so.iloc[i+1] >= osold:
                curr = 1
            elif so.iloc[i] > obought and so.iloc[i+1] <= obought: 
                curr = 0
            states.iloc[i+2] = curr

        return states

    @staticmethod
    def state_returns(price, states):
        """ Calcula para una serie y unos estados de 
        estar dentro fuera, cual es el retorno total
        correspondiente.
        Se debe tener el mismo Index de pandas 
        """
        log_ret = np.log(price).diff()
        log_ret.iloc[0] = 0

        in_rets = log_ret * states
        total_log_ret = in_rets.sum()
        simple_ret = np.exp(total_log_ret) - 1
        return simple_ret

    @staticmethod
    def backtest_so_returns(vseries, win=14, lag=3, obought=0.8, osold=0.2):
        """
        A partir de una serie de precios, calcula el rendimiento del oscilador estocástico 
        con los parámetros correspondientes
        """
        f_states = StochasticOsc.stochastic_osc_signal(
            vseries, 
            win=win, 
            lag=lag,
            obought=obought,
            osold=osold)
        so_return = StochasticOsc.state_returns(vseries, f_states)
        return so_return

