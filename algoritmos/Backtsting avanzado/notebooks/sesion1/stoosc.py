import numpy as np
import pandas as pd


class Sto:
    @staticmethod
    def min_max_scale(s):
        """ Calcula la normalización de rango """
        return (s[-1] - np.min(s)) / (np.max(s) - np.min(s))

    @staticmethod
    def stochastic_osc(s, win):
        """ Calcula la serie primaria del estocástico"""
        lag = int(np.round(win/5))
        so_raw = s.rolling(window=win).apply(Sto.min_max_scale, raw=True)
        so = so_raw.rolling(window=lag).mean()
        return so  
      
    @staticmethod
    def stochastic_osc_states(s, win=14, obought=0.8, osold=0.2):
        """En función de las señales de trading calcula una serie de "estados"
        para indicar que se está 
        1: dentro o invertido
        0: fuera o desinvertido 
        Las señales de trading se determinan a partir 
        de los umbrales de sobre-(compra o venta) """

        states = pd.Series(np.zeros(s.shape[0]), index=s.index)
        so = Sto.stochastic_osc(s, win)

        # Condicion inicial 
        if so.iloc[0] > osold and so.iloc[0] < obought:
            states.iloc[0] = 1
            curr = 1
        else:
            curr = 0

        for i in range(s.shape[0] - 2):
            # corta umbral de sobreventa al alza
            if so.iloc[i] < osold and so.iloc[i+1] >= osold:
                curr = 1
            # corta umbral de sobrecompra a la baja
            elif so.iloc[i] > obought and so.iloc[i+1] <= obought: 
                curr = 0

            # 
            states.iloc[i+2] = curr

        return states
    
    @staticmethod
    def state_returns(price, states):
        """ Calcula para una serie y unos estados de 
        estar dentro fuera, cual es el retorno total
        correspondiente.
        Debe tener el mismo pd.Index 
        """
        ret = price.pct_change()
        ret.iloc[0] = 0

        in_rets = ret * states.shift(1)
        simple_rets = in_rets + 1 
        total_ret = simple_rets.prod() - 1
        return total_ret
    
    @staticmethod
    def ann_returns(price, states):
        tot_ret = Sto.state_returns(price, states)

        init_date = states.index[0]
        end_date = states.index[-1]
        fyears = (end_date - init_date) / pd.Timedelta(days=365, hours=6)

        anual_ret = np.power(tot_ret + 1, 1/fyears) - 1  
        return anual_ret

    @staticmethod
    def backtest_so_returns(vseries, win=20, obought=0.8, osold=0.2):
        """Devuelve la rentabilidad anualizada que se consigue al aplicar
        la regla de trading del oscilador estocastico a la serie de precios
        vseries """
        f_states = Sto.stochastic_osc_states(
            vseries, 
            win=win, 
            obought=obought,
            osold=osold)
        so_return = Sto.ann_returns(vseries, f_states)
        return so_return
