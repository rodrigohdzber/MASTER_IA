import pandas as pd
import matplotlib.pyplot as plt


class SMA:
    @staticmethod
    def movaverage_states(vseries, win):
        sma = vseries.rolling(win).mean()
        signal_states = (vseries > sma).astype(float)
        trading_states = signal_states.shift(1)
        trading_states.iloc[0] = 0
        return trading_states
    
    @staticmethod
    def plot_sma_states(vseries, win):
        sma = vseries.rolling(win).mean()
        states = SMA.movaverage_states(vseries, win)

        fig, ax = plt.subplots(figsize=(20,4))
        vseries.plot(ax=ax)
        sma.plot(ax=ax)

        for i, idx in enumerate(states.index[:-1]):
            if states.loc[idx] == 1:
                ax.axvspan(idx, states.index[i+1], facecolor='g', alpha=0.2)