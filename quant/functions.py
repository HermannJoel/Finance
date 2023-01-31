import pandas as pd
import yfinance as yf
import numpy as np
import quandl

# Read Data for DAX from the Web
def read_data(sticker, start, end):
    ''' Reads historical data from Yahoo! Finance, calculates log returns, simple returns using  
        close-close, hight-low, open-close.
    '''
    data=yf.download(sticker, start, end, progress=True, interval='1mo')
    #valid periods=1d,5d,1mo,3mo,6mo,1y,2y,5y,10y,ytd,max
    #intervals=1m,2m,5m,15m,30m,60m,90m,1h,1d,5d,1wk,1mo,3mo
    data=data.loc[:, ['Open', 'High', 'Low', 'Close', 'Adj Close']]
    data.rename(columns={'Adj Close':'Adj_close'}, inplace=True)
    #simple returns
    data['c-c_sple_rtn']=(data.Adj_close/data.Adj_close.shift(1))-1
    data['h-l_sple_rtn']=(data.High-data.Low)/data.Low
    data['o-c_sple_rtn']=(data.Close/data.Open)/data.Open
    #log returns
    data['c-c_log_rtn']=np.log(data.Adj_close/data.Adj_close.shift(1))
    data['h-l_log_rtn']=np.log(data.High/data.Low.shift(1))
    data['o-c_log_rtn']=np.log(data.Close/data.Open.shift(1))
    #
    df_all_dates=pd.DataFrame(index=pd.date_range(start,end))
    data=df_all_dates.join(data.loc[:,:], how='left').fillna(method='ffill').asfreq('M')
    #reads cpi data from quandl
    data_cpi=quandl.get(dataset='RATEINF/CPI_USA',
    start_date=start,
    end_date=end)
    data_cpi.rename(columns={'Value':'cpi'}, inplace=True)
    data_m=data.join(data_cpi, how='left')
    data_m['inflation_rate']=data_m.cpi.pct_change()
    #adjusted close-adjusted close simple real return
    data_m['c-c_sple_real_rtn']=(data_m['c-c_sple_rtn'] + 1) / (data_m['inflation_rate'] + 1) - 1
    #high-low simple real return
    data_m['h-l_sple_real_rtn']=(data_m['h-l_sple_rtn'] + 1) / (data_m['inflation_rate'] + 1) - 1
    #open-close simple real return
    data_m['o-c_sple_real_rtn']=(data_m['o-c_sple_rtn'] + 1) / (data_m['inflation_rate'] + 1) - 1
    data_m=data_m.dropna()
    
    return data_m