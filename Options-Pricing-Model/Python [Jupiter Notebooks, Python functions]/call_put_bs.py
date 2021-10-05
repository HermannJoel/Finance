import numpy as np
import scipy.stats as ss

def call_put_bs(s,X,T,sigma,r,q,omega):
    '''Gives the Black-Scholes price V of a European call (omega = 1) or put 
        (omega = -1) option with strike X and maturity T. The price of the 
        underlying is s, its volatility is sigma; r and q are cc risk free 
        and cc dividend yield, respectively.
        '''
    # aux variables
    d1 = (np.log(s/X)+(r-q+sigma**2/2)*T)/(sigma*np.sqrt(T))
    d2 = d1-sigma*np.sqrt(T)
    V = omega*(np.exp(-q*T)*s*ss.norm.cdf(omega*d1) \
        -X*np.exp(-r*T)*ss.norm.cdf(omega*d2))
    return V

