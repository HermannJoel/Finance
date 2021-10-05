import numpy as np
import scipy.stats as ss
from call_put_bs import call_put_bs

def implied_vola(VM,s,X,T,r,q,omega,init):
    '''Gives the implied volatility of a call (omega = 1) or put (omega = -1)
        option with market price VM, strike X and maturity T.
        The price of the underlying is s; r and q are cc risk free and cc 
        dividend yield, respectively.
    '''
    
    tol = 10**-10; sigma0 = 0.9*init; sigma1 = init;
    while abs(sigma0-sigma1)>tol:
        sigma0 = sigma1
        V = call_put_bs(s,X,T,sigma0,r,q,omega) # Black-Scholes price ...
        Vp = vega(s,X,T,sigma0,r,q) # ... and the Vega
        sigma1 = sigma0 - (V-VM)/Vp
    
    return sigma1

def vega(s,X,T,sigma,r,q):
    # Vega of the option
    d1 = (np.log(s/X)+(r-q+sigma**2/2)*T)/(sigma*np.sqrt(T))
    Vp = np.exp(-q*T)*s*np.sqrt(T)*ss.norm.pdf(d1)
    return Vp


