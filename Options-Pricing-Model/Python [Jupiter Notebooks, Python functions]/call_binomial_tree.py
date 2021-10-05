from scipy.stats import binom
import numpy as np
import matplotlib.pyplot as plt

def call_binomial_tree(s,X,T,sigma,r,m):
    '''Gives the bimomial tree price v of a European call option with strike X 
        and maturity T. The price of the underlying is s; r is the cc risk free.
    '''
    
    # aux variables
    dt = T/m
    u = 1+sigma*np.sqrt(dt); d = 1-sigma*np.sqrt(dt)
    i = np.ceil((np.log(X/s)-m*np.log(d))/(np.log(u)-np.log(d)))
    p = (np.exp(r*dt)-d)/(u-d); phat = p*u*np.exp(-r*dt)
    V = s*(1-binom.cdf(i-1,m,phat))-X*np.exp(-r*T)*(1-binom.cdf(i-1,m,p))
    return V

