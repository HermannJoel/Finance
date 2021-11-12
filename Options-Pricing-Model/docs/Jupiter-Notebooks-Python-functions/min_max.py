import numpy as np
from numpy.random import randn

def min_max(s0,sigma,r,q,T,J,w1,w2,n):
    '''Finds the value v of a financial product which pays off
    
        g = w_1*max(S_{t_0},...,S_{t_J}) - w_2*min(S_{t_0},...,S_{t_J})
        
        at maturity T = t_J. w_1 >= w_2 are positive weights which don't need 
        to sum up to one. The S_{t_i} are daily closing prices of the underlying
        at the J+1 observation dates t_0,t_1,...,t_J. We assume that the time 
        period t_{j+1}-t_j between two observation dates is the same for all j. 
        The underlying is modelled as a geometric Brownian motion 
 
            dS_t = (r-q)*S_t*dt + sigma*S_tdW_t, S_0 = s0 
            
        of which n paths are generated (at t_i) to aproximate v.
    '''
    v = np.zeros(n); dt = T/J; tj = np.arange(dt,T+dt,dt); 
    tj = np.reshape(np.asarray(tj),[J,1])
    
    for j in range(0,n):
        Z = randn(1,J); X = np.cumsum(Z,axis=1);
        s = s0*np.exp((r-q-0.5*sigma**2)*tj+sigma*np.sqrt(dt)*X.T)
        s = np.vstack((np.asarray(s0),s)) # the stock prices at tj, including s0
        v[j] = w1*np.max(s)-w2*np.min(s) # the payoff
        
    v = np.exp(-r*T)*np.mean(v)
    
    return v

