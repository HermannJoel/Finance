import numpy as np
from scipy.optimize import fsolve
from call_put_bs import call_put_bs

def find_beta(VM,St,N,S0,z,k,X,T,sigma,r0,q):
    '''
    If the risk free is of the form 
            r(t) = r0(1-beta*t)
    find_beta(VM,N,St,S0,z,k,X,T,sigma,r0,q) finds the parameter beta such that
    the Black-Scholes price of an uncapped capital protection product is equal
    to the market price VM (the underyling has value St). sigma, r0, q are the 
    usual Black-Scholes parameters, N,S0,z,K,X,T are the parameters of the 
    product.
    Example:
    VM = 1026.1; N = 1000; St = 8296.14; S0 = 8532.09; z = 1; k = 1.01; X = S0; 
    T = 2233/360; sigma = 0.1759; r0 = -0.005656; q = 0.055624;
    find_beta(VM,St,N,S0,z,k,X,T,sigma,r0,q)
    '''
    def f(x):
        r = lambda x: r0*(1-x*T/2)
        return np.exp(-r(x)*T)*k*N+z*N/S0*call_put_bs(St,X,T,sigma,r(x),q,1)-VM
        
    beta = fsolve(f,0.0)
    return beta[0]