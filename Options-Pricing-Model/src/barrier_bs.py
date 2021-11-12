import numpy as np
import scipy.stats as ss
 
def Vc(s,X,Z,r,q,sigma,T):
    d1 = (np.log(s/Z)+(r-q+sigma**2/2)*T)/(sigma*np.sqrt(T))
    d2 = d1 - sigma*np.sqrt(T)
    return np.exp(-q*T)*s*ss.norm.cdf(d1)-X*np.exp(-r*T)*ss.norm.cdf(d2)

def barrier_bs(s,B,X,T,sigma,r,q):
    '''[Vpdo,Vpdi,Vcdo,Vcdi] = barrier_bs(s,B,X,T,sigma,r,q) gives the 
        Black-Scholes price V of a down-and-out put (pdo), down-and-in put (pdi), 
        down-and-out call (cdo) and down-and-in (cdi) call option with barrier B,
        strike X and maturity T. The actual price of the underlying is s, its
        vola is sigma. r and q are cc risk free and the cc dividend yield,
        respectively.'''
    # aux vars
    Y = (s/B)**2; F = (s/B)**(-1-2*(r-q)/(sigma**2))
    # down-and-out put
    Vpdo = Vc(s,X,X,r,q,sigma,T)-Vc(s,X,B,r,q,sigma,T) - \
        F*(Vc(s,X*Y,X*Y,r,q,sigma,T)-Vc(s,X*Y,B*Y,r,q,sigma,T))
    # down-and-in put, put price via put-call-parity 
    Vpdi = Vc(s,X,X,r,q,sigma,T)-s*np.exp(-q*T)+X*np.exp(-r*T)-Vpdo
    # down-and-out call
    Vcdo = Vc(s,X,X,r,q,sigma,T)-F*Vc(s,X*Y,X*Y,r,q,sigma,T)
    # down-and-in call
    Vcdi = Vc(s,X,X,r,q,sigma,T)-Vcdo
    return Vpdo, Vpdi, Vcdo, Vcdi

