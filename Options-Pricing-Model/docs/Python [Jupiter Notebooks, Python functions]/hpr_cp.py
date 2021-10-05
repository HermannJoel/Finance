import numpy as np
import scipy.integrate as integrate
from scipy.stats import norm
import matplotlib.pyplot as plt


def hpr_cp(St,vt,X1,X2,S0,T,k,c,z,mu,sigma):
    '''Gives the expected holding period return and the volatility of a
    capital protection product (uncapped or capped) with capital protection k, 
    partizipation z and (possible) cap c written on one underlying. The strike
    of the product is X1; the possible cap strike is X2 (the level of the 
    underlying at which the return is capped). It is assumed that the product
    is issued at time t = 0 (the price of underlying then is S0), that we 
    purchase it at time t >= 0 at price vt (in percent of its nominal, St is 
    the price of the underlying at t) and hold it until maturity.
    '''
    
    # the density of ST given St
    muS = (mu-1/2*sigma**2)*T; sigmaS = sigma*np.sqrt(T)
    fRc = lambda x: norm.pdf(x,muS,sigmaS) # density of log return of underlying
    fS = lambda x: fRc(np.log(x/St))/x # density of ST
    y = lambda x: (vt*x+vt-k)*S0/z+X1 # argument for density of cp return
    
    # the probability p1
    p1 = integrate.quad(lambda x: fS(x),0,X1)
    
    # the aux variables alpha, beta
    alpha = k/vt-1; beta = (c+1)/vt-1
    
    if c > 0:
        # expected return (of capped capital protection)
        p2 = integrate.quad(lambda x: fS(x),X2,np.inf)
        I = integrate.quad(lambda x:x*vt*S0/z*fS(y(x)),alpha,beta)
        E_ret = I[0]+alpha*p1[0]+beta*p2[0]
        
        # vola (of capped capital protection)
        I = integrate.quad(lambda x:x**2*vt*S0/z*fS(y(x)),alpha,beta)
        vola = np.sqrt(I[0]+alpha**2*p1[0]+beta**2*p2[0]-E_ret**2)
        
        # graphical output of pdfs
        x = np.arange(alpha,beta+(beta-alpha)/100,(beta-alpha)/100)
        lines1 = plt.plot(x,vt*S0/z*fS(y(x)),np.array([alpha,alpha]),\
                         np.array([0,p1[0]]))
        lines2 = plt.plot(np.array([beta,beta]),np.array([0,p2[0]]),label='product')
        lines3 = plt.plot(2*x,St*fS(St*(2*x+1)),label='underlying')
        
        plt.setp(lines1,'color',(0,0.4470,0.7410))
        plt.setp(lines2,'color',(0,0.4470,0.7410))
        plt.setp(lines3,'color',(0.85,0.325,0.098))
        plt.legend(), plt.xlabel('Return'), plt.ylabel('pdf')
        
    else:
        # expected return (of uncapped capital protection)
        I = integrate.quad(lambda x:x*vt*S0/z*fS(y(x)),alpha,np.inf)
        E_ret = I[0]+alpha*p1[0]
            
        # vola (of uncapped capital protection)
        I = integrate.quad(lambda x:x**2*vt*S0/z*fS(y(x)),alpha,np.inf)
        vola = np.sqrt(I[0]+alpha**2*p1[0]-E_ret**2)
        
        # graphical output of pdfs
        x = np.arange(alpha,1+(1-alpha)/100,(1-alpha)/100)
        lines1 = plt.plot(x,vt*S0/z*fS(y(x)))
        lines2 = plt.plot(np.array([alpha,alpha]),np.array([0,p1[0]]),label='product')
        lines3 = plt.plot(2*x,St*fS(St*(2*x+1)),label='underlying')
        
        plt.setp(lines1,'color',(0,0.4470,0.7410))
        plt.setp(lines2,'color',(0,0.4470,0.7410))
        plt.setp(lines3,'color',(0.85,0.325,0.098))
        plt.legend(), plt.xlabel('Return'), plt.ylabel('pdf')
        
    return E_ret, vola