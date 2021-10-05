import numpy as np
from scipy.optimize import fsolve
from scipy.special import psi


def mle_tlocation(R):
    '''Given the log-returns R (a vector of length n) of an asset, 
        x = mle_tlocation(R) with x = [mu,sigma,nu] gives the maximum 
        likelihood estimator of a t-distribution with density 
        
        f(x) = cv/(sqrt(nu*pi)*sigma)*[1+1/nu*((x-mu)/sigma)^2]^(-(nu+1)/2)
        
        where cv = gamma((nu+1)/2)/gamma(nu/2).
    '''
    
    def f(x):
        n = len(R); f = np.zeros(len(x)); m = x[0]; s = x[1]; v = x[2];
        y = (R-m)/s; z = 1+1/v*y**2; 
        f[0] = sum(y/z)
        f[1] = sum(y**2/z)-n*v/(v+1)
        f[2] = n*(psi((v+1)/2)-psi(v/2))-sum(np.log(z))
        return f
    
    x = fsolve(f,[0,0.001,10],xtol=1e-12)
    return x

