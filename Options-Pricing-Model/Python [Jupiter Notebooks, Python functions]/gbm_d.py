import numpy as np
from numpy.random import randn

def gbm_d(s0,mu,Sigma,Tau,T,dt):
    '''S,t = gbm_d(s0,mu,Sigma,Tau,T,dt) simulates one path of a d-dimensional 
       geometric Brownian motion 
       
       S = [S_0,S_t1,S_t2,...,S_T]
       
       at the time points specified in the list Tau = [t1,t2,...,T]. Each S_tj
       is a (row) array of length d (the GBM evaluated at tj).
       If Tau = [] is empty, the time points are evenly spaced, tj = j*dt, 
       with time step dt.
       
       s0 is a list of length d containing the initital values, mu is list of
       length d containing the drifts Sigma is the d-times-d covariance matrix.
       
       Example:
       s0 = [234,67]; mu = [0.037,0.027]
       Sigma = np.array([[0.18**2,0.18*0.15*0.68],[0.18*0.15*0.68,0.15**2]])
       S,t = gbm_d(s0,mu,Sigma,Tau,T,dt)
       plt.plot(S[:,0],S[:,1])
    '''
    
    if not Tau: # Tau is empty: we use evenly spaced time points in ]0,T]
        t = np.arange(dt,T+dt,dt);
    else: # Tau is not empty:
        t = Tau
    
    d = len(s0); n = len(t) # the dimensions
    
    # convert to arrays 
    s0 = np.reshape(np.asarray(s0),[1,d]) 
    mu = np.reshape(np.asarray(mu),[1,d])
    t = np.reshape(np.asarray(t),[n,1])
    t0 = np.vstack((0.0,t))
     
    L = np.linalg.cholesky(Sigma) # the Cholesky decomposition
    Z = randn(d,n); 
    sqrtDT = np.repeat(np.sqrt(np.diff(t0.T)),d,axis=0)

    X = np.cumsum(sqrtDT*Z,axis=1)
    A = np.repeat((mu-0.5*np.diag(Sigma)),n,axis=0)
    T = np.repeat(t,d,axis=1); S0 = np.repeat(s0,n,axis=0)
    S = S0*np.exp(A*T+np.matmul(L,X).T); S = np.vstack((s0,S));
    
    return S,t0

