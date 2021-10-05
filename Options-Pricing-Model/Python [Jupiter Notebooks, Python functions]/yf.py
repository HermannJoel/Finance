import numpy as np

def yf(t1,t2):
    '''
    Measures the year fraction between dates t1 and t2>t1 according to the
    30/360 European day count convention.
    
    t1 is the tuple (dd,mm,yyyy), t2 is a list of tuples 
    [(dd,mm,yyyy),...,(dd,mm,yyyy)]. Tau = yf(t1,t2) is an array, where Tau[j] 
    is the corresponding year fraction between t1 and t2[j].
    
    Example
    t1 = (28,6,2017)
    t2 = [(5,10,2017),(5,1,2018),(5,4,2018)]
    
    Tau = yf(t1,t2)
    '''
    
    n = len(t2); Tau = np.zeros(n)
    
    for j in range(0,n):
        Tau[j] = (t2[j][2]-t1[2]+(t2[j][1]-t1[1])/12+
           (np.minimum(t2[j][0],30)-np.minimum(t1[0],30))/360)
    
    
    return Tau
