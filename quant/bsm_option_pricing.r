install.packages("fOptions")
library(fOptions)
#call
GBSOption(TypeFlag = "c", S = 900, X =950, Time = 1/4, r = 0.02, sigma = 0.22, b = 0.02)
#put
GBSOption(TypeFlag = "p", S = 900, X =950, Time = 1/4, r = 0.02, sigma= 0.22, b = 0.02)@price

#Greeks
sapply(c('delta', 'gamma', 'vega', 'theta', 'rho'), function(greek) 
    GBSGreeks(Selection = greek, TypeFlag = "c", S = 900, X = 950, Time = 1/4, r = 0.02, b = 0.02, sigma = 0.22))
       
deltas <- sapply(c(1/4, 1/20, 1/50), function(t) 
    sapply(500:1500, function(S) 
        GBSGreeks(Selection = 'delta', TypeFlag = "c", 
                  S = S, X = 950, Time = t, r = 0.02, b = 0.02, sigma = 0.22)))
           
straddles <- sapply(c('c', 'p'), function(type) 
    sapply(500:1500, function(S) 
        GBSGreeks(Selection = 'delta', TypeFlag = type, S = S, 
                  X = 950, Time = 1/4, r = 0.02, b = 0.02, sigma = 0.22)))

#implied vol
volatilites <- sapply(seq_along(goog$Strike), function(i) 
    GBSVolatility(price = goog$Ask.Price[i], TypeFlag = "c", 
                  S = 866.2, X = goog$Strike[i], Time = 88/360, r = 0.02, b = 0.02))