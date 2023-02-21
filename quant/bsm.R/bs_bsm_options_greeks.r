#call & put valuation
#bs
BlackScholes <- function(TypeFlag = c("c", "p"), S, X, Time, r, b, sigma) {
    TypeFlag = TypeFlag[1]
    d1 = (log(S/X) + (b + sigma * sigma/2) * Time)/(sigma * sqrt(Time))
    d2 = d1 - sigma * sqrt(Time)
    if (TypeFlag == "c")
        price = S * exp((b - r) * Time) * pnorm(d1) - X * exp(-r * Time) * pnorm(d2)
    else if (TypeFlag == "p")
        price = X * exp(-r * Time) * pnorm(-d2) - S * exp((b - r) * Time) * pnorm(-d1)
    param <- list(TypeFlag = TypeFlag, S = S, X = X, Time = Time,
    r = r, b = b, sigma = sigma)
    ans <- list(parameters = param, price = price, option = "Black Scholes")
    class(ans) <- c("option", "list")
    ans
}

#delta
delta <- function(TypeFlag, S, X, Time, r, b, sigma) {
    d1 = (log(S/X) + (b + sigma * sigma/2) * Time)/(sigma * sqrt(Time))
    if (TypeFlag == "c")
        delta = exp((b - r) * Time) * pnorm(d1)
    else if (TypeFlag == "p")
        delta = exp((b - r) * Time) * (pnorm(d1) - 1)
    delta
}

#theta
theta <- function(TypeFlag, S, X, Time, r, b, sigma) {
    d1 = (log(S/X) + (b + sigma * sigma/2) * Time)/(sigma * sqrt(Time))
    d2 = d1 - sigma * sqrt(Time)
    NDF <- function(x) exp(-x * x/2)/sqrt(8 * atan(1))
    Theta1 = -(S * exp((b - r) * Time) * NDF(d1) * sigma)/(2 * sqrt(Time))
    if (TypeFlag == "c")
        theta = Theta1 - (b - r) * S * exp((b - r) * Time) * pnorm(+d1) - r * X * exp(-r * Time) * pnorm(+d2)
    else if (TypeFlag == "p")
        theta = Theta1 + (b - r) * S * exp((b - r) * Time) * pnorm(-d1) + r * X * exp(-r * Time) * pnorm(-d2)
    theta
}

#vega
vega <- function(TypeFlag, S, X, Time, r, b, sigma) {
    NDF <- function(x) exp(-x * x/2)/sqrt(8 * atan(1))
    d1 = (log(S/X) + (b + sigma * sigma/2) * Time)/(sigma * sqrt(Time))
    vega = S * exp((b - r) * Time) * NDF(d1) * sqrt(Time)
    vega
}

#rho
rho <- function(TypeFlag, S, X, Time, r, b, sigma) {
    d1 = (log(S/X) + (b + sigma * sigma/2) * Time)/(sigma * sqrt(Time))
    d2 = d1 - sigma * sqrt(Time)
    CallPut = BlackScholes(TypeFlag, S, X, Time, r, b, sigma)$price
    if (TypeFlag == "c")
        if (b != 0)
            rho = Time * X * exp(-r * Time) * pnorm(d2)
        else rho = -Time * CallPut
    else if (TypeFlag == "p")
        if (b != 0)
            rho = -Time * X * exp(-r * Time) * pnorm(-d2)
        else rho = -Time * CallPut
    rho
}

#bsm
BlackScholes <- function(TypeFlag, S, X, Time, r, b, sigma)
{
    # Compute d1 and d2:
    d1 = ( log(S/X) + (b+sigma*sigma/2)*Time ) / (sigma*sqrt(Time))
    d2 = d1 - sigma*sqrt(Time)
    # Compute Option Price:
    if (TypeFlag == "c")
    price = S*exp((b-r)*Time)*pnorm(d1) - X*exp(-r*Time)*pnorm(d2)
    else if (TypeFlag == "p")
    price = X*exp(-r*Time)*pnorm(-d2) - S*exp((b-r)*Time)*pnorm(-d1)
    # Return Value:
    price
}

CBND <- function (a, b, rho){
    # Cumulative Bivariate Normal distribution:
    if (abs(rho) == 1) rho = rho - (1e-12) * sign(rho)
    X = c(0.24840615, 0.39233107, 0.21141819, 0.03324666, 0.00082485334)
    y = c(0.10024215, 0.48281397, 1.0609498, 1.7797294, 2.6697604)
    a1 = a/sqrt(2 * (1 - rho^2))
    b1 = b/sqrt(2 * (1 - rho^2))
    if (a <= 0 && b <= 0 && rho <= 0) {
        Sum1 = 0
        for (I in 1:5) {
            for (j in 1:5) {
                Sum1 = Sum1 + X[I] * X[j] * exp(a1 * (2 * y[I] - 
                                                      a1) + b1 * (2 * y[j] - b1) + 2 * rho * (y[I] -
                                                                                              a1) * (y[j] - b1)) 
            } }
        result = sqrt(1 - rho^2)/pi * Sum1
        return(result) }
    if (a <= 0 && b >= 0 && rho >= 0) {
        result = pnorm(a) - CBND(a, -b, -rho)
    return(result) }
    if (a >= 0 && b <= 0 && rho >= 0) {
        result = pnorm(b) - CBND(-a, b, -rho)
    return(result) }
    if (a >= 0 && b >= 0 && rho <= 0) {
        result = pnorm(a) + pnorm(b) - 1 + CBND(-a, -b, rho)
    return(result) }
    if (a * b * rho >= 0) {
        rho1 = (rho * a - b) * sign(a)/sqrt(a^2 - 2 * rho * a * b + b^2)
        rho2 = (rho * b - a) * sign(b)/sqrt(a^2 - 2 * rho * a * b + b^2)
        delta = (1 - sign(a) * sign(b))/4
        result = CBND(a, 0, rho1) + CBND(b, 0, rho2) - delta
        return(result) }
    invisible()
}

RollGeskeWhaley <- function(S, X, time1, Time2, r, D, sigma)
{
    #Tolerance Settings:
    big = 1.0e+8
    eps = 1.0e-5
    # Compute Option Price:
    Sx = S - D * exp(-r * time1)
    if(D <= X * (1 - exp(-r*(Time2-time1)))) {
        result = BlackScholes("c", Sx, X, Time2, r, b=r, sigma)
        cat("\nWarning: Not optimal to exercise\n")
        return(result) }
    ci = BlackScholes("c", S, X, Time2-time1, r, b=r, sigma)
    HighS = S
    while ( ci-HighS-D+X > 0 && HighS < big ) {
        HighS = HighS * 2
        ci = BlackScholes("c", HighS, X, Time2-time1, r, b=r, sigma) }
    if(HighS > big) {
        result = BlackScholes("c", Sx, X, Time2, r, b=r, sigma)
        stop()}
    LowS = 0
    I = HighS * 0.5
    ci = BlackScholes("c", I, X, Time2-time1, r, b=r, sigma)
    # Search algorithm to find the critical stock price I
    while ( abs(ci-I-D+X) > eps && HighS - LowS > eps ) {
        if(ci-I-D+X < 0 ) HighS = I else LowS = I
        I = (HighS + LowS) / 2
        ci = BlackScholes("c", I, X, Time2-time1, r, b=r, sigma) }
    a1 = (log(Sx/X) + (r+sigma^2/2)*Time2) / (sigma*sqrt(Time2))
    a2 = a1 - sigma*sqrt(Time2)
    b1 = (log(Sx/I) + (r+sigma^2/2)*time1) / (sigma*sqrt(time1))
    b2 = b1 - sigma*sqrt(time1)
    result = Sx*pnorm(b1) + Sx*CBND(a1,-b1,-sqrt(time1/Time2)) - 
    X*exp(-r*Time2)*CBND(a2,-b2,-sqrt(time1/Time2)) - 
    (X-D)*exp(-r*time1)*pnorm(b2)
    
    # Return Value:
    result
}