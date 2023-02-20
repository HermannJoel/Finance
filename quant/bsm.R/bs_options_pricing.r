#bs function 
#credit basic R for finance
BlackScholes <- function(TypeFlag=c("c", "p"), S, X, Time, r, b, sigma)
{
    # Check Type Flag:
    TypeFlag = TypeFlag[1]
    # Compute d1 and d2:
    d1 = ( log(S/X) + (b+sigma*sigma/2)*Time ) / (sigma*sqrt(Time))
    d2 = d1 - sigma*sqrt(Time)
    # Compute Option Price:
    if (TypeFlag == "c")
        price = S*exp((b-r)*Time)*pnorm(d1) - X*exp(-r*Time)*pnorm(d2)
    else if (TypeFlag == "p")
        price = X*exp(-r*Time)*pnorm(-d2) - S*exp((b-r)*Time)*pnorm(-d1)
    # Save Parameters:
    param <- list(TypeFlag=TypeFlag, S=S, X=X, Time=Time, r=r, b=b, sigma=sigma)
    ans <- list(parameters=param, price=price, option = "Black Scholes")
    class(ans) <- c("option", "list")
    # Return Value:
    ans
}

print.option <- function(x, ...)
{
    # Parameters:
    cat("\nOption:\n ")
    cat(x$option, "\n\n")
    # Parameters:
    cat("Parameters:\n")
    Parameter = x$parameters
    Names = names(Parameter)
    Parameter = cbind(as.character(Parameter))
    rownames(Parameter) = Names
    colnames(Parameter) = "Value:"
    print(Parameter, quote = FALSE)
    # Price:
    cat("\nOption Price:\n")
    cat(x$price, "\n")
    # Return Value:
    invisible()
}

#implementation
S = 100 - 2 * exp(-0.1 * 0.25) - 2 * exp(-0.1 * 0.5)
r = 0.1
BlackScholes("c", S = S, X = 90, Time = 0.75, r = r, b = r, sigma = 0.25)

r = 0.1
q = 0.05
BlackScholes("p", S = 100, X = 95, Time = 0.5, r = r, b = r - q, sigma = 0.2)

#futures
FuturesPrice = 19
b = 0
BlackScholes("c", S = FuturesPrice, X = 19, Time = 0.75, r = 0.1, b = b, sigma = 0.28)
#currencies
r = 0.06
rf = 0.08
BlackScholes("c", S = 1.56, X = 1.6, Time = 0.5, r = r, b = r - rf, sigma = 0.12)