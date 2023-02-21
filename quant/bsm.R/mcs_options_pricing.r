
MonteCarloOption <- function(delta.t, pathLength, mcSteps, mcLoops, path.gen, payoff.calc){
    # Arguments:
    # delta.t - The length of the time interval, by default one day
    # pathLength - Number of Time Intervals which add up to the path
    # mcSteps - The number of Monte Carlo Steps performed in one loop
    # mcLoops - The number of Monte Carlo Loops
    # path.gen - the generator for the MC paths
    # payoff.calc - the payoff claculator function
    # Monte Carlo Simulation:
    delta.t <<- delta.t
    cat("\nMonte Carlo Simulation Path:\n\n")
    iteration = rep(0, length = mcLoops)
    cat("Loop:\t", "No\t")
    for ( i in 1:mcLoops ) {
        if ( i > 1) init = FALSE
        # 1 Generate Random Innovations:
            eps = matrix(rnorm(mcSteps*pathLength), nrow=mcSteps)
        # 2 Calculate for each path the option price:
            path = t(path.gen(eps))
            payoff = NULL
            for (j in 1:dim(path)[1])
                payoff = c(payoff, payoff.calc(path[, j]))
        iteration[i] = mean(payoff)
    # 3 Trace the Simualtion:
        cat("\nLoop:\t", i, "\t:", iteration[i], sum(iteration)/i )
    }
    cat("\n")
    # Return Value:
    iteration
}


wienerPath <- function(eps){
    # Generate the Paths:
    path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
    # Return Value:
    path
}

plainVanillaPayoff <- function(path)
{
    # Compute the Call/Put Payoff Value:
    ST = S*exp(sum(path))
    if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
    else if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
    # Return Value:
    payoff
}

