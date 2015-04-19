press <- function(lm.fit){ 
    infl.out <- lm.influence(lm.fit)
    hii <- infl.out$hat
    resids <- lm.fit$residuals
    eii <- resids/(1-hii)
    press <- sum(eii^2)
    # display and return the results
    press
    return(press)
} 