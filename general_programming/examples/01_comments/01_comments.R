# an update function
update <- function(x_old, parms, subs = 12) {
  
  # x_old is an atomic vector of characters of one and zeros
  
  x_new <- x_old
  
  climate <- parms$b 
  if(parms$sigma != 0)  climate <- climate * abs(rnorm(1, 1, parms$sigma))
  
  for(s in 1:subs) {
    
    # define update procedures depending on parms 
    
    # model specific part:
    # 1 - setting time-step parameters
    rho_one <- sum(x_old$cells == "1")/(x_old$dim[1]*x_old$dim[2]) # get initial vegetation cover
    q_one_one <- neighbors(x_old, "1")/4  # count local density of occupied fields for each cell
    
    # 2 - drawing random numbers
    rnum <- runif(x_old$dim[1]*x_old$dim[2]) # one random number between 0 and 1 for each cell
    
    # 3 - setting transition probabilities
    growth <- with(parms, (r * (climate + (1-climate)*f*q_one_one) * rho_one^(1 + alpha) * ( 1 - (rho_one / (K * (1-c*q_one_one) ))) / (1 - rho_one))  *1/subs)  # recolonisation rates of all cells 
    
    growth[growth < 0] <- 0
    
    death <- with(parms,       (m + ( (a+ v*q_one_one) * (1-p*q_one_one) * L * rho_one^(1+q) )/( 1 + (a+ v*q_one_one) * (1-p*q_one_one) * h * rho_one^(1+q) )) *1/subs)   # set probability of death for each cell
    
    death[death < 0] <- 0
    
    # check for sum of probabilities to be inferior 1 and superior 0
    if(any(c(growth, death) > 1 )) warning(paste("a set probability is exceeding 1 in time step", i, "! decrease delta!!!")) 
    #if(any(c(growth, death) < 0)) warning(paste("a set probability falls below 0 in time step", i, "! balance parameters!!!")) 
    
    # 4 - apply transition probabilities  
    
    x_new$cells[which(x_old$cells == "0" & rnum <= growth)] <- "1"
    x_new$cells[which(x_old$cells == "1" & rnum <= death)] <- "0"
    
    # 5- store x_new as next x_old
    
    x_old <- x_new
    
  }
  
  return(x_new)
  
}

