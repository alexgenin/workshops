# 
# 
# This file contains examples describing advanced uses of functions.
#   It comes with the files for the R workshop.
# 

# 
# A function that has arguments referencing each other
coordinate_system <- function(x = r * cos(theta), 
                              y = r * sin(theta),
                              r = sqrt(x^2 + y^2), 
                              theta = atan(y/x)) { 
  
  # We return things as a list
  new_list <- list(xy = list(x = x, y = y), 
                   rtheta = list(r = r, theta = theta))
  
  return(new_list)
}

# 
# A function that uses a function as argument (a functional)
apply_10_times <- function(f, values) { 
  
  # Init
  i <- 0
  result <- values
  
  while ( i < 10 ) { 
    result <- f(result)
    i <- i+1
  }
  
  return(result)
}


# 
# A function that returns a function
make_applier <- function(f, x) { 
  
  new_function <- function(...) { 
    i <- 0
    while ( i < x ) { 
      result <- f(...)
    }
    return(result)
  }
  
  new_function
  
}

