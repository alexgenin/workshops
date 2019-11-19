
# Print the error between an expected value and what is observed
print_error <- function(expected, observed) { 
  print0('Picked value ', observed, ' (error: ', observed - expected,')')
}

# Are the values the minima of a vector ? 
# e.g. for c(1, 2, 1), this function returns c(TRUE, FALSE, TRUE)
is_minimum <- function(vec) { 
  vec == min(vec)
}

# Get the closest value in a vector
closest_to <- function(val, X, quiet = FALSE) { 
  index <- which( is_minimum(abs(X-val)) )
  new_value <- X[min(index)]
  
  if ( !quiet ) { 
    print_error(val, new_value)
  }
  
  return(new_value)
}
