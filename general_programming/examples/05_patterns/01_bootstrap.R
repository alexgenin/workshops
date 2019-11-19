# 
# 
# This file contains a bootstrap example based on functions. It comes with 
#   the slides used in the R workshop (26/01/2016).
# 

# Take a formula and data and return the square root of coefficients
fit_coeffs <- function(data, formula) { 
  
  # We fit the model 
  model_fit <- lm(formula, data)
  
  return( coefficients(model_fit) )
}

# Do the same with bootstrapping: this represents one job that is repeated
#   to compute the output distribution.
sample_and_fit <- function(sample_number, fit_function, data, formula) { 
  
  # Sample data: we create a new data frame with random rows taken with 
  #   replacement in the original one.
  data.boot <- data[sample.int(nrow(data), size = nrow(data), replace = TRUE), ]
  
  # Return results
  return( c(sample_number, fit_function(data.boot, formula)) )
  
}

# This function applies a function many times and return a list of results.
gen_boot_distribution <- function(replicates, data, formula) { 
  
  result_list <- lapply(as.list(seq.int(replicates)), 
                        sample_and_fit, fit_coeffs, data, formula)
  
  return( result_list )
}

# Function that combines results
combine_results <- function(result_list) { 
  # Using do.call() is equivalent to writing the following: 
  #   rbind(result_list[[1]], result_list[[2]], ..., result_list[[n]])
  result_matrix <- do.call(rbind, result_list) 
  colnames(result_matrix) <- c('boot_number', 'alpha', 'beta')
  
  return( result_matrix )
}



# 
# Execution starts here
# 
results_raw <- gen_boot_distribution(replicates = 1000, 
                                     data = iris, 
                                     formula = Petal.Width ~ Petal.Length)

bootstrap_results <- combine_results(results_raw)

summary(bootstrap_results)

# Plot results
library(ggplot2)
ggplot() + 
  geom_point(aes(Petal.Length, Petal.Width), data = iris) + 
  geom_abline(aes(intercept = alpha, slope = beta), 
              data = as.data.frame(bootstrap_results), 
              alpha = .01)



