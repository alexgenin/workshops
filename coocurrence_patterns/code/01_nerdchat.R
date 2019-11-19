#
#
# This file comes with the Nerdchat on coocurrences (01/11/2016)
#
# 

# We load the required packages
library(picante)
library(ggplot2)
library(lattice)

# Here are three interesting matrices
checker1 <- matrix(c(0, 0, 0, 0, 0, 1, 1, 1, 1, 
                     0, 0, 1, 1, 1, 1, 1, 1, 1, 
                     0, 1, 0, 1, 0, 1, 1, 1, 1, 
                     0, 0, 1, 1, 1, 1, 1, 1, 1, 
                     0, 0, 1, 1, 1, 1, 1, 1, 1, 
                     1, 1, 1, 0, 1, 1, 1, 1, 0, 
                     0, 0, 1, 1, 1, 1, 1, 1, 1, 
                     1, 0, 1, 0, 0, 1, 0, 1, 0, 
                     1, 1, 1, 1, 1, 1, 1, 0, 0, 
                     0, 0, 1, 1, 1, 1, 1, 1, 1, 
                     1, 1, 1, 0, 1, 1, 1, 1, 0, 
                     1, 0, 1, 0, 0, 1, 0, 1, 0, 
                     1, 1, 1, 1, 1, 0, 1, 0, 1, 
                     1, 1, 1, 1, 1, 0, 0, 1, 0), 
                    byrow = TRUE, ncol = 9)

checker2 <- t(matrix(c(1,1,1,1,1,0,0,0,0,0,
                       0,0,0,0,0,1,1,1,1,1,
                       1,0,1,0,1,0,1,0,1,0,
                       0,1,0,1,0,1,0,1,0,1,
                       0,0,1,1,1,1,1,0,0,0,
                       1,1,0,0,0,0,0,1,1,1,
                       1,0,0,0,0,0,0,0,0,0,
                       0,1,0,0,0,0,0,0,0,0,
                       0,0,1,0,0,0,0,0,0,0,
                       0,0,0,1,0,0,0,0,0,0,
                       0,0,0,0,1,0,0,0,0,0,
                       0,0,0,0,0,1,0,0,0,0,
                       0,0,0,0,0,0,1,0,0,0), 
byrow = TRUE, ncol = 10))

finches <- t(matrix(c(1,1,1,1,1,1,1,1,1,1,1,0,0,1,0,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                      1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,
                      1,1,1,1,1,0,1,1,0,0,0,0,0,1,0,1,1,
                      1,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
                      1,1,1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,
                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
                      1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
                      1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
                      1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                      1,1,1,1,0,1,0,0,1,0,0,0,0,0,0,0,0,
                      0,0,0,0,1,0,1,1,0,1,1,1,1,1,1,1,1,
                      1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0), 
                      byrow = TRUE, ncol = 17))
colnames(finches) <- c('G. magnirostris', 'G. fortis',   'G. fuliginosa', 
                       'G. difficilis',   'G. scandens', 'G. conirostris', 
                       'C. psittacula', 'C. pauper', 'C. parvulus', 
                       'C. pallida', 'C. heliobates', 'C. olivacea', 
                       'C. fusca', 'P. crassirostris')


# A function that takes a matrix and returns the checkerboard score
checker_score <- function(mat) { 
  Cij <- 0
  n <- ncol(mat) # species N
  
  for (i in seq.int(n-1)) { 
    for (j in seq(i+1, n)) { 
      
      # Compute Sij for species pair
      ri <- sum(mat[ ,i])
      rj <- sum(mat[ ,j])
      Sij <- sum(mat[ ,i] == 1 & mat[ ,j] == 1)
      
      # Compute and add Cij to total
      Cij <- Cij + (ri - Sij) * (rj - Sij)
    }
  }
  
  Cs <- Cij * 2 / (n * (n-1))
  
  return(Cs)
}

# We use the function levelplot (from lattice) to have a quick look at our matrix
levelplot(checker2)

# Compute the checkerboard score
obs_score <- checker_score(checker2)

# Generate a null distribution
null_distr <- replicate(10000, 
                        checker_score( randomizeMatrix(checker2, "trialswap") ))

# Plot the output
ggplot( data.frame(null = null_distr) ) + 
  geom_density(aes(x = null)) + 
  geom_vline(xintercept = quantile(null_distr, .95), color = 'blue') + 
  geom_vline(xintercept = obs_score, color = "red") 


# Do the same with checker2

