# 
#
# This file comes with the Nerdchat on coocurrences (01/11/2016)
#
# 

# Function that counts the number of coocurrences
ncooc <- function(mat) { 
  Sij <- matrix(NA, ncol(mat), ncol(mat))
  n <- ncol(mat) # species N
  for (i in seq.int(n-1)) { 
    for (j in seq(i+1, n)) { 
      Sij[i,j] <- sum(mat[ ,i] == 1 & mat[ ,j] == 1)
    }
  }
  
  return(Sij)
}

# Observed number of coocurrences between species
cooc_obs <- ncooc(checker2)

# Generate a null distribution
null_distr <- replicate(10000, 
                        ncooc( randomizeMatrix(checker2, "trialswap") ))

# Test to see whether a pair is unusual (occurs less than 5% in the null 
#   distribution)
nsp <- nrow(cooc_obs)
is_unusual <- matrix(NA, ncol = ncol(cooc_obs), nrow = nrow(cooc_obs))
pval <- is_unusual
for (spi in seq.int(nsp-1)) { 
  for (spj in seq(spi+1, nsp)) { 
    is_unusual[spi,spj] <- mean(null_distr[spi, spj, ] == cooc_obs[spi,spj]) < .05
  }
}

# Are there unusual pairs ? 

#  NB: Suggestion when working with the finches dataset: transfer the names 
#    so that the plot is clearer
# colnames(is_unusual) <- rownames(is_unusual) <- colnames(finches)

levelplot(is_unusual)



