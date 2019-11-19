# 
# 
# 
# 

# 
# The old way of doing it: all species together
# 
results_raw <- gen_boot_distribution(replicates = 1000, 
                                     data = iris, 
                                     formula = Petal.Width ~ Petal.Length)

bootstrap_results <- combine_results(results_raw)

summary(bootstrap_results)



# The same thing done by species
# 
library(plyr)

results_by_species <- 
  ddply(iris, ~ Species, 
        function(split_df) { 
          one_result <- gen_boot_distribution(replicates = 1000, 
                                data = split_df, 
                                formula = Petal.Width ~ Petal.Length)
          return( combine_results(one_result) )
      })


ggplot() + 
  geom_point(aes(y = Petal.Width, 
                 x = Petal.Length, 
                 color = Species), 
             data = iris) + 
  geom_abline(aes(intercept = alpha, 
                  slope = beta, 
                  color = Species), 
              data = results_by_species, 
              alpha = 0.05)
