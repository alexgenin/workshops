# 
# 

get_boot_coeffs <- function(df, N = 1000) { 
  
  
  get_coeffs <- function(df) {
    model <- lm(Petal.Length ~ Sepal.Length, data = df)
    coeffs <- coefficients(model)

    # coeffs is a vector -> convert to data.frame
    data.frame(slope     = coeffs["Sepal.Length"],
               intercept = coeffs["(Intercept)"])
  }
  
  # Take nrows with replacement
  ddply(data.frame(id = seq.int(N)), ~ id, 
        function(n) { 
          df_random <- df[sample.int(nrow(df), replace = TRUE), ]
          get_coeffs(df_random)
        })
  
}
