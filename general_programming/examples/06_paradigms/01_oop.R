# 
# 
# This file contains an example on OOP. 
# 

a <- c(26, 30, 15, 67, 12, 43)

print(a)

# Now with some class
class(a) <- "telephone_number"

print.telephone_number <- function(tel) { 
  paste(c('Phone number:', a), collapse = ' ')
}

print(a)

