#////////////////////////////////////////
#
#  ~~~ Implémentation de Devictor + Robert
#  
#////////////////////////////////////////

# Acquisition des paramètres pour les distributions beta
shapes_I <- get_shapes(mean_I, sd_I) 
shapes_p <- get_shapes(mean_p, sd_p)

r0 <- 		rnorm(n=n_sp, mean=mean_r0, sd=sd_r0)
Hopt <- 	runif(n=n_sp, min=0, max=1)
I <- 		rbeta(n_sp, shape1=shapes_I[1], shape2=shapes_I[2] )
p <- 		rbeta(n_sp, shape1=shapes_p[1], shape2=shapes_p[2] )
H <- 		runif(n=size, min=0, max=1)
#K  <- 		rpois(n=size, lambda=mean_K) # lambda = espérance
K <- 		rep(5000, size)
D <- 		rnorm(size, mean=mean_D, sd=sd_D)

# Liste des paramètres ne changeant pas au cours du temps
params <- 	list(size=size,n_sp=n_sp, r0=r0,Hopt=Hopt,I=I,p=p,K=K) 

# Matrice représentant la métacommunauté, size*n_sp
# Lignes = # de patch
# Colonnes = # de l'espèce
metacomm <- init_metacomm(size,n_sp,params)

# Déclaration des variables
# Celles-ci n'interviennent pas sur le résultat 
# mais sont des variables temporaires pour le calcul

# taux d'occupation de l'env. par les espèces
tx_patch <- rep(0,n_sp) 
# Matrice 1/0 pour gérer le cas ou non d'une immigration
imm <- 		matrix(rep(0,size*n_sp), ncol=n_sp) 
# Replacement rates réells
r <- 		matrix(rep(0,size*n_sp), ncol=n_sp)
# Paramètre donné pour le calcul de la population à l'itération 
# suivante dans un patch
metacomm_param <- matrix(rep(0,size*n_sp), ncol=n_sp) 

# Output
total <- 	rep(0,iterations) # Total des individus
total[1] <- sum(metacomm)

W_mean <- 	rep(0,times=size) # W moyen pour un patch
alpha <- 	rep(0,times=size) # Proportion des espèces dans chaque patch
W <- 		rep(0,iterations) # W moyen, moyenne des patchs
	