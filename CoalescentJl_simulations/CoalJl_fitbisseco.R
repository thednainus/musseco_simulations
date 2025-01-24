# Fit bisseco model using the trees simulated with TiPS
library(ape)
library(musseco)
library(glue)

#ncpu is the number of cpu to use in the fitbisseco function
ncpu <- 1

#read tree
#tr <- read.tree("CoalescentJl_simulations/trees/params_1/rep1/param_1_rep_1.nwk")
tr <- read.tree(list.files(pattern=".nwk"))


isvariant <- grepl( tr$tip.label, pattern = '.V' )


# gamma value used in our epidemiological model
#Tg = 1/gamma; gamma = 1
gamma <- 1/10.2
# mu (molecular clock) value used in our epidemiological model
mu <- 0.0016

#using augment_likelihood = TRUE
fb_au <- fitbisseco( tr,
                  isvariant,
                  Tg = 1/gamma,
                  mu = mu,
                  Net = NULL,
                  theta0 = log(c(2,.75,1/2)),
                  optim_parms = list(),
                  mlesky_parms = list(tau = NULL,
                                      tau_lower = .1,
                                      tau_upper = 1e7,
                                      ncpu = ncpu, model = 1 ) )

#using augment_likelihood = FALSE
fb <- fitbisseco( tr,
                  isvariant,
                  Tg = 1/gamma,
                  mu = mu,
                  Net = NULL,
                  augment_likelihood = FALSE,
                  theta0 = log(c(2,.75,1/2)),
                  optim_parms = list(),
                  mlesky_parms = list(tau = NULL,
                                      tau_lower = .1,
                                      tau_upper = 1e7,
                                      ncpu = ncpu, model = 1 ) )

save(fb_au, fb, file = "julia_fitbisseco.rda")
