#simulate trees using the program diversitree

library(ape)
library(phydynR)
library(glue)
library(diversitree)

#' Theoretical frequency of the wild type assuming mutation & selection balance
#' with a variant that has lower transmission fitness
pancestral_mutsel_balance1 <- function( mu, gamma, alpha, omega, tol = 1e-3 ){
  proot <- function( p ){#
    p*(gamma*(1-p)+mu*(1-p)-mu*alpha*p) - (omega*(1-p))*(gamma*p + mu*alpha*p - mu*(1-p))
  }
  uniroot( proot, c(0,1), tol = tol )$root
}

dtR0 <- function( dtpars )
{
  pa = pancestral_mutsel_balance1( dtpars[5],
                                   dtpars[3] ,
                                   dtpars[6]/dtpars[5],
                                   dtpars[1]/dtpars[2] )
  pa * dtpars[2] / dtpars[4] + (1-pa)*dtpars[1]/dtpars[3]
}

params <- readRDS("diversitree_sim/diversitree_params.RDS")

#change molecular evolution rate to 0.0016
params[5,] <- 0.0016

#change q10 in order to have alpha the same as in the simulations with
#TiPS and Colaescent.jl
params[6,] <- c(0.0017601470, 0.0154352822, 0.0005768699, 0.0047038594)

#get the value of alpha
params[6,]/params[5,]

#ge the value of omega
params[1,]/params[2,]

#get the values of R0 and pa
params

#get R0
apply(params, 2, dtR0)

#get pa (proportion of ancestral)
for(i in 1:ncol(params)){

  print(pancestral_mutsel_balance1( params[5,i],
                              params[3,i] ,
                              params[6,i]/params[5,i],
                              params[1,i]/params[2,i]))

}



saveRDS(params, "diversitree_params2.RDS")

