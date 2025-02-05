invisible('
Application of bisseco model to bisse birth/death trees simulated with diversitree

')

#' Theoretical frequency of the wild type assuming mutation & selection balance
#' with a variant that has lower transmission fitness
pancestral_mutsel_balance1 <- function( mu, gamma, alpha, omega, tol = 1e-3 ){
  proot <- function( p ){#
    p*(gamma*(1-p)+mu*(1-p)-mu*alpha*p) - (omega*(1-p))*(gamma*p + mu*alpha*p - mu*(1-p))
  }
  uniroot( proot, c(0,1), tol = tol )$root
}

nCPU = 1 #number of cpus to request at the Imperial College London HPC

library( ape )
library( phydynR )
library( glue )
library( mlesky )
library( diversitree )
library( musseco )

#read tree
#load("diversitree_sim/trees/sample_size_1000/params_1/rep1/diversitree_params_1_rep_1.rda")
load(list.files(pattern=".rda"))

# zero is variant, one is ancestral
# λ0 , λ1 , µ0 , µ1 , q01 , q10

dtphy <- tr
dtstates <- dtphy$tip.state
dtisv <- dtstates==0

dtfb_au <-  fitbisseco( dtphy,
                        dtisv,
                        Tg = 1/dtpars[3],
                        mu = dtpars[5],
                        Net = NULL,
                        theta0 = log(c(2,.75,1/2)),
                        optim_parms = list(),
                        mlesky_parms = list(tau = NULL,
                                            tau_lower = .1,
                                            tau_upper = 1e7,
                                            ncpu = nCPU,
                                            model = 1 ) )

dtfb_au$theoralpha <- dtpars[6] / dtpars[5]
dtfb_au$theoromega <- dtpars[1] / dtpars[2]
dtfb_au$theorpa <-  pancestral_mutsel_balance1( dtpars[5],
                                             dtpars[3] ,
                                             coef(dtfb_au)['alpha'],
                                             coef(dtfb_au)['omega'] )

dtfb_au$empiricalpa <- 1-mean( dtisv )

dtfb_au


#without using the augment likelihood
dtfb = fitbisseco( dtphy,
                   dtisv,
                   Tg=1/dtpars[3],
                   mu = dtpars[5],
                   Net = NULL,
                   theta0 = log(c(2,.75,1/2)),
                   augment_likelihood = FALSE,
                   optim_parms = list(),
                   mlesky_parms = list(tau = NULL,
                                       tau_lower = .1,
                                       tau_upper = 1e7,
                                       ncpu = nCPU,
                                       model = 1 ) )

dtfb$theoralpha <- dtpars[6] / dtpars[5]
dtfb$theoromega <- dtpars[1] / dtpars[2]
dtfb$theorpa <-  pancestral_mutsel_balance1( dtpars[5],
                                             dtpars[3] ,
                                             coef(dtfb)['alpha'],
                                             coef(dtfb)['omega'] )
dtfb$empiricalpa <- 1-mean( dtisv )

save(dtfb_au, dtfb, file = "diversitree_fitbisseco.rda")
