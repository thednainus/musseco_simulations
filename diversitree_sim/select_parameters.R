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

# > pancestral_mutsel_balance1( .001, 52, 2000, .85 )
# [1] 0.7531224

#High R0
# More likely to see accumulation of transmission-deleterious mutations in HIV
# than e.g. flu b/c of longer generation times (dtpars[3])
sapply( 1:9, function(x)
{
  # zero is variant, one is ancestral
  # λ0 , λ1 , µ0 , µ1 , q01 , q10
  # diversitree package (FitzJohn 2012, Diversitree: comparative phylogenetic
  # analyses of diversification in R, Methods in Ecology and Evolution,
  # 3, 1084-1092)
  # lineages in state i speciate at rate λi;
  # lineages go extinct at rate µi;
  # transition to state j ≠ i at rate qij
  # there are k speciation rates; k extinction rates and k(k-1) transition rates

  dtpars <- c( (1-x*.10)*2/10
               , 2/10
               , 1/10, 1/10
               , 0.0015
               , 0.0015*(1*(1+x))
  )
  print( '-------------' )
  print( x )
  print( dtpars )
  c( dtR0( dtpars ) , pancestral_mutsel_balance1( dtpars[5],
                                                  dtpars[3] ,
                                                  dtpars[6]/dtpars[5],
                                                  dtpars[1]/dtpars[2] ) ) |> print()
  print( dtpars[1] / dtpars[2] )
  dtpars
}
) -> parmmatrix
rownames(parmmatrix) <- c( 'λ0' , 'λ1' , 'µ0' , 'µ1' , 'q01' , 'q10')

parmmatrix1 <- parmmatrix[,c(1,9)]

#lower R0
sapply( 1:9, function(x)
{
  # zero is variant, one is ancestral
  # λ0 , λ1 , µ0 , µ1 , q01 , q10
  # For details on the diversitree package pars (dtpars),
  # see Fitzjohn (2012), Diversitree: comparative phylogenetic analyses of diversification in R,
  # Methods in Ecology and Evolution, 3(6): 1084-1092, DOI: 10.1111/j.2041-210X.2012.00234.x
  dtpars <- c( (1-x*.10)*1.25/10 #(1-x*.10)*2/10    # λ0 (speciation rate of state 0 (variant))
               , 1.25/10          #2/10              # λ1 (speciation rate of state 1 (ancestral))
               , 1/10             #1/10              # µ0 (extinction rate of variant state (0) )
               , 1/10             #1/10              # µ1 (extinction rate of ancestral state (1))
               , 0.0015           #0.0015            # q01 (transition rate from variant (0) to ancestral (1) state)
               , 0.0015*(1*(1+x)) #0.0015*(1*(1+x))  # q10 (transition rate from ancestral (1) to variant (0) state)
  )
  print( '-------------' )
  print( x )
  print( dtpars )
  c( dtR0( dtpars ) , pancestral_mutsel_balance1( dtpars[5], dtpars[3] , dtpars[6]/dtpars[5], dtpars[1]/dtpars[2] ) ) |> print()
  print( dtpars[1] / dtpars[2] )
  dtpars
}
) -> parmmatrix
rownames(parmmatrix) <- c( 'λ0' , 'λ1' , 'µ0' , 'µ1' , 'q01' , 'q10'  )
parmmatrix2 <- parmmatrix[,c(1,9)]


all_params <- cbind(parmmatrix1, parmmatrix2)
saveRDS(all_params, "diversitree_params.RDS")

