#model is here: https://github.com/emvolz/mussecotest/blob/master/musseco.yaml#L12
#this. might work: https://cran.r-project.org/web/packages/TiPS/vignettes/TiPS.html#Simulating_trajectories
library(TiPS)
library(ape)
library(glue)



reactions <- c("0 [beta * A * (1-(A+V)/K)] -> A",
               "A [gamma * A] -> 0",
               "A [mu_AV * A] -> V",
               "0 [beta * (1+s) * V * (1-(A+V)/K)] -> V",
               "V [gamma * V] -> 0",
               "V [mu_VA * V] -> A")

bd_simu <- build_simulator(reactions)

initialStates <- c(A = 1, V = 0)

time <- c(0, 1000)

safe_run <- function(f, ...) {
  out <- list()
  while(! length(out)) {out <- f(...)}
  out
}


safe_bd_simu <- function(...) safe_run(bd_simu, ...)

dnames = c( 'A', 'V' )
