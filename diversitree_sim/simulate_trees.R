#simulate trees using the program diversitree

library(ape)
library(glue)
library(diversitree)

n <- 1000 #max taxa in the tree
nrep <- 50 #total number of replicates

simtree <- function( dtpars, max.taxa = 1e3)
{
  #set.seed( 1111 )
  dtphy <- tree.bisse(dtpars, max.taxa = max.taxa, x0 = 1)
  while( is.null( dtphy ))
    dtphy <- tree.bisse(dtpars, max.taxa = max.taxa, x0 = 1)
  dtphy
}

params <- readRDS("diversitree_sim/diversitree_params.RDS")

for(i in 1:ncol(params)){

  dtpars <- params[,i]
  print(dtpars)

  for(j in 1:nrep){

    tr <- simtree(dtpars, max.taxa = n)

    dirname <- glue("diversitree_sim/trees/params_{i}/rep{j}")
    filename <- glue("diversitree_params_{i}_rep_{j}.rda")
    if(!file.exists(dirname)){
      dir.create(dirname, recursive = TRUE)
    }
    save(tr, dtpars, file = paste(dirname, "/", filename, sep = ""))
  }
}





#
# tr = simtree( parmmatrix[,9] )
# plot(tr)
# table( tr$tip.state )
