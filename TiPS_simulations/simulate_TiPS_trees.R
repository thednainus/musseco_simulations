library(TiPS)
library(ape)
library(glue)

#number of replicates per parameter value
nrep <- 50

source("TiPS_simulations/tips_model.R")

params <- read.csv("params_all.csv")

i=1
while(i <= 4){

  print(i)
  beta <- params[i,]$beta
  gamma <- params[i,]$gamma

  mu_AV <- params[i,]$mu_AV
  mu_VA <- params[i,]$mu_VA
  s <- params[i,]$s


  print(paste("mu_AV:", mu_AV, sep = " "))
  print(paste("mu_VA:", mu_VA, sep = " "))
  print(paste("s:", s, sep = " "))
  print(paste("beta:", beta, sep = " "))
  print(paste("gamma:", gamma, sep = " "))

  theta <- list(gamma = gamma,
                beta = beta,
                mu_AV = mu_AV,
                mu_VA = mu_VA,
                s = s,
                K = 10000.0)

  #simulate trajectory
  traj_dm <- safe_bd_simu(
    paramValues = theta,
    initialStates = initialStates,
    times = time,
    method = "exact")

  for(j in 1:nrep){

    print(j)

    #read trees simulated with Julia and get sample times from there
    tr = read.tree(glue("/Users/sofia/Desktop/Imperial/R_packages/fitMuSSECo/CoalescentJl_simulations/trees/params_{i}/rep{j}/param_{i}_rep_{j}.nwk"))
    demes = sapply( strsplit( tr$tip.label, split='\\.'), '[', 2 )

    #get sample times
    sts <- node.depth.edgelength( tr )[1:Ntip(tr)] |> setNames( tr$tip.label )
    sts <- sts + (1e3 - max(sts))
    dates_bd <- data.frame(Date = unname(sts), Comp = demes)

    #simulate phylogenetic trees
    bd_tree <- simulate_tree(
      simuResults = traj_dm,
      dates = dates_bd,
      deme = dnames,
      root = "A",
      nTrials = 3,
      addInfos = TRUE) # additional info for each node


    dirname <- glue("TiPS_simulations/trees/params_{i}/rep{j}")
    filename <- glue("tips_params_{i}_rep_{j}.rds")
    if(!file.exists(dirname)){
      dir.create(dirname, recursive = TRUE)
    }
    saveRDS(bd_tree, file = paste(dirname, "/", filename, sep = ""))
  }

  i = i + 1
}
