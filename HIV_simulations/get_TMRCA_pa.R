#get estimated values for alpha and omega
library(musseco)
library(stringr)
library(fitMuSSECo)
library(treedater)

#alpha = inflation of the mutation rate from A (ancestral) to V (variant)
#omega = relative transmission fitness (between-host selection coefficient)

#read true parameter values
params_true <- read.csv("HIV_simulations/hiv_params.csv")

#add the results from params3 in which we used the same values for the
#v3 but let the simulation run for 45 year to each equilibrium
params_true <- rbind(params_true, params_true[1,])



#multiply beta (transmission rate) by 2 because that was the value used
#to generate the trajectories and trees with Coalescent.jl
params_true[,1] <- params_true[,1] * 2

#add the true value for the TMRCA
params_true["tmrca"] <- c(29, 45, 45)
#add the true value for the substitution rate
params_true["rate"] <- c(0.0036, 0.0032, 0.0036)



#read all data
loc_results <- "HIV_simulations/trees"
all_results <- list.files(loc_results,
                          pattern = "*.RDS",
                          recursive = TRUE,
                          full.names = TRUE)


all_data <- data.frame()

for(i in 1:length(all_results)){

  texts <- str_split(all_results[i], "/")
  sample_size <- str_split(texts[[1]][3], "_")[[1]][3]
  params <- texts[[1]][4]
  param_n <- str_split(params, "_")[[1]][2]

  rep <- texts[[1]][5]
  rep <- str_split(rep, "rep")[[1]][2]

  res <- readRDS(all_results[i])

  ajust_rate <- res$adjusted.mean.rate
  rate <- res$mean.rate


  tmrca <- res$timeToMRCA

  true_tmrca <- params_true$tmrca[as.numeric(param_n)]
  true_rate <- params_true$rate[as.numeric(param_n)]



  #proportion of ancestral
  pa <- sum(grepl( res$tip.label, pattern = '.A' ))/length(res$tip.label)

  true_pa <- params_true[param_n,8]




  results <- data.frame(param = param_n, rep = rep,
                        sample_size = sample_size,
                        true_tmrca = true_tmrca,
                        true_rate = true_rate,
                        true_pa = true_pa,
                        tmrca = tmrca,
                        pa = pa,
                        ajust_rate = ajust_rate,
                        rate = rate )


  all_data <- rbind(all_data, results)

}

saveRDS(all_data, "hivsim_results_treedater_tmrca.rds")

