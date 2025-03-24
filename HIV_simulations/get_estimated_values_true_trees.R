#get estimated values for alpha and omega
library(musseco)
library(stringr)
library(fitMuSSECo)

#alpha = inflation of the mutation rate from A (ancestral) to V (variant)
#omega = relative transmission fitness (between-host selection coefficient)

#read true parameter values
params_true <- read.csv("HIV_simulations/hiv_params.csv")

#add the results from params3 in which we used the same values for the
#v3 but let the simulation run for 45 year to each equilibrium
params_true <- rbind(params_true, params_true[1,])

# add params4 too which is the same as params1 but the trajectory was run
# for 1000 years to reach equilibrium
params_true <- rbind(params_true, params_true[1,])

#multiply beta (transmission rate) by 2 because that was the value used
#to generate the trajectories and trees with Coalescent.jl
params_true[,1] <- params_true[,1] * 2

#add the true value for the TMRCA
params_true["tmrca"] <- c(29, 45, 45, 1000)


#read all data
loc_results <- "HIV_simulations/trees"
all_results <- list.files(loc_results,
                          pattern = "julia_fitbisseco.rda",
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

  res <- load(all_results[i])

  #no augmented likelihood
  cis <- get_CI(fb)

  alpha <- cis[1,1]
  alpha_lower <- cis[1,2]
  alpha_upper <- cis[1,3]

  omega <- cis[2,1]
  omega_lower <- cis[2,2]
  omega_upper <- cis[2,3]


  #using augmented likelihood
  cis_au <- get_CI(fb_au)

  alpha_au <- cis_au[1,1]
  alpha_au_lower <- cis_au[1,2]
  alpha_au_upper <- cis_au[1,3]

  omega_au <- cis_au[2,1]
  omega_au_lower <- cis_au[2,2]
  omega_au_upper <- cis_au[2,3]




  results <- data.frame(param = param_n, rep = rep,
                        sample_size = sample_size,
                        true_tmrca = params_true$tmrca[as.numeric(param_n)],
                        true_alpha =  params_true$alpha[as.numeric(param_n)],
                        true_omega =  params_true$omega[as.numeric(param_n)],
                        omega = omega, omega_lower = omega_lower,
                        omega_upper = omega_upper,
                        alpha = alpha, alpha_lower = alpha_lower,
                        alpha_upper = alpha_upper,
                        likelihood = "normal")


  results_au <- data.frame(param = param_n, rep = rep,
                           sample_size = sample_size,
                           true_tmrca = params_true$tmrca[as.numeric(param_n)],
                           true_alpha =  params_true$alpha[as.numeric(param_n)],
                           true_omega =  params_true$omega[as.numeric(param_n)],
                           omega = omega_au, omega_lower = omega_au_lower,
                           omega_upper = omega_au_upper,
                           alpha = alpha_au, alpha_lower = alpha_au_lower,
                           alpha_upper = alpha_au_upper,
                           likelihood = "augmented")


  all_data <- rbind(all_data, results, results_au)

}

saveRDS(all_data, "hivsim_fit_results_true_trees.rds")

