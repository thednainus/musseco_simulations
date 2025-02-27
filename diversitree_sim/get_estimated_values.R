#get estimated values for alpha and omega
library(musseco)
library(stringr)
library(fitMuSSECo)


#alpha = inflation of the mutation rate from A (ancestral) to V (variant)
#omega = relative transmission fitness (between-host selection coefficient)

#read true parameter values
params_true <- readRDS("diversitree_sim/diversitree_params2.RDS")


#read all data#readRDS()read all data
loc_results <- "diversitree_sim/trees"

all_results <- list.files(loc_results,
                          pattern = "fitbisseco",
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

  #load reasults
  load(all_results[i])

  #no augmented likelihood
  cis <- get_CI(dtfb)

  alpha <- cis[1,1]
  alpha_lower <- cis[1,2]
  alpha_upper <- cis[1,3]

  omega <- cis[2,1]
  omega_lower <- cis[2,2]
  omega_upper <- cis[2,3]


  #using augmented likelihood
  cis_au <- get_CI(dtfb_au)

  alpha_au <- cis_au[1,1]
  alpha_au_lower <- cis_au[1,2]
  alpha_au_upper <- cis_au[1,3]

  omega_au <- cis_au[2,1]
  omega_au_lower <- cis_au[2,2]
  omega_au_upper <- cis_au[2,3]


  true_alpha =  unname(params_true[6,as.numeric(param_n)]/params_true[5,as.numeric(param_n)])
  true_omega =  unname(params_true[1,as.numeric(param_n)]/params_true[2,as.numeric(param_n)])

  if(param_n == 1){

    r0 <-  1.970763
    pa <- 0.85

  }else if(param_n == 2){

    r0 <- 1.736439
    pa <- 0.85

  }else if(param_n == 3){

    r0 <- 1.243854
    pa <- 0.95

  }else{

    r0 <- 1.194794
    pa <- 0.95
  }


  results <- data.frame(param = param_n, rep = rep,
                        sample_size = sample_size,
                        r0 = r0,
                        pa = pa,
                        true_alpha =  true_alpha,
                        true_omega =  true_omega,
                        omega = omega, omega_lower = omega_lower, omega_upper = omega_upper,
                        alpha = alpha, alpha_lower = alpha_lower, alpha_upper = alpha_upper,
                        likelihood = "normal")


  results_au <- data.frame(param = param_n, rep = rep,
                           sample_size = sample_size,
                           r0 = r0,
                           pa = pa,
                           true_alpha =  true_alpha,
                           true_omega =  true_omega,
                           omega = omega_au, omega_lower = omega_au_lower,
                           omega_upper = omega_au_upper,
                           alpha = alpha_au, alpha_lower = alpha_au_lower,
                           alpha_upper = alpha_au_upper,
                           likelihood = "augmented")


  all_data <- rbind(all_data, results, results_au)

}

saveRDS(all_data, "diversitree_fit_results.rds")

