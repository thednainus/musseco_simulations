
#precision: the difference between the upper and lower interval of the
# 95% credible interval divided by the true value

#coverage: how often among the replicates per parameter value does the credible
#interval contains the simulated true value

#relative error: shows the significance of an error by comparing it to the
#original error

library(dplyr)
library(stringr)

#get the data that was completed i = 800 iterations

CIs <- list.files("results/CIs", full.names = TRUE)


for(j in 1:length(CIs)){

  CI <- readRDS(CIs[j])

  #precision
  precision <- CI %>%
    group_by(sample_size, param, true_omega, true_alpha, likelihood) %>%
    reframe(prec_omega =  (omega_upper - omega_lower)/true_omega,
            prec_alpha = (alpha_upper - alpha_lower)/true_alpha)



  #coverage
  CI["within_range_omega"] <- ifelse(CI$true_omega >= CI$omega_lower  &
                                       CI$true_omega <= CI$omega_upper, "yes", "no")

  CI["within_range_alpha"] <- ifelse(CI$true_alpha >= CI$alpha_lower  &
                                       CI$true_alpha <= CI$alpha_upper, "yes", "no")

  coverage <- CI %>%
    group_by(sample_size, param, true_omega, true_alpha, likelihood) %>%
    reframe(coverage_omega =  sum(within_range_omega == "yes")/n(),
            coverage_alpha = sum(within_range_alpha == "yes")/n())




  #relative error for combined runs
  #precision
  relative_error <- CI %>%
    group_by(sample_size, param, true_omega, true_alpha, likelihood) %>%
    reframe(relative_error_omega =  abs((omega - true_omega)/true_omega),
            relative_error_alpha =  abs((alpha - true_alpha)/true_alpha))

  #proportion of replicates that upper bound was not possible to calculate
  prop_inf <- CI %>%
    group_by(sample_size, param, true_omega, true_alpha, likelihood) %>%
    reframe(prop_omega_upper_inf = sum(omega_upper == Inf)/n(),
            prop_alpha_upper_inf = sum(alpha_upper == Inf)/n())

  prefix <- str_split(str_split(CIs[j], "/")[[1]][3], "_")[[1]][1]


  saveRDS(coverage, paste(prefix, "_coverage.rds", sep = ""))
  saveRDS(precision, paste(prefix, "_precision.rds", sep = ""))
  saveRDS(relative_error, paste(prefix, "_relative_error.rds", sep = ""))
  saveRDS(prop_inf, paste(prefix, "_prop_inf.rds", sep = ""))
}
