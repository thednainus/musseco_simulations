#results obtained with the real HIV data

#number of tips
n <- 4056
pv <- 812/n
pa <- 1 - pv

mu_v3 <- 3.6e-03
mu_env <- 3.2e-03

alpha_v3 <- 3.96
alpha_env <- 6.87

omega_v3 <- 0.39
omega_env <- 0.09

#selection coefficient
s_v3 <- omega_v3 - 1
s_env <- omega_env - 1


#q10_v3 <- alpha_v3 * mu_v3
q10_v3 <- 0.01453918  #based on the mutation-selection balanced
#q10_env <- alpha_env * mu_env
q10_env <- 0.02264042 #based on the mutation-selection balanced


#new parameters based on the coalescent data
params <- read.csv("coalescent_sim/params_all.csv")

hiv_params <- data.frame(beta = params$beta[1:2]*2,
                         gamma = params$gamma[1:2],
                         K = params$K[1:2],
                         s = c(s_v3, s_env),
                         omega = c(omega_v3, omega_env),
                         mu_AV = c(q10_v3, q10_env),
                         mu_VA = c(mu_v3, mu_env),
                         pa = c(pa,pa),
                         alpha = c(q10_v3/mu_v3, q10_env/mu_env))

write.csv(x = hiv_params, file = "hiv_params.csv", quote = FALSE, row.names = FALSE)




#alpha_q10_mutsel_balance( pa = pa,
#                          mu = mu_env,
#                          gamma = 0.09803922,
#                          omega = omega_env,
#                          tol = 1e-3)
