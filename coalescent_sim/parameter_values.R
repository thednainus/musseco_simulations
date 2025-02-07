# Parameter selection for use in coalescent.jl via musseco model
library(fitMuSSECo)

# High omega, low alpha
Tg <- 10.2
gamma <- 1 / Tg
beta <- 2.2 * gamma
mu_v_to_a <- 0.0016
#mu_a_to_v <- mu_v_to_a * 10
#alpha <- mu_a_to_v / mu_v_to_a
k <- 10000
s <- -0.1
omega <- 1+s
#proportion_a <- 0.85123338
proportion_a <- 0.95
R0 <- proportion_a * ( beta / gamma ) + (1-proportion_a) * ((omega * beta) / gamma)

alpha_q10 <- alpha_q10_mutsel_balance( pa = proportion_a
                                       , mu = mu_v_to_a
                                       , gamma = gamma
                                       , omega = omega
                                       , tol = 1e-3
)
alpha <- alpha_q10[1]
mu_a_to_v <- alpha_q10[2]

musseco_sim_pars_1 <- c( beta,
                         gamma,
                         k,
                         s,
                         mu_a_to_v,
                         mu_v_to_a,
                         R0,
                         proportion_a,
                         1-proportion_a,
                         alpha,
                         omega )

# Low omega, high alpha
#Tg <- 10.2
#gamma <- 1 / Tg
#beta <- 2.2 * gamma
#mu_v_to_a <- 0.0016
#mu_a_to_v <- mu_v_to_a * 10
##alpha <- mu_a_to_v / mu_v_to_a
#k <- 10000
s <- -0.9
omega <- 1+s
#proportion_a <- 0.85123338
# Compute R0 as weighted average of R for each of ancestral and variant
R0 <- proportion_a * ( beta / gamma ) + (1-proportion_a) * ((omega * beta) / gamma)

alpha_q10 <- alpha_q10_mutsel_balance( pa = proportion_a
                                       , mu = mu_v_to_a
                                       , gamma = gamma
                                       , omega = omega
                                       , tol = 1e-3
)
alpha <- alpha_q10[1]
mu_a_to_v <- alpha_q10[2]

musseco_sim_pars_2 <- c( beta, gamma, k, s, mu_a_to_v, mu_v_to_a, R0, proportion_a, 1-proportion_a, alpha, omega )

musseco_sim_pars <- cbind( musseco_sim_pars_1 , musseco_sim_pars_2 )

rownames(musseco_sim_pars) <- c( 'β', 'γ', 'K', 's', 'μ_a_to_v', 'μ_v_to_a',
                                 'R0' , 'propotion_ancestral', 'proportion_variant',
                                 'α (= μ_a_to_v/μ_v_to_a)' , 'ω (= 1+s)' )
colnames(musseco_sim_pars) <- c( 'high_omega_low_alpha' , 'low_omega_high_alpha' )

musseco_sim_pars <- as.data.frame(musseco_sim_pars)

params <- data.frame(beta = c(musseco_sim_pars$high_omega_low_alpha[1], musseco_sim_pars$low_omega_high_alpha[1]),
                     gamma = c(musseco_sim_pars$high_omega_low_alpha[2], musseco_sim_pars$low_omega_high_alpha[2]),
                     K = c(10000, 10000),
                     s = c(musseco_sim_pars$high_omega_low_alpha[4], musseco_sim_pars$low_omega_high_alpha[4]),
                     mu_AV = c(musseco_sim_pars$high_omega_low_alpha[5], musseco_sim_pars$low_omega_high_alpha[5]),
                     mu_VA = c(0.0016, 0.0016),
                     pa = c(musseco_sim_pars$high_omega_low_alpha[8], musseco_sim_pars$low_omega_high_alpha[8]),
                     alpha = c(musseco_sim_pars$high_omega_low_alpha[10], musseco_sim_pars$low_omega_high_alpha[10]),
                     omega = c(musseco_sim_pars$high_omega_low_alpha[11],musseco_sim_pars$low_omega_high_alpha[11]))



write.csv(x = params, file = "params2.csv", quote = FALSE, row.names = FALSE)
