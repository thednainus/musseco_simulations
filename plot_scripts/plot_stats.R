# plot coverage, precision, relative error and proportion of inf
# proportion of Inf is those replicates in which the upper bound was not possible
# to estimate


library(data.table)
library(ggplot2); theme_set(theme_bw(base_family ="Helvetica"))
library(dplyr)
library(stringr)
library(reshape2)

#get the data


#stats for trees simulated with Coalescent.jl ----

coverage_julia <- readRDS("results/julia_coverage.rds")
coverage_julia["simulator"] <- "Coalescent.jl"
precision_julia <- readRDS("results/julia_precision.rds")
precision_julia["simulator"] <- "Coalescent.jl"
relative_error_julia <- readRDS("results/julia_relative_error.rds")
relative_error_julia["simulator"] <- "Coalescent.jl"
prop_inf_julia <- readRDS("results/julia_prop_inf.rds")
prop_inf_julia["simulator"] <- "Coalescent.jl"


#stats for trees simulated with TiPS ----

coverage_tips <- readRDS("results/tips_coverage.rds")
coverage_tips["simulator"] <- "TiPS"
precision_tips <- readRDS("results/tips_precision.rds")
precision_tips["simulator"] <- "TiPS"
relative_error_tips <- readRDS("results/tips_relative_error.rds")
relative_error_tips["simulator"] <- "TiPS"
prop_inf_tips <- readRDS("results/tips_prop_inf.rds")
prop_inf_tips["simulator"] <- "TiPS"


#stats for trees simulated with diversitree ----

coverage_dst <- readRDS("results/diversitree_coverage.rds")
coverage_dst["simulator"] <- "diversitree"
precision_dst <- readRDS("results/diversitree_precision.rds")
precision_dst["simulator"] <- "diversitree"
relative_error_dst <- readRDS("results/diversitree_relative_error.rds")
relative_error_dst["simulator"] <- "diversitree"
prop_inf_dst <- readRDS("results/diversitree_prop_inf.rds")
prop_inf_dst["simulator"] <- "diversitree"



# stats for combined julia and tips data ----
coverage <- rbind(coverage_julia, coverage_tips)
coverage["prop_ancestral"] <- ifelse(coverage$param == 1 | coverage$param == 2,
                                     0.85, 0.95)
precision <- rbind(precision_julia, precision_tips)
precision["prop_ancestral"] <- ifelse(precision$param == 1 | precision$param == 2,
                                      0.85, 0.95)
relative_error <- rbind(relative_error_julia, relative_error_tips)
relative_error["prop_ancestral"] <- ifelse(relative_error$param == 1 | relative_error$param == 2,
                                           0.85, 0.95)
prop_inf <- rbind(prop_inf_julia, prop_inf_tips)
prop_inf["prop_ancestral"] <- ifelse(prop_inf$param == 1 | prop_inf$param == 2,
                                     0.85, 0.95)




# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plots for combined Julia and TiPS data ----

#coverage
#get values as percentages instead of proportion
coverage["omega_percentage"] <- coverage$coverage_omega * 100
coverage["alpha_percentage"] <- coverage$coverage_alpha * 100
coverage$sample_size <- as.factor(coverage$sample_size )
coverage$true_omega <- as.factor(coverage$true_omega)
coverage$true_alpha <- round(coverage$true_alpha, 2)
coverage$true_alpha <- as.factor(coverage$true_alpha)
coverage$prop_ancestral <- as.factor(coverage$prop_ancestral)






#plot coverage: omega
quartz()
ggplot(coverage, aes(x = true_omega, y = omega_percentage, shape = prop_ancestral)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 100, linetype="dotted") +
  facet_grid(simulator ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.85", "0.95"),
              labels = c("85%", "95%")) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Coverage") +
  theme(text = element_text(size = 20), legend.position = "bottom")

#plot coverage: alpha
quartz()
ggplot(coverage, aes(x = true_alpha, y = alpha_percentage, shape = prop_ancestral)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 100, linetype="dotted") +
  facet_grid(simulator ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.85", "0.95"),
              labels = c("85%", "95%")) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Coverage") +
  theme(text = element_text(size = 20), legend.position = "bottom")




#plots for percentage of Inf


#get values as percentages instead of proportion
prop_inf["omega_percentage"] <- prop_inf$prop_omega_upper_inf * 100
prop_inf["alpha_percentage"] <- prop_inf$prop_alpha_upper_inf * 100
prop_inf$sample_size <- as.factor(prop_inf$sample_size )
prop_inf$true_omega <- as.factor(prop_inf$true_omega)
prop_inf$true_alpha <- round(prop_inf$true_alpha, 2)
prop_inf$true_alpha <- as.factor(prop_inf$true_alpha)
prop_inf$prop_ancestral <- as.factor(prop_inf$prop_ancestral)


#plot proportion of replicates we could not estimate the upper bound: omega
quartz()
ggplot(prop_inf, aes(x = true_omega, y = omega_percentage, shape = prop_ancestral)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(simulator ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.85", "0.95"),
              labels = c("85%", "95%")) +
  theme_bw() +
  xlab("True omega value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 20), legend.position = "bottom")


#plot proportion of replicates we could not estimate the upper bound: alpha
quartz()
ggplot(prop_inf, aes(x = true_alpha, y = alpha_percentage, shape = prop_ancestral)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(simulator ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.85", "0.95"),
              labels = c("85%", "95%")) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 20), legend.position = "bottom")




# Relative error ----

relative_error$sample_size <- as.factor(relative_error$sample_size )
relative_error$true_omega <- as.factor(relative_error$true_omega)
relative_error$true_alpha <- round(relative_error$true_alpha, 2)
relative_error$true_alpha <- as.factor(relative_error$true_alpha)
relative_error$prop_ancestral <- as.factor(relative_error$prop_ancestral)



relerror_quant <- relative_error %>%
  group_by(sample_size, param, true_omega, true_alpha, likelihood, prop_ancestral, simulator) %>%
  reframe(lower_omega = quantile(relative_error_omega, probs = 0.025),
          median_omega = quantile(relative_error_omega, probs = 0.5),
          upper_omega = quantile(relative_error_omega, probs = 0.975),
          lower_alpha = quantile(relative_error_alpha, probs = 0.025),
          median_alpha = quantile(relative_error_alpha, probs = 0.5),
          upper_alpha = quantile(relative_error_alpha, probs = 0.975))

#for omega because log of zero is undefined
#we had 1 observation for the lower bound of omega = 0.
#to pplot the relative error at log scale we added a small constant to the value
#of omega = 0
min_non_zero <- min(relerror_quant$lower_omega[relerror_quant$lower_omega > 0], na.rm = TRUE)
relerror_quant$lower_omega[relerror_quant$lower_omega == 0] <- min_non_zero / 2


#plot relative error quantiles for omega
quartz()
ggplot(relerror_quant, aes(x = true_omega )) +
  geom_point(aes(y = median_omega, colour = likelihood, shape = prop_ancestral),
             size = 4, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_omega, ymin = lower_omega, width = 0.5,
                    colour = likelihood, shape = prop_ancestral),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0.00001, linetype="dotted") + #0.00001 was added here instead of 0 because log of 0 is undefined.
  scale_y_log10() +
  facet_grid(simulator ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.85", "0.95"),
              labels = c("85%", "95%")) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Relative error quantiles") +
  theme(text = element_text(size = 20), legend.position = "bottom")


#plot relative error quantiles for alpha
quartz()
ggplot(relerror_quant, aes(x = true_alpha )) +
  geom_point(aes(y = median_alpha, colour = likelihood, shape = prop_ancestral), size = 4,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_alpha, ymin = lower_alpha, width = 0.5,
                    colour = likelihood, shape = prop_ancestral),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0.00001, linetype="dotted") +
  scale_y_log10() +
  facet_grid(simulator ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.85", "0.95"),
              labels = c("85%", "95%")) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Relative error quantiles") +
  theme(text = element_text(size = 20), legend.position = "bottom")



#Diversitree ----
#coverage ----


#get values as percentages instead of proportion
coverage_dst["omega_percentage"] <- coverage_dst$coverage_omega * 100
coverage_dst["alpha_percentage"] <- coverage_dst$coverage_alpha * 100
coverage_dst$sample_size <- as.factor(coverage_dst$sample_size )
coverage_dst$true_omega <- as.factor(coverage_dst$true_omega)
coverage_dst$true_alpha <- round(coverage_dst$true_alpha, 2)
coverage_dst$true_alpha <- as.factor(coverage_dst$true_alpha)
coverage_dst["average_r0"] <- ifelse(coverage_dst$param == 1 | coverage_dst$param == 2, 1.85, 1.15)
coverage_dst$r0 <- as.factor(coverage_dst$r0)
coverage_dst$average_r0 <- as.factor(coverage_dst$average_r0)
coverage_dst$pa <- as.factor(coverage_dst$pa)





#plot coverage: omega
quartz()
ggplot(coverage_dst, aes(x = true_omega, y = omega_percentage, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 100, linetype="dotted") +
  facet_grid(average_r0 ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.86", "0.75"),
              labels = c("86%", "75%")) +
  xlab("True omega value") +
  ylab("Coverage") +
  theme(text = element_text(size = 20), legend.position = "bottom")

#plot coverage: alpha
quartz()
ggplot(coverage_dst, aes(x = true_alpha, y = alpha_percentage, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 100, linetype="dotted") +
  facet_grid(average_r0 ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.86", "0.75"),
              labels = c("86%", "75%")) +
  xlab("True alpha value") +
  ylab("Coverage") +
  theme(text = element_text(size = 20), legend.position = "bottom")




#plots for percentage of Inf ----


#get values as percentages instead of proportion
prop_inf_dst["omega_percentage"] <- prop_inf_dst$prop_omega_upper_inf * 100
prop_inf_dst["alpha_percentage"] <- prop_inf_dst$prop_alpha_upper_inf * 100
prop_inf_dst$sample_size <- as.factor(prop_inf_dst$sample_size )
prop_inf_dst$true_omega <- as.factor(prop_inf_dst$true_omega)
prop_inf_dst$true_alpha <- round(prop_inf_dst$true_alpha, 2)
prop_inf_dst$true_alpha <- as.factor(prop_inf_dst$true_alpha)
prop_inf_dst["average_r0"] <- ifelse(prop_inf_dst$param == 1 | prop_inf_dst$param == 2, 1.85, 1.15)
prop_inf_dst$r0 <- as.factor(prop_inf_dst$r0)
prop_inf_dst$average_r0 <- as.factor(prop_inf_dst$average_r0)
prop_inf_dst$pa <- as.factor(prop_inf_dst$pa)


#plot proportion of replicates we could not estimate the upper bound: omega
quartz()
ggplot(prop_inf_dst, aes(x = true_omega, y = omega_percentage, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(average_r0 ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.86", "0.75"),
              labels = c("86%", "75%")) +
  theme_bw() +
  xlab("True omega value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 20), legend.position = "bottom")


#plot proportion of replicates we could not estimate the upper bound: alpha
quartz()
ggplot(prop_inf_dst, aes(x = true_alpha, y = alpha_percentage, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(average_r0 ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.86", "0.75"),
              labels = c("86%", "75%")) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 20), legend.position = "bottom")





# Relative error ----

relative_error_dst$sample_size <- as.factor(relative_error_dst$sample_size )
relative_error_dst$true_omega <- as.factor(relative_error_dst$true_omega)
relative_error_dst$true_alpha <- round(relative_error_dst$true_alpha, 2)
relative_error_dst$true_alpha <- as.factor(relative_error_dst$true_alpha)
relative_error_dst["average_r0"] <- ifelse(relative_error_dst$param == 1 | relative_error_dst$param == 2, 1.85, 1.15)
relative_error_dst$r0 <- as.factor(relative_error_dst$r0)
relative_error_dst$average_r0 <- as.factor(relative_error_dst$average_r0)
relative_error_dst$pa <- as.factor(relative_error_dst$pa)



relerror_quant <- relative_error_dst %>%
  group_by(sample_size, param, true_omega, true_alpha, likelihood, average_r0, pa, r0) %>%
  reframe(lower_omega = quantile(relative_error_omega, probs = 0.025),
          median_omega = quantile(relative_error_omega, probs = 0.5),
          upper_omega = quantile(relative_error_omega, probs = 0.975),
          lower_alpha = quantile(relative_error_alpha, probs = 0.025),
          median_alpha = quantile(relative_error_alpha, probs = 0.5),
          upper_alpha = quantile(relative_error_alpha, probs = 0.975))


#plot relative error quantiles for omega
quartz()
ggplot(relerror_quant, aes(x = true_omega )) +
  geom_point(aes(y = median_omega, colour = likelihood, shape = pa),
             size = 4, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_omega, ymin = lower_omega, width = 0.5,
                    colour = likelihood, shape = pa),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0.00001, linetype="dotted") + #0.00001 was added here instead of 0 because log of 0 is undefined.
  scale_y_log10() +
  facet_grid(average_r0 ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.86", "0.75"),
              labels = c("86%", "75%")) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Relative error quantiles") +
  theme(text = element_text(size = 20), legend.position = "bottom")


#plot relative error quantiles for alpha
quartz()
ggplot(relerror_quant, aes(x = true_alpha )) +
  geom_point(aes(y = median_alpha, colour = likelihood, shape = pa), size = 4,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_alpha, ymin = lower_alpha, width = 0.5,
                    colour = likelihood, shape = pa),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0.00001, linetype="dotted") +
  scale_y_log10() +
  facet_grid(average_r0 ~ sample_size, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.86", "0.75"),
              labels = c("86%", "75%")) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Relative error quantiles") +
  theme(text = element_text(size = 20), legend.position = "bottom")


