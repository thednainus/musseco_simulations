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

# stats ----
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

#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plots for coverage ----


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




#plots for percentage of Inf ----


#get values as percentages instead of proportion
prop_inf["omega_percentage"] <- prop_inf$prop_omega_upper_inf * 100
prop_inf["alpha_percentage"] <- prop_inf$prop_alpha_upper_inf * 100
prop_inf$sample_size <- as.factor(prop_inf$sample_size )
prop_inf$true_omega <- as.factor(prop_inf$true_omega)
prop_inf$true_alpha <- round(prop_inf$true_alpha, 2)
prop_inf$true_alpha <- as.factor(prop_inf$true_alpha)
prop_inf$prop_ancestral <- as.factor(prop_inf$prop_ancestral)






#plot coverage: omega
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


#plot coverage: alpha
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






# Precision ----
precision$sample_size <- as.factor(precision$sample_size )
precision$true_omega <- as.factor(precision$true_omega)
precision$true_alpha <- round(precision$true_alpha, 2)
precision$true_alpha <- as.factor(precision$true_alpha)
precision$prop_ancestral <- as.factor(precision$prop_ancestral)



precision_quantiles <- precision %>%
  group_by(sample_size, param, true_omega, true_alpha, likelihood, prop_ancestral, simulator) %>%
  reframe(lower_omega = quantile(prec_omega, probs = 0.025),
            median_omega = quantile(prec_omega, probs = 0.5),
            upper_omega = quantile(prec_omega, probs = 0.975),
            lower_alpha = quantile(prec_alpha, probs = 0.025),
            median_alpha = quantile(prec_alpha, probs = 0.5),
            upper_alpha = quantile(prec_alpha, probs = 0.975))


precision_quantiles2 <- precision_quantiles

precision_quantiles2$median_alpha[precision_quantiles2$median_alpha == Inf] <- NA
precision_quantiles2$upper_alpha[precision_quantiles2$upper_alpha == Inf] <- NA
precision_quantiles2$median_omega[precision_quantiles2$median_omega == Inf] <- NA
precision_quantiles2$upper_omega[precision_quantiles2$upper_omega == Inf] <- NA

quartz()
ggplot(precision_quantiles2, aes(x = true_omega, shape = prop_ancestral)) +
  geom_point(aes(y = median_omega, colour = likelihood), size = 4,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_omega, ymin = lower_omega,
                    width = 0.5, colour = likelihood),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(simulator ~ sample_size, scales = "free_y") +
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
  ylab("Precision") +
  theme(text = element_text(size = 20), legend.position = "bottom")



# Relative error ----

#To Do
