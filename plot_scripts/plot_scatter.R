# plot coverage, precision, relative error and proportion of inf
# proportion of Inf is those replicates in which the upper bound was not possible
# to estimate


#library(data.table)
library(ggplot2); theme_set(theme_bw(base_family ="Helvetica"))
library(dplyr)
#library(stringr)
#library(reshape2)

#get the data


#get CIs for trees simulated with Coalescent.jl ----
cis_julia <- readRDS("results/CIs/julia_fit_results.rds")
cis_julia["simulator"] <- "Coalescent.jl"
cis_julia["pa"] <- ifelse(cis_julia$param == 1 | cis_julia$param == 2,
                          0.85, 0.95)
cis_julia["average_r0"] <- "R[0]  %~~% 1.0"


#get CIs for trees simulated with TiPS ----
cis_tips <- readRDS("results/CIs/tips_fit_results.rds")
cis_tips["simulator"] <- "TiPS"
cis_tips["pa"] <- ifelse(cis_tips$param == 1 | cis_tips$param == 2,
                          0.85, 0.95)
cis_tips["average_r0"] <- "R[0]  %~~% 1.0"


#get CIs for trees simulated with diversitree ----
cis_dst <- readRDS("results/CIs/diversitree_fit_results.rds")
cis_dst["simulator"] <- "diversitree"
cis_dst["average_r0"] <- ifelse(cis_dst$param == 1 | cis_dst$param == 2,
                                "R[0]  %~~% 1.85", "R[0]  %~~% 1.22")



#merge all data
cis_all <- rbind(cis_julia, cis_tips, cis_dst[c(1:3,5:16)])

cis_all$sample_size <- as.factor(cis_all$sample_size )
cis_all$true_omega <- as.factor(cis_all$true_omega)
cis_all$true_alpha <- round(cis_all$true_alpha, 2)
cis_all$true_alpha <- as.factor(cis_all$true_alpha)
cis_all$pa <- as.factor(cis_all$pa)
cis_all$simulator <- as.factor(cis_all$simulator)
cis_all$simulator <- factor(cis_all$simulator,
                            levels=c("Coalescent.jl", "TiPS", "diversitree"))




estimates_quant <- cis_all %>%
  group_by(sample_size, param, true_omega, true_alpha, likelihood, pa, simulator, average_r0) %>%
  reframe(lower_omega = quantile(omega, probs = 0.025),
          median_omega = quantile(omega, probs = 0.5),
          upper_omega = quantile(omega, probs = 0.975),
          lower_alpha = quantile(alpha, probs = 0.025),
          median_alpha = quantile(alpha, probs = 0.5),
          upper_alpha = quantile(alpha, probs = 0.975))





# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plots for combined Julia, TiPS, and diversitree data ----



#plot coverage: omega
quartz()
ggplot(estimates_quant, aes(x = true_omega, y = median_omega, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_errorbar(aes(ymin = lower_omega,ymax = upper_omega, color=likelihood),
                position = position_dodge(width = 0.5),
                width = 0.1 ) +
  facet_grid(simulator + average_r0 ~ sample_size, scales = "free_y", labeller = label_parsed) +
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
  ylab("Estimate omega value") +
  theme(text = element_text(size = 15), legend.position = "bottom")

#plot coverage: alpha
quartz()
ggplot(estimates_quant, aes(x = true_alpha, y = median_alpha, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_errorbar(aes(ymin = lower_alpha, ymax = upper_alpha, color=likelihood),
                position = position_dodge(width = 0.5),
                width = 0.1 ) +
  facet_grid(simulator + average_r0 ~ sample_size, scales = "free_y", labeller = label_parsed) +
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
  ylab("Estimate alpha value") +
  theme(text = element_text(size = 15), legend.position = "bottom")




#plots for percentage of Inf ----


#get values as percentages instead of proportion
prop_inf_all["omega_percentage"] <- prop_inf_all$prop_omega_upper_inf * 100
prop_inf_all["alpha_percentage"] <- prop_inf_all$prop_alpha_upper_inf * 100
prop_inf_all$sample_size <- as.factor(prop_inf_all$sample_size )
prop_inf_all$true_omega <- as.factor(prop_inf_all$true_omega)

prop_inf_all$true_alpha <- round(prop_inf_all$true_alpha, 2)
prop_inf_all$true_alpha <- as.factor(prop_inf_all$true_alpha)
prop_inf_all$pa <- as.factor(prop_inf_all$pa)
prop_inf_all$simulator <- as.factor(prop_inf_all$simulator)
prop_inf_all$simulator <- factor(prop_inf_all$simulator,
                             levels=c("Coalescent.jl", "TiPS", "diversitree"))



#plot proportion of replicates we could not estimate the upper bound: omega
quartz()
ggplot(prop_inf_all, aes(x = true_omega, y = omega_percentage, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(simulator + average_r0 ~ sample_size, scales = "free_y", labeller = label_parsed) +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.75", "0.85", "0.95"),
              labels = c("75%", "85%", "95%")) +
  theme_bw() +
  xlab("True omega value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 15), legend.position = "bottom")


#plot proportion of replicates we could not estimate the upper bound: alpha
quartz()
ggplot(prop_inf_all, aes(x = true_alpha, y = alpha_percentage, shape = pa)) +
  geom_point(position = position_dodge(width = 0.5),
             aes(colour = likelihood), size = 3) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(simulator + average_r0 ~ sample_size, scales = "free_y", labeller = label_parsed) +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.75", "0.85", "0.95"),
              labels = c("75%", "85%", "95%")) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 15), legend.position = "bottom")




# Relative error ----

relative_error_all$sample_size <- as.factor(relative_error_all$sample_size )
relative_error_all$true_omega <- as.factor(relative_error_all$true_omega)
relative_error_all$true_alpha <- round(relative_error_all$true_alpha, 2)
relative_error_all$true_alpha <- as.factor(relative_error_all$true_alpha)
relative_error_all$pa <- as.factor(relative_error_all$pa)
relative_error_all$simulator <- as.factor(relative_error_all$simulator)
relative_error_all$simulator <- factor(relative_error_all$simulator,
                                       levels=c("Coalescent.jl", "TiPS", "diversitree"))




relerror_quant <- relative_error_all %>%
  group_by(sample_size, param, true_omega, true_alpha, likelihood, pa, simulator, average_r0) %>%
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
  geom_point(aes(y = median_omega, colour = likelihood, shape = pa),
             size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymax = upper_omega, ymin = lower_omega, width = 0.4,
                    colour = likelihood, shape = pa),
                position = position_dodge(width = 0.4)) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 0.00001, linetype="dotted") + #0.00001 was added here instead of 0 because log of 0 is undefined.
  facet_grid(simulator + average_r0 ~ sample_size, scales = "free_y", labeller = label_parsed) +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.75", "0.85", "0.95"),
              labels = c("75%", "85%", "95%")) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Relative error quantiles") +
  theme(text = element_text(size = 15), legend.position = "bottom")


#plot relative error quantiles for alpha
quartz()
ggplot(relerror_quant, aes(x = true_alpha )) +
  geom_point(aes(y = median_alpha, colour = likelihood, shape = pa),
             size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymax = upper_alpha, ymin = lower_alpha, width = 0.4,
                    colour = likelihood, shape = pa),
                position = position_dodge(width = 0.4)) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 0.00001, linetype="dotted") + #0.00001 was added here instead of 0 because log of 0 is undefined.
  facet_grid(simulator + average_r0 ~ sample_size, scales = "free_y", labeller = label_parsed) +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_shape(name = "% ancestral",
              breaks = c("0.75", "0.85", "0.95"),
              labels = c("75%", "85%", "95%")) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Relative error quantiles") +
  theme(text = element_text(size = 15), legend.position = "bottom")




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


