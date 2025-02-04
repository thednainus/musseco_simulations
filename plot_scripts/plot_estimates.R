#plot results

library(ggplot2)


#read data in which trees were simulated with CoalescentJl
julia_data <- readRDS("results/julia_fit_results.rds")
julia_data["method"] <- "coalescentJl"


#read data in which trees were simulated with TiPS
tips_data <- readRDS("results/tips_fit_results.rds")
tips_data["method"] <- "TiPS"

# all data
all_data <- rbind(julia_data, tips_data)
rep_order <- as.character(sort(as.numeric(unique(all_data$rep))))
all_data$rep <- factor(all_data$rep, levels=rep_order)

#high omega and low omega
params1 <- subset(all_data, true_omega == 0.9)
params2 <- subset(all_data, true_omega == 0.1)
params3 <- subset(all_data, true_omega == 0.9)
params4 <- subset(all_data, true_omega == 0.1)


#subset to "normal" likelihood only for the low omega
normal_lik <- subset(low_omega, likelihood == "normal")
au_lik <-  subset(low_omega, likelihood == "augmented")





#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# plotting omega when : high omega and low omega
quartz()
ggplot(high_omega, aes(x = rep, y = omega, colour = likelihood)) +
  geom_point(size = 1, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = omega_upper, ymin = omega_lower),
                width = 1, position= position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0.9, linetype="dotted") +
  facet_wrap(~ method, scales = "free") +
  scale_colour_manual(values = cbbPalette[c (4,7)]) +
  theme_bw() +
  ylab("Confidence interval for omega estimates") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")

quartz()
ggplot(low_omega, aes(x = rep, y = omega, colour = likelihood)) +
  geom_point(size = 1, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = omega_upper, ymin = omega_lower),
                width = 1, position= position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0.1, linetype="dotted") +
  facet_wrap(~ method, scales = "free") +
  scale_colour_manual(values = cbbPalette[c (4,7)]) +
  theme_bw() +
  ylab("Confidence interval for omega estimates") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")


# plotting alpha when : high alpha and low alpha
quartz()
ggplot(high_omega, aes(x = rep, y = alpha, colour = likelihood)) +
  geom_point(size = 1, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = alpha_upper, ymin = alpha_lower),
                width = 1, position= position_dodge(width = 0.8)) +
  geom_hline(yintercept = 1.100092, linetype="dotted") +
  facet_wrap(~ method, scales = "free") +
  scale_colour_manual(values = cbbPalette[c (4,7)]) +
  theme_bw() +
  ylab("Confidence interval for alpha estimates") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")

quartz()
ggplot(low_omega, aes(x = rep, y = alpha, colour = likelihood)) +
  geom_point(size = 1, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = alpha_upper, ymin = alpha_lower),
                width = 1, position= position_dodge(width = 0.8)) +
  geom_hline(yintercept = 9.647051, linetype="dotted") +
  facet_wrap(~ method, scales = "free") +
  scale_colour_manual(values = cbbPalette[c (4,7)]) +
  theme_bw() +
  ylab("Confidence interval for alpha estimates") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")


#plotting the normal likelihood for alpha
quartz()
ggplot(normal_lik, aes(x = rep, y = omega, colour = likelihood)) +
  geom_point(size = 1, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = omega_upper, ymin = omega_lower),
                width = 1, position= position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0.1, linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c (7)]) +
  facet_wrap(~ method, scales = "free") +
  theme_bw() +
  ylab("Confidence interval for omega estimates") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")

quartz()
ggplot(au_lik, aes(x = rep, y = alpha, colour = likelihood)) +
  geom_point(size = 1, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = alpha_upper, ymin = alpha_lower),
                width = 1, position= position_dodge(width = 0.8)) +
  geom_hline(yintercept = 9.647051, linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c (4)]) +
  facet_wrap(~ method, scales = "free") +
  theme_bw() +
  ylab("Confidence interval for alpha estimates") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")





