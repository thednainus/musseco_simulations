#plot the credible intervals for each replicate

library(ggplot2)
library(dplyr)


cis_julia <- readRDS("results/CIs/hivsim_fit_results_true_trees.rds")
cis_julia["simulator"] <- "Coalescent.jl"
cis_julia$true_alpha <- round(cis_julia$true_alpha,2)
cis_julia$true_alpha <- as.factor(cis_julia$true_alpha)
cis_julia$true_omega <- as.factor(cis_julia$true_omega)
cis_julia["gene"] <- ifelse(cis_julia$param == 1, "V3", "env")


cis_julia_v3 <- readRDS("results/CIs/hivsim_fit_results_treedater_v3.rds")
cis_julia_v3["simulator"] <- "Estimated trees"
cis_julia_v3$true_alpha <- round(cis_julia_v3$true_alpha,2)
cis_julia_v3$true_alpha <- as.factor(cis_julia_v3$true_alpha)
cis_julia_v3$true_omega <- as.factor(cis_julia_v3$true_omega)
cis_julia_v3["gene"] <- "V3"


cis_julia_env <- readRDS("results/CIs/hivsim_fit_results_treedater_env.rds")
cis_julia_env["simulator"] <- "Estimated trees"
cis_julia_env$true_alpha <- round(cis_julia_env$true_alpha,2)
cis_julia_env$true_alpha <- as.factor(cis_julia_env$true_alpha)
cis_julia_env$true_omega <- as.factor(cis_julia_env$true_omega)
cis_julia_env["gene"] <- "env"



cis_julia_au <- subset(cis_julia, likelihood == "augmented")

all_cis <- rbind(cis_julia_au, cis_julia_v3, cis_julia_env)
all_cis$simulator <- as.factor(all_cis$simulator)
all_cis$gene <- as.factor(all_cis$gene)
all_cis$likelihood <- as.factor(all_cis$likelihood)
rep_order <- as.character(sort(as.numeric(unique(all_cis$rep))))
all_cis$rep <- factor(all_cis$rep, levels = rep_order)


#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

v3 <- subset(all_cis, gene == "V3")
env <- subset(all_cis, gene == "env")

# plots for V3 loop ----
plot_alpha <- ggplot(v3, aes(x = rep )) +
  geom_point(aes(y = alpha, colour = simulator),
             size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = alpha_upper, ymin = alpha_lower,
                    colour = simulator),
                position = position_dodge(width = 0.9), width = 0.5) +
  geom_hline(aes(yintercept = as.numeric(as.character(true_alpha))), linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c(2,4)]) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  xlab("Replicate number") +
  ylab("Alpha credible interval") +
  ggtitle("Replicates results for V3 loop")  +
  theme(text = element_text(size = 20), legend.position = "bottom")

plot_omega <- ggplot(v3, aes(x = rep )) +
  geom_point(aes(y = omega, colour = simulator),
             size = 2,position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = omega_upper, ymin = omega_lower,
                    colour = simulator), position = position_dodge(width = 0.9),
                width = 0.5) +
  geom_hline(aes(yintercept = as.numeric(as.character(true_omega))), linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c(2,4)]) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  xlab("Replicate number") +
  ylab("Omega credible interval") +
  ggtitle("Replicates results for V3 loop") +
  theme(text = element_text(size = 20), legend.position = "bottom")



# plots for env gene ----
plot_alpha_env <- ggplot(env, aes(x = rep )) +
  geom_point(aes(y = alpha, colour = simulator),
             size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = alpha_upper, ymin = alpha_lower,
                    colour = simulator),
                position = position_dodge(width = 0.9), width = 0.5) +
  geom_hline(aes(yintercept = as.numeric(as.character(true_alpha))), linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c(2,4)]) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  xlab("Replicate number") +
  ylab("Alpha credible interval") +
  ggtitle("Replicates results for env gene")  +
  theme(text = element_text(size = 20), legend.position = "bottom")

plot_omega_env <- ggplot(env, aes(x = rep )) +
  geom_point(aes(y = omega, colour = simulator),
             size = 2,position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = omega_upper, ymin = omega_lower,
                    colour = simulator), position = position_dodge(width = 0.9),
                width = 0.5) +
  geom_hline(aes(yintercept = as.numeric(as.character(true_omega))), linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c(2,4)]) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  xlab("Replicate number") +
  ylab("Omega credible interval") +
  ggtitle("Replicates results for env gene") +
  theme(text = element_text(size = 20), legend.position = "bottom")
