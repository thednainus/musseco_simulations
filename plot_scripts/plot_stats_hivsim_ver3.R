# plot coverage, precision, and relative error for true trees

library(data.table)
library(ggplot2); theme_set(theme_bw(base_family ="Helvetica"))
library(dplyr)
library(stringr)
library(reshape2)
library(scales)
library(patchwork)

#source data for sina plots
source("plot_scripts/plot_scatter_hivsim_ver3.R")

#get the data


#stats for trees simulated with Coalescent.jl ----
coverage_ttr <- readRDS("results/hivsim/hivsim_coverage.rds")
coverage_ttr["simulator"] <- "Coalescent.jl"
coverage_ttr["gene"] <- ifelse(coverage_ttr$param == 2, "env", "V3")
relative_error_ttr <- readRDS("results/hivsim/hivsim_relative_error.rds")
relative_error_ttr["simulator"] <- "Coalescent.jl"
relative_error_ttr["gene"] <- ifelse(relative_error_ttr$param == 2, "env", "V3")
precision_ttr <- readRDS("results/hivsim/hivsim_precision.rds")
precision_ttr["simulator"] <- "Coalescent.jl"
precision_ttr["gene"] <- ifelse(precision_ttr$param == 2, "env", "V3")

# stats for estimated trees ----
coverage_estr <- readRDS("results/hivsim/hivsim_estimated_trees_coverage.rds")
coverage_estr["simulator"] <- "Estimated trees"
coverage_estr["gene"] <- ifelse(coverage_estr$param == 2, "env", "V3")
relative_error_estr <- readRDS("results/hivsim/hivsim_estimated_trees_relative_error.rds")
relative_error_estr["simulator"] <- "Estimated trees"
relative_error_estr["gene"] <- ifelse(relative_error_estr$param == 2, "env", "V3")
precision_estr <- readRDS("results/hivsim/hivsim_estimated_trees_precision.rds")
precision_estr["simulator"] <- "Estimated trees"
precision_estr["gene"] <- ifelse(precision_estr$param == 2, "env", "V3")






# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plots for combined Julia, TiPS, and diversitree data ----

#coverage

coverage <- rbind(coverage_ttr, coverage_estr)

#get values as percentages instead of proportion
coverage["omega_percentage"] <- coverage$coverage_omega * 100
coverage["alpha_percentage"] <- coverage$coverage_alpha * 100
coverage$sample_size <- as.factor(coverage$sample_size)
coverage$true_omega <- as.factor(coverage$true_omega)
coverage$true_alpha <- round(coverage$true_alpha, 2)
coverage$true_alpha <- as.factor(coverage$true_alpha)
coverage$simulator <- as.factor(coverage$simulator)
coverage$gene <- as.factor(coverage$gene)
coverage["true_tmrca"] <- 0
coverage$true_tmrca[coverage$param == 1] <- 29
coverage$true_tmrca[coverage$param == 2 | coverage$param == 3] <- 45
coverage$true_tmrca[coverage$param == 4] <- 1000
coverage$true_tmrca <- as.factor(coverage$true_tmrca)

coverage["param_omega"] <- paste("par", coverage$param, ": ", coverage$true_omega, sep = "")
coverage$param_omega[coverage$param_omega == "par2: 0.09"] <- "par1: 0.09"
coverage$param_omega[coverage$param_omega == "par1: 0.39"] <- "par2: 0.39"
coverage$param_omega <- as.factor(coverage$param_omega)

coverage["param_alpha"] <- paste("par", coverage$param, ": ", coverage$true_alpha, sep = "")
coverage$param_alpha[coverage$param_alpha == "par2: 7.08"] <- "par1: 7.08"
coverage$param_alpha[coverage$param_alpha == "par1: 4.04"] <- "par2: 4.04"
coverage$param_alpha <- as.factor(coverage$param_alpha)




coverage_au <- subset(coverage, likelihood == "augmented")

#remove TMRCA = 1000
coverage_au <- subset(coverage_au, true_tmrca != 1000)

#change the order and names of parameter values




#plot coverage: omega
co_omega <- ggplot(coverage_au, aes(x = param_omega, y = omega_percentage,
                                 shape = simulator, colour = true_tmrca)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  geom_hline(yintercept = 100, linetype="dotted") +
  scale_shape_manual(name = "Tree type",
                     values = c(Coalescent.jl = 15,  `Estimated trees` = 4),
                     breaks = c("Coalescent.jl", "Estimated trees"),
                     labels = c("True trees", "Estimated trees")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  guides(color = guide_legend(order = 1, title = "TMRCA"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Coverage") +
  theme(text = element_text(size = 11), legend.position = "none")


#plot coverage: alpha ----
co_alpha <- ggplot(coverage_au, aes(x = param_alpha, y = alpha_percentage,
                                 shape = simulator, colour = true_tmrca)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 100, linetype="dotted") +
  scale_shape_manual(name = "Tree type",
                     values = c(Coalescent.jl = 15,  `Estimated trees` = 4),
                     breaks = c("Coalescent.jl", "Estimated trees"),
                     labels = c("True trees", "Estimated trees")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  guides(color = guide_legend(order = 1, title = "TMRCA"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Coverage") +
  theme(text = element_text(size = 11), legend.position = "none")

#co_alpha + co_omega



# Relative error ----
relative_error <- rbind(relative_error_ttr, relative_error_estr)
relative_error_au <- subset(relative_error, likelihood == "augmented")

relative_error_au$sample_size <- as.factor(relative_error_au$sample_size )
relative_error_au$true_omega <- as.factor(relative_error_au$true_omega)
relative_error_au$true_alpha <- round(relative_error_au$true_alpha, 2)
relative_error_au$true_alpha <- as.factor(relative_error_au$true_alpha)
relative_error_au$simulator <- as.factor(relative_error_au$simulator)
relative_error_au$gene <- as.factor(relative_error_au$gene)

relative_error_au["true_tmrca"] <- 0
relative_error_au$true_tmrca[relative_error_au$param == 1] <- 29
relative_error_au$true_tmrca[relative_error_au$param == 2 | relative_error_au$param == 3] <- 45
relative_error_au$true_tmrca[relative_error_au$param == 4] <- 1000
relative_error_au$true_tmrca <- as.factor(relative_error_au$true_tmrca)


relative_error_au["param_omega"] <- paste("par", relative_error_au$param, ": ",
                                          relative_error_au$true_omega, sep = "")
relative_error_au$param_omega[relative_error_au$param_omega == "par2: 0.09"] <- "par1: 0.09"
relative_error_au$param_omega[relative_error_au$param_omega == "par1: 0.39"] <- "par2: 0.39"
relative_error_au$param_omega <- as.factor(relative_error_au$param_omega)

relative_error_au["param_alpha"] <- paste("par", relative_error_au$param, ": ",
                                          relative_error_au$true_alpha, sep = "")
relative_error_au$param_alpha[relative_error_au$param_alpha == "par2: 7.08"] <- "par1: 7.08"
relative_error_au$param_alpha[relative_error_au$param_alpha == "par1: 4.04"] <- "par2: 4.04"
relative_error_au$param_alpha <- as.factor(relative_error_au$param_alpha)


#remove param4 in with tmra = 1000
relative_error_au <- subset(relative_error_au, true_tmrca != 1000)

relerror_quant_au <- relative_error_au %>%
  group_by(sample_size, param, true_omega, true_alpha, likelihood,
           simulator, gene, param_alpha, param_omega, true_tmrca) %>%
  reframe(lower_omega = quantile(relative_error_omega, probs = 0.025),
          median_omega = quantile(relative_error_omega, probs = 0.5),
          upper_omega = quantile(relative_error_omega, probs = 0.975),
          lower_alpha = quantile(relative_error_alpha, probs = 0.025),
          median_alpha = quantile(relative_error_alpha, probs = 0.5),
          upper_alpha = quantile(relative_error_alpha, probs = 0.975))


#plot relative error quantiles for omega
rerr_omega <- ggplot(relerror_quant_au, aes(x = param_omega )) +
  geom_point(aes(y = median_omega, shape = simulator, colour = true_tmrca),
             size = 2, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = upper_omega, ymin = lower_omega, width = 0.4,
                    shape = simulator, colour = true_tmrca),
                position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  scale_shape_manual(name = "Tree type",
                     values = c(Coalescent.jl = 15,  `Estimated trees` = 4),
                     breaks = c("Coalescent.jl", "Estimated trees"),
                     labels = c("True trees", "Estimated trees")) +
  guides(color = guide_legend(order = 1, title = "Type"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Relative error") +
  theme(text = element_text(size = 11), legend.position = "none")



rerr_alpha <- ggplot(relerror_quant_au, aes(x = param_alpha )) +
  geom_point(aes(y = median_alpha, shape = simulator, colour = true_tmrca),
             size = 2, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_alpha, ymin = lower_alpha, width = 0.4,
                    shape = simulator, colour = true_tmrca),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  scale_shape_manual(name = "Tree type",
                     values = c(Coalescent.jl = 15,  `Estimated trees` = 4),
                     breaks = c("Coalescent.jl", "Estimated trees"),
                     labels = c("True trees", "Estimated trees")) +
  guides(color = guide_legend(order = 1, title = "Type"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Relative error") +
  theme(text = element_text(size = 11), legend.position = "none")

#rerr_alpha + rerr_omega




#make composite plot
quartz()
all_plots_legend <- p_alpha  + p_omega + co_alpha + co_omega +
  rerr_alpha + rerr_omega +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'A') +
  theme(legend.position =  "bottom")

all_plots <- p_alpha  + p_omega + co_alpha + co_omega +
  rerr_alpha + rerr_omega +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')



ggsave(filename = "all_plots_hiv_legend.pdf",plot = all_plots_legend,  width = 18,
       height = 20, units = "cm")

ggsave(filename = "all_plots_hiv.pdf",plot = all_plots,  width = 18, height = 20,
       units = "cm")





