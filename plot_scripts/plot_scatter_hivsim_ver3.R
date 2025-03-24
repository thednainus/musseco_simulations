# plot true values vs estimated values
# for the true trees

library(ggplot2); theme_set(theme_bw(base_family ="Helvetica"))
library(dplyr)
library(ggforce)
library(patchwork)

#get the data


#get CIs for trees simulated with Coalescent.jl ----
cis_julia <- readRDS("results/CIs/hivsim_fit_results_true_trees.rds")
cis_julia["simulator"] <- "Coalescent.jl"
cis_julia$true_alpha <- round(cis_julia$true_alpha,2)
cis_julia$true_alpha <- as.factor(cis_julia$true_alpha)
cis_julia$true_omega <- as.factor(cis_julia$true_omega)
cis_julia["gene"] <- ifelse(cis_julia$param == 2, "env", "V3")


#get CIs for trees estimated timetrees ----
cis_julia_estr <- readRDS("results/CIs/hivsim_fit_results_treedater.rds")
cis_julia_estr["simulator"] <- "Estimated trees"
cis_julia_estr$true_alpha <- round(cis_julia_estr$true_alpha,2)
cis_julia_estr$true_alpha <- as.factor(cis_julia_estr$true_alpha)
cis_julia_estr$true_omega <- as.factor(cis_julia_estr$true_omega)
cis_julia_estr["gene"] <- ifelse(cis_julia_estr$param == 2, "env", "V3")




cis_julia_au <- subset(cis_julia, likelihood == "augmented")

all_cis <- rbind(cis_julia_au, cis_julia_estr)
all_cis$simulator <- as.factor(all_cis$simulator)
all_cis$gene <- as.factor(all_cis$gene)
all_cis$likelihood <- as.factor(all_cis$likelihood)
all_cis$true_tmrca <- as.factor(all_cis$true_tmrca)

all_cis["param_omega"] <- paste("par", all_cis$param, ": ", all_cis$true_omega, sep = "")
all_cis$param_omega[all_cis$param_omega == "par2: 0.09"] <- "par1: 0.09"
all_cis$param_omega[all_cis$param_omega == "par1: 0.39"] <- "par2: 0.39"
all_cis$param_omega <- as.factor(all_cis$param_omega)
all_cis["param_alpha"] <- paste("par", all_cis$param, ": ", all_cis$true_alpha, sep = "")
all_cis$param_alpha[all_cis$param_alpha == "par2: 7.08"] <- "par1: 7.08"
all_cis$param_alpha[all_cis$param_alpha == "par1: 4.04"] <- "par2: 4.04"
all_cis$param_alpha <- as.factor(all_cis$param_alpha)


#for these plots remove the TMRCA = 1000
all_cis <- subset(all_cis, true_tmrca != 1000)

add_true_values <- all_cis[c(4,5,6, 14:17)]
add_true_values <-  distinct(add_true_values, .keep_all=TRUE)

#all_cis_true <- subset(all_cis, simulator == "Coalescent.jl")
add_true_values2 <- add_true_values[1:3,]

# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plot scatter plot for omega
#quartz()

p_omega <- ggplot(all_cis, aes(x = param_omega, y = omega,
                           shape = simulator, colour = true_tmrca)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.8, alpha = 0.7,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values2, aes(x = param_omega, y = as.numeric(as.character(true_omega))),
            size = 1.6, shape = 18, alpha = 0.8, colour = "black") +
  scale_shape_manual(name = "Tree type",
                     values = c(Coalescent.jl = 15,  `Estimated trees` = 4),  # Add this line
                     breaks = c("Coalescent.jl", "Estimated trees"),
                     labels = c("True trees", "Estimated trees")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +

  theme_bw() +
  xlab("True omega value") +
  ylab("Estimated value") +
  theme(text = element_text(size = 11), legend.position = "none")


#quartz()
p_alpha <- ggplot(all_cis, aes(x = param_alpha, y = alpha,
                               colour = true_tmrca, shape = simulator)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.8, alpha = 0.7,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values2, aes(x = param_alpha, y = as.numeric(as.character(true_alpha))),
            size = 1.6, shape = 18, alpha = 0.8, colour = "black") +
  scale_shape_manual(name = "Tree type",
                     values = c(Coalescent.jl = 15,  `Estimated trees` = 4),  # Add this line
                     breaks = c("Coalescent.jl", "Estimated trees"),
                     labels = c("True trees", "Estimated trees")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  guides(color = guide_legend(order = 1, title = "TMRCA"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Estimated value") +
  theme(text = element_text(size = 11), legend.position = "none")


#quartz()
#p_alpha + p_omega

