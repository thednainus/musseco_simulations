# plot true values vs estimated values
# for the true trees

library(ggplot2); theme_set(theme_bw(base_family ="Helvetica"))
library(dplyr)
library(ggforce)
library(patchwork)

#get the data


#get results  ----
results <- readRDS("results/hivsim/hivsim_results_treedater_tmrca.rds")
results["simulator"] <- "Estimated trees"
results["gene"] <- ifelse(results$param == 2, "env", "V3")
results$true_pa <- as.factor(results$true_pa)
results$true_tmrca <- as.factor(results$true_tmrca)
results$true_rate <- as.factor(results$true_rate)
results$simulator <- as.factor(results$simulator)
results$gene <- as.factor(results$gene)

results["param_tmrca"] <- paste("par", results$param, ": ", results$true_tmrca, sep = "")
results$param_tmrca[results$param_tmrca == "par2: 45"] <- "par1: 45"
results$param_tmrca[results$param_tmrca == "par1: 29"] <- "par2: 29"
results$param_tmrca <- as.factor(results$param_tmrca)

results["param_rate"] <- paste("par", results$param, ": ", results$true_rate, sep = "")
results$param_rate[results$param_rate == "par2: 0.0032"] <- "par1: 0.0032"
results$param_rate[results$param_rate == "par1: 0.0036"] <- "par2: 0.0036"
results$param_rate <- as.factor(results$param_rate)



add_true_values <- results[c(4:6,11:14)]
add_true_values <-  distinct(add_true_values, .keep_all=TRUE)



# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plot scatter plot for omega
quartz()

p_tmrca <- ggplot(results, aes(x = param_tmrca, y = tmrca, colour = gene)) +
  geom_sina(position = position_dodge(width = 0.8), size = 1.5, alpha = 0.6,
            maxwidth = 0.8, stroke = 0.8) +
  geom_sina(data = add_true_values, aes(x = param_tmrca, y = as.numeric(as.character(true_tmrca))),
            size = 3.5, shape = 18, colour = "black") +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  guides(color = guide_legend(order = 1, title = "Type")) +
  theme_bw() +
  xlab("True TMRCA (years)") +
  ylab("Estimated TMRCA (years)") +
  theme(text = element_text(size = 11), legend.position = "none")


p_rate <- ggplot(results, aes(x = param_rate, y = rate, colour = gene)) +
  geom_sina(position = position_dodge(width = 0.8), size = 1.5, alpha = 0.8,
            maxwidth = 0.8, stroke = 0.8) +
  geom_sina(data = add_true_values, aes(x = param_rate, y = as.numeric(as.character(true_rate))),
            size = 3.5, shape = 18, colour = "black", alpha = 0.8) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9")) +
  guides(color = guide_legend(order = 1, title = "Type")) +
  theme_bw() +
  xlab("True mean rate (subst./site)") +
  ylab("Estimated mean rate (subst./site)") +
  theme(text = element_text(size = 11), legend.position = "none")



#quartz()
all_rates <- p_tmrca + p_rate

ggsave(filename = "rates_tmrca_plots.pdf",
       plot = all_rates,  width = 18, height = 14, units = "cm")


