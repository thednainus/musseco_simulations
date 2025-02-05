#plot the credible intervals for each replicate


library(ggplot2)
library(dplyr)


ci_julia <- readRDS("results/CIs/julia_fit_results.rds")
ci_tips <- readRDS("results/CIs/tips_fit_results.rds")

ci_julia$sample_size <- as.factor(ci_julia$sample_size )
rep_order1 <- as.character(sort(as.numeric(unique(ci_julia$rep))))
ci_julia$rep <- factor(ci_julia$rep, levels=rep_order1)
#ci_julia$true_omega <- as.factor(ci_julia$true_omega)
ci_julia$true_alpha <- round(ci_julia$true_alpha, 2)
#ci_julia$true_alpha <- as.factor(ci_julia$true_alpha)




ci_tips$sample_size <- as.factor(ci_tips$sample_size )
rep_order2 <- as.character(sort(as.numeric(unique(ci_tips$rep))))
ci_tips$rep <- factor(ci_tips$rep, levels=rep_order2)
#ci_tips$true_omega <- as.factor(ci_tips$true_omega)
ci_tips$true_alpha <- round(ci_tips$true_alpha, 2)
#ci_tips$true_alpha <- as.factor(ci_tips$true_alpha)


# Prepare the data to not plot the upper bound when it does not exist
ci_julia2 <- ci_julia %>%
  mutate(
    omega_upper_available = !is.infinite(omega_upper),
    omega_upper_for_plot = ifelse(omega_upper_available, omega_upper, omega),
    annotation_omega = ifelse(omega_upper_available, "", "*"),
    alpha_upper_available = !is.infinite(alpha_upper),
    alpha_upper_for_plot = ifelse(alpha_upper_available, alpha_upper, alpha),
    annotation_alpha = ifelse(alpha_upper_available, "", "*")
  )


ci_tips2 <- ci_tips %>%
  mutate(
    omega_upper_available = !is.infinite(omega_upper),
    omega_upper_for_plot = ifelse(omega_upper_available, omega_upper, omega),
    annotation_omega = ifelse(omega_upper_available, "", "*"),
    alpha_upper_available = !is.infinite(alpha_upper),
    alpha_upper_for_plot = ifelse(alpha_upper_available, alpha_upper, alpha),
    annotation_alpha = ifelse(alpha_upper_available, "", "*")
  )


julia_ind1000_p1 <- subset(ci_julia2, param == 1 & sample_size == 500)
julia_ind1000_p2 <- subset(ci_julia2, param == 2 & sample_size == 500)
julia_ind1000_p3 <- subset(ci_julia2, param == 3 & sample_size == 500)
julia_ind1000_p4 <- subset(ci_julia2, param == 4 & sample_size == 500)


julia_ind1000_p1 <- subset(ci_julia2, param == 1 & sample_size == 1000)
julia_ind1000_p2 <- subset(ci_julia2, param == 2 & sample_size == 1000)
julia_ind1000_p3 <- subset(ci_julia2, param == 3 & sample_size == 1000)
julia_ind1000_p4 <- subset(ci_julia2, param == 4 & sample_size == 1000)


tips_ind1000_p1 <- subset(ci_tips2, param == 1 & sample_size == 500)
tips_ind1000_p2 <- subset(ci_tips2, param == 2 & sample_size == 500)
tips_ind1000_p3 <- subset(ci_tips2, param == 3 & sample_size == 500)
tips_ind1000_p4 <- subset(ci_tips2, param == 4 & sample_size == 500)


tips_ind1000_p1 <- subset(ci_tips2, param == 1 & sample_size == 1000)
tips_ind1000_p2 <- subset(ci_tips2, param == 2 & sample_size == 1000)
tips_ind1000_p3 <- subset(ci_tips2, param == 3 & sample_size == 1000)
tips_ind1000_p4 <- subset(ci_tips2, param == 4 & sample_size == 1000 & likelihood == "normal")

#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


params <- c(1:4)
ssizes <- c(500, 1000)

for(i in params){
  for(j in ssizes){

    #get the subseted_sample
    ssample <- subset(ci_julia2, param == i & sample_size == j)

    plot_alpha <- ggplot(ssample, aes(x = rep )) +
      geom_point(aes(y = alpha, colour = likelihood),
                 size = 2, position = position_dodge(width = 0.9)) +
      geom_errorbar(aes(ymax = alpha_upper_for_plot, ymin = alpha_lower,
                        width = ifelse(alpha_upper_available, 0.5, 0),
                        colour = likelihood),
                    position = position_dodge(width = 0.9)) +
      geom_hline(aes(yintercept = true_alpha), linetype="dotted") +
      scale_colour_manual(values = cbbPalette[c(2,4)],
                          name = "Likelihood",
                          breaks = c("normal", "augmented"),
                          labels = c("coalescent", "augmented")) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme_bw() +
      xlab("Replicate number") +
      ylab("Alpha credible interval") +
      theme(text = element_text(size = 20), legend.position = "bottom")

    plot_omega <- ggplot(ssample, aes(x = rep )) +
      geom_point(aes(y = omega, colour = likelihood),
                 size = 2,position = position_dodge(width = 0.9)) +
      geom_errorbar(aes(ymax = omega_upper_for_plot, ymin = omega_lower,
                        width = ifelse(omega_upper_available, 0.5, 0),
                        colour = likelihood),
                    position = position_dodge(width = 0.9)) +
      geom_hline(aes(yintercept = true_omega), linetype="dotted") +
      scale_colour_manual(values = cbbPalette[c(2,4)],
                          name = "Likelihood",
                          breaks = c("normal", "augmented"),
                          labels = c("coalescent", "augmented")) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme_bw() +
      xlab("Replicate number") +
      ylab("Omega credible interval") +
      theme(text = element_text(size = 20), legend.position = "bottom")

    filename1 <- paste("julia", "alpha", "param", i, "sample_size", j, sep = "_")
    filename1 <- paste(filename1, ".pdf", sep = "")

    ggsave(filename = filename1, plot = plot_alpha, width = 17, height = 8)


    filename2 <- paste("julia", "omega", "param", i, "sample_size", j, sep = "_")
    filename2 <- paste(filename2, ".pdf", sep = "")

    ggsave(filename = filename2, plot = plot_omega, width = 17, height = 8)

  }
}

quartz()
ggplot(tips_ind1000_p4, aes(x = rep )) +
  geom_point(aes(y = alpha, colour = likelihood, alpha = ifelse(alpha_upper_available, 1, 0.5)),
             size = 2) +
  geom_errorbar(aes(ymax = alpha_upper_for_plot, ymin = alpha_lower,
                    width = ifelse(alpha_upper_available, 0.5, 0),
                    alpha = ifelse(alpha_upper_available, 1, 0.5),
                    colour = likelihood)) +
  scale_alpha_identity() +
  geom_text(aes(label = annotation_alpha, y = alpha_upper_for_plot),
            vjust = -0.5, color = "black", size = 5) +
  geom_hline(aes(yintercept = true_alpha), linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  xlab("Replicate number") +
  ylab("Alpha credible interval") +
  theme(text = element_text(size = 20), legend.position = "bottom")



ggplot(tips_ind1000_p4, aes(x = rep )) +
  geom_point(aes(y = alpha, colour = likelihood),
             size = 2,position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = alpha_upper, ymin = alpha_lower,
                    width = 0.5,
                    colour = likelihood),
                position = position_dodge(width = 0.9)) +
  geom_hline(aes(yintercept = true_alpha), linetype="dotted") +
  scale_colour_manual(values = cbbPalette[c(2,4)],
                      name = "Likelihood",
                      breaks = c("normal", "augmented"),
                      labels = c("coalescent", "augmented")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  xlab("Replicate number") +
  ylab("Alpha credible interval") +
  theme(text = element_text(size = 20), legend.position = "bottom")

ggsave(filename = "teste.pdf", width = 17, height = 8)






quartz()
# Create the plot
ggplot(data2, aes(x = x, y = y)) +
  geom_point(size = 3) +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper_for_plot,
    width = ifelse(upper_available, 0.2, 0)
  )) +
  labs(title = "Error Bar Plot with Conditional Upper Bounds", y = "Value") +
  theme_minimal()
