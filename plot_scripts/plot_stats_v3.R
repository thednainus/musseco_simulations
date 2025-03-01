# plot coverage, precision, relative error and proportion of inf
# proportion of Inf is those replicates in which the upper bound was not possible
# to estimate


library(data.table)
library(ggplot2); theme_set(theme_bw(base_family ="Helvetica"))
library(dplyr)
library(stringr)
library(reshape2)
library(scales)
library(patchwork)

#source script to get the scatter plots
source("plot_scripts/plot_sina_v2.R")

#get the data


#stats for trees simulated with Coalescent.jl ----

coverage_julia <- readRDS("results/julia_coverage.rds")
coverage_julia["simulator"] <- "Coalescent.jl"
relative_error_julia <- readRDS("results/julia_relative_error.rds")
relative_error_julia["simulator"] <- "Coalescent.jl"
prop_inf_julia <- readRDS("results/julia_prop_inf.rds")
prop_inf_julia["simulator"] <- "Coalescent.jl"


#stats for trees simulated with TiPS ----

coverage_tips <- readRDS("results/tips_coverage.rds")
coverage_tips["simulator"] <- "TiPS"
relative_error_tips <- readRDS("results/tips_relative_error.rds")
relative_error_tips["simulator"] <- "TiPS"
prop_inf_tips <- readRDS("results/tips_prop_inf.rds")
prop_inf_tips["simulator"] <- "TiPS"


#stats for trees simulated with diversitree ----

coverage_dst <- readRDS("results/diversitree_coverage.rds")
coverage_dst["simulator"] <- "diversitree"

#get values as percentages instead of proportion
coverage_dst["average_r0"] <- ifelse(coverage_dst$param == 1 | coverage_dst$param == 2,
                                     "R[0] %~~% 1.85", "R[0] %~~% 1.22")
coverage_dst$average_r0 <- as.factor(coverage_dst$average_r0)
coverage_dst$pa <- as.factor(coverage_dst$pa)




relative_error_dst <- readRDS("results/diversitree_relative_error.rds")
relative_error_dst["simulator"] <- "diversitree"
relative_error_dst["average_r0"] <- ifelse(relative_error_dst$param == 1 | relative_error_dst$param == 2,
                                           "R[0] %~~% 1.85", "R[0] %~~% 1.22")
relative_error_dst$average_r0 <- as.factor(relative_error_dst$average_r0)
relative_error_dst$pa <- as.factor(relative_error_dst$pa)


prop_inf_dst <- readRDS("results/diversitree_prop_inf.rds")
prop_inf_dst["simulator"] <- "diversitree"
prop_inf_dst["average_r0"] <- ifelse(prop_inf_dst$param == 1 | prop_inf_dst$param == 2,
                                     "R[0] %~~% 1.85", "R[0] %~~% 1.22")
prop_inf_dst$average_r0 <- as.factor(prop_inf_dst$average_r0)
prop_inf_dst$pa <- as.factor(prop_inf_dst$pa)



# stats for combined julia and tips data ----
#also add R0 value for the Coalescent.jl and TiPS to plot wverything together
#with the diversitree analyses.
coverage <- rbind(coverage_julia, coverage_tips)
coverage["pa"] <- ifelse(coverage$param == 1 | coverage$param == 2,
                         0.85, 0.95)
coverage["average_r0"] <- "R[0] %~~% 1.0"
relative_error <- rbind(relative_error_julia, relative_error_tips)
relative_error["pa"] <- ifelse(relative_error$param == 1 | relative_error$param == 2,
                               0.85, 0.95)
relative_error["average_r0"] <- "R[0] %~~% 1.0"
prop_inf <- rbind(prop_inf_julia, prop_inf_tips)
prop_inf["pa"] <- ifelse(prop_inf$param == 1 | prop_inf$param == 2,
                         0.85, 0.95)
prop_inf["average_r0"] <- "R[0] %~~% 1.0"






#merge all data now with diversitree analyses
coverage_all <- rbind(coverage, coverage_dst[c(1:5, 7:11)])
relative_error_all <- rbind(relative_error, relative_error_dst[c(1:5, 7:11)])
prop_inf_all <- rbind(prop_inf, prop_inf_dst[c(1:5, 7:11)])




# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plots for combined Julia, TiPS, and diversitree data

#plots for coverage ----
#get values as percentages instead of proportion
coverage_all["omega_percentage"] <- coverage_all$coverage_omega * 100
coverage_all["alpha_percentage"] <- coverage_all$coverage_alpha * 100
coverage_all$sample_size <- as.factor(coverage_all$sample_size)
coverage_all$true_omega <- as.factor(coverage_all$true_omega)
coverage_all$true_alpha <- round(coverage_all$true_alpha, 2)
coverage_all$true_alpha <- as.factor(coverage_all$true_alpha)
coverage_all$pa <- as.factor(coverage_all$pa)
coverage_all$simulator <- as.factor(coverage_all$simulator)
coverage_all$simulator <- factor(coverage_all$simulator,
                                 levels=c("Coalescent.jl", "TiPS", "diversitree"))

#merge simulator with R_0 to create a single plot
coverage_all["simulator_pa_R0"] <- paste(paste(coverage_all$simulator, paste(" pa", coverage_all$pa, sep = " = "), sep = ":"), coverage_all$average_r0, sep = " | ")
coverage_all$simulator_pa_R0 <- factor(coverage_all$simulator_pa_R0,
                                    levels=c("Coalescent.jl: pa = 0.85 | R[0] %~~% 1.0",
                                             "Coalescent.jl: pa = 0.95 | R[0] %~~% 1.0",
                                             "TiPS: pa = 0.85 | R[0] %~~% 1.0",
                                             "TiPS: pa = 0.95 | R[0] %~~% 1.0",
                                             "diversitree: pa = 0.85 | R[0] %~~% 1.85",
                                             "diversitree: pa = 0.95 | R[0] %~~% 1.22"))


coverage_all_1000 <- subset(coverage_all, sample_size == 1000)
coverage_all_500 <- subset(coverage_all, sample_size == 500)


#plot coverage: omega; sample size = 1000 tips ----
#quartz()
co1000_omega <- ggplot(coverage_all_1000, aes(x = true_omega, y = omega_percentage,
                              shape = likelihood, colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  geom_hline(yintercept = 100, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(coverage_all_1000$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Coverage") +
  theme(text = element_text(size = 12), legend.position = "none")


#plot coverage: alpha; sample size = 1000 tips ----
co1000_alpha <- ggplot(coverage_all_1000, aes(x = true_alpha, y = alpha_percentage,
                              shape = likelihood, colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 100, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(coverage_all_1000$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Coverage") +
  theme(text = element_text(size = 12), legend.position = "none")

#co1000_alpha + co1000_omega


#plot coverage: omega; sample size = 500 tips ----
co500_omega <- ggplot(coverage_all_500, aes(x = true_omega, y = omega_percentage,
                                              shape = likelihood,
                                            colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  geom_hline(yintercept = 100, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(coverage_all_500$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Coverage") +
  theme(text = element_text(size = 12), legend.position = "none")


#plot coverage: alpha; sample size = 500 tips ----
co500_alpha <- ggplot(coverage_all_500, aes(x = true_alpha, y = alpha_percentage,
                                              shape = likelihood,
                                            colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 100, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(coverage_all_500$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Coverage") +
  theme(text = element_text(size = 12), legend.position = "none")

#co500_alpha + co500_omega



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



#merge simulator with R_0 to create a single plot
prop_inf_all["simulator_pa_R0"] <- paste(paste(prop_inf_all$simulator, paste(" pa", prop_inf_all$pa, sep = " = "), sep = ":"), prop_inf_all$average_r0, sep = " | ")
prop_inf_all$simulator_pa_R0 <- factor(prop_inf_all$simulator_pa_R0,
                                       levels=c("Coalescent.jl: pa = 0.85 | R[0] %~~% 1.0",
                                                "Coalescent.jl: pa = 0.95 | R[0] %~~% 1.0",
                                                "TiPS: pa = 0.85 | R[0] %~~% 1.0",
                                                "TiPS: pa = 0.95 | R[0] %~~% 1.0",
                                                "diversitree: pa = 0.85 | R[0] %~~% 1.85",
                                                "diversitree: pa = 0.95 | R[0] %~~% 1.22"))


prop_inf_all_1000 <- subset(prop_inf_all, sample_size == 1000)
prop_inf_all_500 <- subset(prop_inf_all, sample_size == 500)

#plot proportion of replicates we could not estimate the upper bound: omega
#1000 tips
#quartz()
noup1000_omega <- ggplot(prop_inf_all_1000, aes(x = true_omega, y = omega_percentage,
                                              shape = likelihood, colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(prop_inf_all_1000$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 12), legend.position = "none")


#plot prop inf: alpha
#1000tips
noup1000_alpha <- ggplot(prop_inf_all_1000, aes(x = true_alpha, y = alpha_percentage,
                                              shape = likelihood, colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(prop_inf_all_1000$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 12), legend.position = "none")

#noup1000_alpha + noup1000_omega

#plot omega 500tips
noup500_omega <- ggplot(prop_inf_all_500, aes(x = true_omega, y = omega_percentage,
                                                shape = likelihood, colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(prop_inf_all_500$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 12), legend.position = "none")


#plot prop inf: alpha
#500 tips
noup500_alpha <- ggplot(prop_inf_all_500, aes(x = true_alpha, y = alpha_percentage,
                                                shape = likelihood, colour = simulator_pa_R0)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(prop_inf_all_500$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("% replicates without upper bound") +
  theme(text = element_text(size = 12), legend.position = "none")

#noup500_alpha + noup500_omega




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



#merge simulator with R_0 to create a single plot
relerror_quant["simulator_pa_R0"] <- paste(paste(relerror_quant$simulator, paste(" pa", relerror_quant$pa, sep = " = "), sep = ":"), relerror_quant$average_r0, sep = " | ")
relerror_quant$simulator_pa_R0 <- factor(relerror_quant$simulator_pa_R0,
                                       levels=c("Coalescent.jl: pa = 0.85 | R[0] %~~% 1.0",
                                                "Coalescent.jl: pa = 0.95 | R[0] %~~% 1.0",
                                                "TiPS: pa = 0.85 | R[0] %~~% 1.0",
                                                "TiPS: pa = 0.95 | R[0] %~~% 1.0",
                                                "diversitree: pa = 0.85 | R[0] %~~% 1.85",
                                                "diversitree: pa = 0.95 | R[0] %~~% 1.22"))



relerror_quant_1000 <- subset(relerror_quant, sample_size == 1000)
relerror_quant_500 <- subset(relerror_quant, sample_size == 500)


#for omega because log of zero is undefined
#we had 1 observation for the lower bound of omega = 0.
#to pplot the relative error at log scale we added a small constant to the value
#of omega = 0
relerror_quant_500$lower_omega[relerror_quant_500$lower_omega == 0] <- 0.0001


#plot relative error quantiles for omega
#1000tips
#quartz()
rerr_omega_1000 <- ggplot(relerror_quant_1000, aes(x = true_omega )) +
  geom_point(aes(y = median_omega, shape = likelihood, colour = simulator_pa_R0),
             size = 2, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = upper_omega, ymin = lower_omega, width = 0.4,
                    shape = likelihood, colour = simulator_pa_R0),
                position = position_dodge(width = 0.8)) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 0.0001, linetype="dotted") + #0.0001 was added here instead of 0 because log of 0 is undefined.
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(relerror_quant_1000$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Relative error") +
  theme(text = element_text(size = 12), legend.position = "none")



rerr_alpha_1000 <- ggplot(relerror_quant_1000, aes(x = true_alpha )) +
  geom_point(aes(y = median_alpha, shape = likelihood, colour = simulator_pa_R0),
             size = 2, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_alpha, ymin = lower_alpha, width = 0.4,
                    shape = likelihood, colour = simulator_pa_R0),
                position = position_dodge(width = 0.7)) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 0.0001, linetype="dotted") + #0.00001 was added here instead of 0 because log of 0 is undefined.
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(relerror_quant_1000$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Relative error") +
  theme(text = element_text(size = 12), legend.position = "none")

#rerr_alpha_1000 + rerr_omega_1000



#quartz()
rerr_omega_500 <- ggplot(relerror_quant_500, aes(x = true_omega )) +
  geom_point(aes(y = median_omega, shape = likelihood, colour = simulator_pa_R0),
             size = 2, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymax = upper_omega, ymin = lower_omega, width = 0.4,
                    shape = likelihood, colour = simulator_pa_R0),
                position = position_dodge(width = 0.8)) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 0.0001, linetype="dotted") + #0.00001 was added here instead of 0 because log of 0 is undefined.
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(relerror_quant_500$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True omega value") +
  ylab("Relative error") +
  theme(text = element_text(size = 12), legend.position = "none")



rerr_alpha_500 <- ggplot(relerror_quant_500, aes(x = true_alpha )) +
  geom_point(aes(y = median_alpha, shape = likelihood, colour = simulator_pa_R0),
             size = 2, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper_alpha, ymin = lower_alpha, width = 0.4,
                    shape = likelihood, colour = simulator_pa_R0),
                position = position_dodge(width = 0.7)) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 0.0001, linetype="dotted") + #0.00001 was added here instead of 0 because log of 0 is undefined.
  scale_colour_manual(
    values = cbbPalette[c(2,3,4,6,7,8)],
    breaks = levels(relerror_quant_500$simulator_pa_R0),
    labels = function(x) {
      parsed_labels <- sapply(x, function(label) {
        parts <- strsplit(label, ": ")[[1]]
        prefix <- parts[1]
        parts2 <- strsplit(parts[2], "\\|")[[1]]
        pa_value <- trimws(parts2[1])
        r_value <- sub("R\\[0\\]\\s*%~~%\\s*(\\d+\\.?\\d*)", "\\1", trimws(parts2[2]))
        paste0(prefix, ": ", pa_value, " * ' | ' * R[0] %~~% ", r_value)
      })
      parse(text = gsub("pa = ", "pa == ", parsed_labels))
    }
  ) +
  scale_shape_manual(name = "Likelihood",
                     values = c(normal = 15, augmented = 16),  # Add this line
                     breaks = c("normal", "augmented"),
                     labels = c("Coalescent", "Augmented")) +
  guides(color = guide_legend(order = 1, title = "Simulator"),
         shape = guide_legend(order = 2)) +
  theme_bw() +
  xlab("True alpha value") +
  ylab("Relative error") +
  theme(text = element_text(size = 12), legend.position = "none")

#rerr_alpha_500 + rerr_omega_500

#make composite plot for 1000 individuals
quartz()
all_plots <- p_alpha  + p_omega + co1000_alpha + co1000_omega +
  rerr_alpha_1000 + rerr_omega_1000 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'A') +
  theme(legend.position = "none")

all_plots_legend <- p_alpha  + p_omega + co1000_alpha + co1000_omega +
  rerr_alpha_1000 + rerr_omega_1000 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'A') +
  theme(legend.position = "right")

ggsave(filename = "all_plots_1000tips.pdf",plot = all_plots,  width = 18, height = 20, units = "cm")
ggsave(filename = "all_plots_1000tips_legend.pdf",plot = all_plots_legend,  width = 18, height = 20, units = "cm")


#for 500 individuals
all_plots500ind <- p_alpha500  + p_omega500 + co500_alpha + co500_omega +
  rerr_alpha_500 + rerr_omega_500 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'A') +
  theme(legend.position = "none")



ggsave(filename = "all_plots_500tips.pdf",plot = all_plots500ind,  width = 18, height = 20, units = "cm")

