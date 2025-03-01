# plot true values vs estimated values

library(ggplot2); theme_set(theme_bw(base_family ="Helvetica"))
library(dplyr)
library(ggforce)
library(patchwork)

#get the data


#get CIs for trees simulated with Coalescent.jl ----
cis_julia <- readRDS("results/CIs/julia_fit_results.rds")
cis_julia["simulator"] <- "Coalescent.jl"
cis_julia["pa"] <- ifelse(cis_julia$param == 1 | cis_julia$param == 2,
                          0.85, 0.95)
cis_julia["average_r0"] <- "R[0]  %~~% 1.0"
cis_julia["simulator_pa_R0"] <- paste(paste(cis_julia$simulator, paste(" pa", cis_julia$pa, sep = " = "), sep = ":"), cis_julia$average_r0, sep = " | ")
cis_julia$simulator_pa_R0 <- as.factor(cis_julia$simulator_pa_R0)

cis_julia$true_alpha <- round(cis_julia$true_alpha,2)
cis_julia$true_alpha <- as.factor(cis_julia$true_alpha)
cis_julia$true_omega <- as.factor(cis_julia$true_omega)


cis_julia_1000 <- subset(cis_julia, sample_size == 1000 & likelihood == "augmented")
cis_julia_500 <- subset(cis_julia, sample_size == 500 & likelihood == "augmented")

add_true_values <- cis_julia_1000[c(4,5,12,16)]
add_true_values <-  distinct(add_true_values, .keep_all=TRUE)
add_true_values["col_var"] <- c("val1", "val1", "val2", "val2")


# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#plot scatter plot for omega
quartz()

p_omega <- ggplot(cis_julia_1000, aes(x = true_omega, y = omega,
                           shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_1000$simulator_pa_R0),
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
  ylab("Estimated value") +
  theme(text = element_text(size = 12), legend.position = "none")



p_alpha <- ggplot(cis_julia_1000, aes(x = true_alpha, y = alpha,
                                      shape = likelihood,
                                      colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_1000$simulator_pa_R0),
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
  ylab("Estimated value") +
  theme(text = element_text(size = 12), legend.position = "none")



#500 tips
p_omega500 <- ggplot(cis_julia_500, aes(x = true_omega, y = omega,
                                      shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_1000$simulator_pa_R0),
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
  ylab("Estimated value") +
  theme(text = element_text(size = 12), legend.position = "none")



p_alpha500 <- ggplot(cis_julia_500, aes(x = true_alpha, y = alpha,
                                      shape = likelihood,
                                      colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_1000$simulator_pa_R0),
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
  ylab("Estimated value") +
  theme(text = element_text(size = 12), legend.position = "none")


