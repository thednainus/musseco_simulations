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


cis_julia_1000_co <- subset(cis_julia, sample_size == 1000 & likelihood == "normal")
cis_julia_500_co <- subset(cis_julia, sample_size == 500 & likelihood == "normal")
cis_julia_500_au <- subset(cis_julia, sample_size == 500 & likelihood == "augmented")



#get CIs for trees simulated with TiPS ----
cis_tips <- readRDS("results/CIs/tips_fit_results.rds")
cis_tips["simulator"] <- "TiPS"
cis_tips["pa"] <- ifelse(cis_tips$param == 1 | cis_tips$param == 2,
                         0.85, 0.95)
cis_tips["average_r0"] <- "R[0]  %~~% 1.0"
cis_tips["simulator_pa_R0"] <- paste(paste(cis_tips$simulator,
                                           paste(" pa", cis_tips$pa, sep = " = "),
                                           sep = ":"), cis_tips$average_r0, sep = " | ")
cis_tips$simulator_pa_R0 <- as.factor(cis_tips$simulator_pa_R0)

cis_tips$true_alpha <- round(cis_tips$true_alpha,2)
cis_tips$true_alpha <- as.factor(cis_tips$true_alpha)
cis_tips$true_omega <- as.factor(cis_tips$true_omega)


cis_tips_1000_co <- subset(cis_tips, sample_size == 1000 & likelihood == "normal")
cis_tips_500_co <- subset(cis_tips, sample_size == 500 & likelihood == "normal")

cis_tips_1000_au <- subset(cis_tips, sample_size == 1000 & likelihood == "augmented")
cis_tips_500_au <- subset(cis_tips, sample_size == 500 & likelihood == "augmented")


#get CIs for trees simulated with diversitree  ----
cis_dst <- readRDS("results/CIs/diversitree_fit_results.rds")
cis_dst["simulator"] <- "diversitree"
cis_dst["average_r0"] <- ifelse(cis_dst$param == 1 | cis_dst$param == 2,
                                "R[0] %~~% 1.85", "R[0] %~~% 1.22")
cis_dst["simulator_pa_R0"] <- paste(paste(cis_dst$simulator,
                                          paste(" pa", cis_dst$pa, sep = " = "),
                                          sep = ":"), cis_dst$average_r0, sep = " | ")
cis_dst$simulator_pa_R0 <- as.factor(cis_dst$simulator_pa_R0)

cis_dst$true_alpha <- round(cis_dst$true_alpha,2)
cis_dst$true_alpha <- as.factor(cis_dst$true_alpha)
cis_dst$true_omega <- as.factor(cis_dst$true_omega)


cis_dst_1000_co <- subset(cis_dst, sample_size == 1000 & likelihood == "normal")
cis_dst_500_co <- subset(cis_dst, sample_size == 500 & likelihood == "normal")

cis_dst_1000_au <- subset(cis_dst, sample_size == 1000 & likelihood == "augmented")
cis_dst_500_au <- subset(cis_dst, sample_size == 500 & likelihood == "augmented")


#to add true values to plot
add_true_values_julia <- cis_julia_1000_co[c(4,5,12,16)]
add_true_values_julia <-  distinct(add_true_values_julia, .keep_all=TRUE)

add_true_values_tips <- cis_tips_1000_co[c(4,5,12,16)]
add_true_values_tips <-  distinct(add_true_values_tips, .keep_all=TRUE)

add_true_values_dst <- cis_dst_1000_co[c(6,7,14,17)]
add_true_values_dst <-  distinct(add_true_values_dst, .keep_all=TRUE)



# colour palette ----
#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



#sina plots for omega for Coalescent.jl for 1000 tips ----
#quartz()

p_omega_jl_co1000 <- ggplot(cis_julia_1000_co, aes(x = true_omega, y = omega,
                           shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_julia, aes(x = true_omega,
                                              y = as.numeric(as.character(true_omega))),
            size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_1000_co$simulator_pa_R0),
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



p_alpha_jl_co1000 <- ggplot(cis_julia_1000_co, aes(x = true_alpha, y = alpha,
                                      shape = likelihood,
                                      colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_julia, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_1000_co$simulator_pa_R0),
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



#sina plots for omega for Coalescent.jl for 500 tips ----
p_omega_jl_co500 <- ggplot(cis_julia_500_co, aes(x = true_omega, y = omega,
                                      shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_julia, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_500_co$simulator_pa_R0),
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



p_alpha_jl_co500 <- ggplot(cis_julia_500_co, aes(x = true_alpha, y = alpha,
                                      shape = likelihood,
                                      colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_julia, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(2,3)],
    breaks = levels(cis_julia_500_co$simulator_pa_R0),
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


#create composite with sina plots
quartz()
co_julia_1000 <- p_alpha_jl_co1000 + p_omega_jl_co1000 +
  plot_annotation(tag_levels = 'A')

co_julia_500 <- p_alpha_jl_co500 + p_omega_jl_co500 +
  plot_annotation(tag_levels = 'A')



ggsave(filename = "sina_plots_coalescentjl_coalescent1000tips.pdf",
       plot = co_julia_1000,  width = 18, height = 7, units = "cm")

ggsave(filename = "sina_plots_coalescentjl_coalescent500tips.pdf",
       plot = co_julia_500,  width = 18, height = 7, units = "cm")





#sina plots for omega for TiPS for 1000 tips ----
p_omega_tips_co1000 <- ggplot(cis_tips_1000_co, aes(x = true_omega, y = omega,
                                                    shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_co$simulator_pa_R0),
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



p_alpha_tips_co1000 <- ggplot(cis_tips_1000_co, aes(x = true_alpha, y = alpha,
                                                    shape = likelihood,
                                                    colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_co$simulator_pa_R0),
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



p_omega_tips_au1000 <- ggplot(cis_tips_1000_au, aes(x = true_omega, y = omega,
                                                    shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_au$simulator_pa_R0),
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



p_alpha_tips_au1000 <- ggplot(cis_tips_1000_au, aes(x = true_alpha, y = alpha,
                                                    shape = likelihood,
                                                    colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_au$simulator_pa_R0),
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



#sina plots for omega for TiPS for 500 tips ----
p_omega_tips_co500 <- ggplot(cis_tips_500_co, aes(x = true_omega, y = omega,
                                                  shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_co$simulator_pa_R0),
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



p_alpha_tips_co500 <- ggplot(cis_tips_500_co, aes(x = true_alpha, y = alpha,
                                                  shape = likelihood,
                                                  colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_co$simulator_pa_R0),
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



p_omega_tips_au500 <- ggplot(cis_tips_500_au, aes(x = true_omega, y = omega,
                                                  shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_au$simulator_pa_R0),
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



p_alpha_tips_au500 <- ggplot(cis_tips_500_au, aes(x = true_alpha, y = alpha,
                                                  shape = likelihood,
                                                  colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_au$simulator_pa_R0),
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


#create composite with sina plots
quartz()
co_tips_1000 <- p_alpha_tips_co1000 + p_omega_tips_co1000 +
  p_alpha_tips_au1000 + p_omega_tips_au1000 +
  plot_annotation(tag_levels = 'A')

co_tips_500 <- p_alpha_tips_co500 + p_omega_tips_co500 +
  p_alpha_tips_au500 + p_omega_tips_au500
  plot_annotation(tag_levels = 'A')




ggsave(filename = "sina_plots_tips_1000tips.pdf",
       plot = co_tips_1000,  width = 18, height = 14, units = "cm")

ggsave(filename = "sina_plots_tips_500tips.pdf",
       plot = co_tips_500,  width = 18, height = 14, units = "cm")



#sina plots for omega for TiPS for 1000 tips ----
p_omega_tips_co1000 <- ggplot(cis_tips_1000_co, aes(x = true_omega, y = omega,
                                                    shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_co$simulator_pa_R0),
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



p_alpha_tips_co1000 <- ggplot(cis_tips_1000_co, aes(x = true_alpha, y = alpha,
                                                    shape = likelihood,
                                                    colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_co$simulator_pa_R0),
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



p_omega_tips_au1000 <- ggplot(cis_tips_1000_au, aes(x = true_omega, y = omega,
                                                    shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_au$simulator_pa_R0),
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



p_alpha_tips_au1000 <- ggplot(cis_tips_1000_au, aes(x = true_alpha, y = alpha,
                                                    shape = likelihood,
                                                    colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_1000_au$simulator_pa_R0),
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



#sina plots for omega for TiPS for 500 tips ----
p_omega_tips_co500 <- ggplot(cis_tips_500_co, aes(x = true_omega, y = omega,
                                                  shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_co$simulator_pa_R0),
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



p_alpha_tips_co500 <- ggplot(cis_tips_500_co, aes(x = true_alpha, y = alpha,
                                                  shape = likelihood,
                                                  colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_co$simulator_pa_R0),
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



p_omega_tips_au500 <- ggplot(cis_tips_500_au, aes(x = true_omega, y = omega,
                                                  shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_au$simulator_pa_R0),
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



p_alpha_tips_au500 <- ggplot(cis_tips_500_au, aes(x = true_alpha, y = alpha,
                                                  shape = likelihood,
                                                  colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_tips, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(4,6)],
    breaks = levels(cis_tips_500_au$simulator_pa_R0),
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


#create composite with sina plots
quartz()
co_tips_1000 <- p_alpha_tips_co1000 + p_omega_tips_co1000 +
  p_alpha_tips_au1000 + p_omega_tips_au1000 +
  plot_annotation(tag_levels = 'A')

co_tips_500 <- p_alpha_tips_co500 + p_omega_tips_co500 +
  p_alpha_tips_au500 + p_omega_tips_au500 +
  plot_annotation(tag_levels = 'A')




ggsave(filename = "sina_plots_tips_1000tips.pdf",
       plot = co_tips_1000,  width = 18, height = 14, units = "cm")

ggsave(filename = "sina_plots_tips_500tips.pdf",
       plot = co_tips_500,  width = 18, height = 14, units = "cm")



#sina plots for omega for diversitree for 1000 tips ----
p_omega_dst_co1000 <- ggplot(cis_dst_1000_co, aes(x = true_omega, y = omega,
                                                  shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_1000_co$simulator_pa_R0),
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



p_alpha_dst_co1000 <- ggplot(cis_dst_1000_co, aes(x = true_alpha, y = alpha,
                                                  shape = likelihood,
                                                  colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_1000_co$simulator_pa_R0),
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



p_omega_dst_au1000 <- ggplot(cis_dst_1000_au, aes(x = true_omega, y = omega,
                                                  shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_1000_au$simulator_pa_R0),
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



p_alpha_dst_au1000 <- ggplot(cis_dst_1000_au, aes(x = true_alpha, y = alpha,
                                                  shape = likelihood,
                                                  colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.3, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_1000_au$simulator_pa_R0),
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



#sina plots for omega for diversitree for 500 tips ----
p_omega_dst_co500 <- ggplot(cis_dst_500_co, aes(x = true_omega, y = omega,
                                                shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_500_co$simulator_pa_R0),
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



p_alpha_dst_co500 <- ggplot(cis_dst_500_co, aes(x = true_alpha, y = alpha,
                                                shape = likelihood,
                                                colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_y_log10(breaks = c(0.0001, 0.01, 1, 100, 1000),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_500_co$simulator_pa_R0),
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



p_omega_dst_au500 <- ggplot(cis_dst_500_au, aes(x = true_omega, y = omega,
                                                shape = likelihood, colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_omega, y = as.numeric(as.character(true_omega))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_500_au$simulator_pa_R0),
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



p_alpha_dst_au500 <- ggplot(cis_dst_500_au, aes(x = true_alpha, y = alpha,
                                                shape = likelihood,
                                                colour = simulator_pa_R0)) +
  geom_sina(position = position_dodge(width = 0.8), size = 0.5, alpha = 0.8,
            maxwidth = 0.8) +
  geom_sina(data = add_true_values_dst, aes(x = true_alpha, y = as.numeric(as.character(true_alpha))), size = 1, shape = 21) +
  scale_colour_manual(
    values = cbbPalette[c(7,8)],
    breaks = levels(cis_dst_500_au$simulator_pa_R0),
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


#create composite with sina plots
quartz()
co_dst_1000 <- p_alpha_dst_co1000 + p_omega_dst_co1000 +
  p_alpha_dst_au1000 + p_omega_dst_au1000 +
  plot_annotation(tag_levels = 'A')

co_dst_500 <- p_alpha_dst_co500 + p_omega_dst_co500 +
  p_alpha_dst_au500 + p_omega_dst_au500 +
plot_annotation(tag_levels = 'A')




ggsave(filename = "sina_plots_dst_1000tips.pdf",
       plot = co_dst_1000,  width = 18, height = 14, units = "cm")

ggsave(filename = "sina_plots_dst_500tips.pdf",
       plot = co_dst_500,  width = 18, height = 14, units = "cm")
