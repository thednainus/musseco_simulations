
library(ape)
library(ggtree)
tr1_julia <- read.tree("CoalescentJl_simulations/trees/params_2/rep1/param_2_rep_1.nwk")

tip_data1 <- data.frame(label = tr1_julia$tip.label, state = unlist(lapply(tr1_julia$tip.label, function(x) str_split(x, "\\.")[[1]][2])))


p <- ggtree(tr1_julia) %<+% tip_data1 +
  geom_tree(aes(color = state), size = 0.5) +
  scale_color_discrete(name = "State") +
  theme(legend.position = "right")

# Remove tip labels
p <- p + geom_tiplab(aes(label = ""), size = 0)

print(p)



tr9_julia <- read.tree("CoalescentJl_simulations/trees/params_2/rep9/param_2_rep_9.nwk")
tip_data9 <- data.frame(label = tr9_julia$tip.label, state = unlist(lapply(tr9_julia$tip.label, function(x) str_split(x, "\\.")[[1]][2])))


p9 <- ggtree(tr9_julia) %<+% tip_data1 +
  geom_tree(aes(color = state), size = 0.5) +
  scale_color_discrete(name = "State") +
  theme(legend.position = "right")

# Remove tip labels
p9 <- p9 + geom_tiplab(aes(label = ""), size = 0)

quartz()
print(p9)


