#simulate sequence alignments with AliSim
#http://www.iqtree.org/doc/AliSim

# Simulate sequence alignments
# IQ-TREE MUST be installed in your computer

#In this new version TFP mutations will not be added my Alisim

library(ape)



# Location for AliSim. It should be changed to the correct location on your computer. AliSim is part of iqtree
Software <- "/Applications/iqtree-2.4.0-macOS/bin/iqtree2"
#Software <- "iqtree"

#parameter for AliSim: for nucleotide substitution model


#for env gene
#clock rate: 3.2e-3
# iqtree2 --alisim HIV_MSA_sim_1 -t HIV-sim-tree.treefile -m GTR{2.21430,5.04449,0.73827,0.72478,5.39684,1.0}+F{0.342,0.169,0.246,0.244}+I{0.117} --branch-scale  0.0032 -seed 42 --out-format fasta

#for v3 loop
#clock rate: 3.6e-3
# iqtree2 --alisim HIV_MSA_sim_1 -t HIV-sim-tree.treefile -m GTR{3.51951,7.79129,2.15558,3.12071,21.10303,1.00000}+F{0.444,0.156,0.220,0.180}+I{0.063} --branch-scale 0.0036 -seed 42 --out-format fasta



# read simulated tree
#tre <- read.tree("HIV_simulations/trees/sample_size_4056/params_1/rep1/param_1_rep_1.nwk")
tre_filename <- list.files(pattern = "*.nwk")


# simulate sequence alignment using AliSim
#Simulate alignments of the size 2371 bp equivalent to the env gene
# and 126 bp for the V3


# generate a seed to run AliSim

#using codon substitution model
args <- c( "--alisim ali",
  "-m GTR{3.51951/7.79129/2.15558/3.12071/21.10303/1.00000}+F{0.444/0.156/0.220/0.180}+I{0.063}", #'formatting is OS dependent, see https://github.com/iqtree/iqtree2/discussions/196
  paste("-t", tre_filename, " "),
  "--branch-scale 0.0036",
  "--length 105",
  "--out-format fasta"
)
system2(command = Software, args = args)
