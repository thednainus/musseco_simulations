
library(treedater)
library(ape)
#library(stringr)

# start of script
#start_time <- Sys.time()

#arguments will be the sequence length and the name of the alignment file
get_params <- commandArgs(trailingOnly = TRUE)

seqlength <- as.numeric(get_params[1])
seq_length <- paste(seqlength, "bp", sep = "")


#estimate treedated tree
#read ML tree
mltree <- read.tree(list.files(pattern = ".treefile"))

#check if tree has polytomies
#if tree has polytomies resolve polytomies randomly
#if(is.binary(mltree) == FALSE){
#  mltree <- multi2di(mltree)
#  if(is.rooted(mltree) == TRUE){
#    mltree <- unroot(mltree)
#  }
#}


#get sample times from the names of the phylogenetic tree tips

#read true trees to get sample times
#true_tr = read.tree(list.files(path = "..", pattern = ".nwk", full.names = T)[1])
true_tr = read.tree(list.files(pattern = ".nwk"))

#get sample times
sts <- node.depth.edgelength( true_tr )[1:Ntip(true_tr)] |> setNames( true_tr$tip.label )

sts <- sts + (29 - max(sts))


#run treedater
dated_tree <- dater(tre = mltree, sts = sts[mltree$tip.label],
                    s = seqlength, clock = "strict")


#dave dated tree
saveRDS(dated_tree, "dated_tree.RDS")
