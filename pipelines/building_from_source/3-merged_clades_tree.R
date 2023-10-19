# Infer the general phylogenetic tree for the per clades analysis.
library(ape)
library(castor)

measure_distance = function(tree,tip1,tip2){
  cophenetic_matrix <- cophenetic(tree)
  index1 <- which(tree$tip.label == tip1)
  index2 <- which(tree$tip.label == tip2)
  distance <- cophenetic_matrix[index1, index2]
  return(distance)
}



clade_dt = read.delim("database/list_species.tab")
rownames(clade_dt) = clade_dt$species
clade_dt$clade_group = str_replace_all(clade_dt$clade_group," ","_")

# Which clade is included in an other clades.
ref_group_list = list("Other_Invertebrates"=c("Other_Insecta","Other_Vertebrates","Nematoda"),"Other_Vertebrates"=c("Mammalia","Aves","Teleostei"),"Other_Insecta"=c("Hymenoptera","Lepido_Diptera"))

# Start from one clade and add the other clade one by one to the tree
order_clade = c("Other_Invertebrates","Other_Vertebrates","Mammalia","Aves","Teleostei","Other_Insecta", "Nematoda","Hymenoptera","Lepido_Diptera"    )

clade_ref = order_clade[1]
clade_ref_group = order_clade[1]
original_tree <- read.tree(paste("data/dnds_phylo/per_clade/",clade_ref,"/phylogeny/raxml.root.nwk",sep=""))

for (clade in order_clade[2:9]){print(clade) # Basically it starts with one tree and merges the other trees based on the existing shared outgroup species.
  clade_togreff = clade
  clade_togreff_group = names(ref_group_list)[grepl(clade,ref_group_list)]
  
  out_name = paste(clade_ref,clade_togreff,sep=";")
  
  subtree_to_add <- read.tree(paste("data/dnds_phylo/per_clade/",clade_togreff,"/phylogeny/raxml.root.nwk",sep="")) # Extract the tree to incorporate.
  
  species_1 = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & clade_dt$clade_group == clade_ref_group  ,]$species # Identify the species shared by both tree that corresponds to the upper clade.
  species_2 = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & clade_dt$clade_group == clade  ,]$species # Identify the species shared by both tree that corresponds to the clade to incorporate.
  
  max_distance = max(measure_distance(original_tree,species_1,species_2), measure_distance(subtree_to_add,species_1,species_2)) # Measure the maximum distance between both species in the two trees.
  
  print( unlist(ref_group_list[clade_togreff]))
  species_to_remove_from_original = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & clade_dt$clade_group %in% unlist(ref_group_list[clade_togreff])  ,]$species # Remove species from original tree.
  print(species_to_remove_from_original)
  species_to_remove_from_greff = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & !clade_dt$clade_group %in% unlist(c(clade,ref_group_list[clade_togreff]))  ,]$species # Remove species from the tree to incorporate.
  print(species_to_remove_from_greff)
  
  tree_to_greff <- drop.tip(subtree_to_add, species_to_remove_from_greff)
  original_tree <- drop.tip(original_tree, species_to_remove_from_original)
  
  tip_node <- which(tree_to_greff$tip.label == species_2)
  dist_from_root = get_all_distances_to_root(tree_to_greff)[tip_node]
  
  tip_node <- which(original_tree$tip.label == species_2)
  original_tree$edge.length[which(original_tree$edge[,2]==tip_node)] = 0
  
  # Identify the phylogenetic distance that needs to be put on the first branch of the incorporated tree.
  distance = measure_distance(original_tree,species_1,species_2)
  
  print(max_distance - dist_from_root - distance)
  if ( max_distance - dist_from_root - distance < 0){
    original_tree$edge.length[which(original_tree$edge[,2]==tip_node)] = 0.001 # If the distance has already reached the maximum distance previously found.
  } else {
    original_tree$edge.length[which(original_tree$edge[,2]==tip_node)] = max_distance - dist_from_root - distance
  }
  node_to_attach_to <- which(original_tree$tip.label == species_2)
  
  original_tree <- bind.tree(original_tree, tree_to_greff, where = node_to_attach_to)
  
  measure_distance(original_tree,species_1,species_2)
  
  clade_ref = out_name
}

write.tree(original_tree, file = paste("data/dnds_phylo/per_clade/merged_clades_tree_root.nwk",sep="")) # Save the tree.
write.tree(original_tree, file = paste("database/dNdS/phylogeny/merged_clades_tree_root.nwk",sep="")) # Save the tree.

