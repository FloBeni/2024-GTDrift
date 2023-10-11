library(stringr)
library(ape)

get_newick_value = function(arbre_phylo){
  nodes <- sapply(arbre_phylo$tip.label,function(x,y) which(y==x),y=arbre_phylo$tip.label)
  edge.length <- setNames(arbre_phylo$edge.length[sapply(nodes,function(x,y) which(y==x),y=arbre_phylo$edge[,2])],names(nodes))
  return(edge.length)
}

list_species = data.frame(sp_taxid = list.dirs("database/BUSCO_annotations/",recursive = F,full.names = F))
list_species$species = sapply(list_species$sp_taxid,function(x) str_split(x,"_NCBI.taxid")[[1]][1])
list_species$NCBI.taxid = sapply(list_species$sp_taxid,function(x) str_split(x,"_NCBI.taxid")[[1]][2])
rownames(list_species) = list_species$species

path = "data/dnds_phylo/"

compute_files <- function(name){
  concatenate_list = list.files(paste(path,name,"/dnds",sep=""),recursive = T,full.names = F,pattern="raxml.dnd_1")
  concatenate_list = str_replace_all(concatenate_list,"/raxml.dnd_1","")
  
  big_data = data.frame()
  
  concatenate = concatenate_list[1]
  tree = read.tree(paste(path,name,"/dnds/",concatenate,"/raxml.dnd_1",sep=""))
  
  dt_per_edge = data.frame(edge.length = tree$edge.length)
  for (concatenate in concatenate_list){
    tree_length = read.tree(paste(path,name,"/dnds/",concatenate,"/raxml.dnd_1",sep=""))
    for (type in c("dN","dS")){
      for (numden in c("","_norm")){
        tree = read.tree(paste(path,name,"/dnds/",concatenate,"/counts_",type,numden,".dnd",sep=""))
        if (numden == "_norm"){ tree$edge.length = tree$edge.length / tree_length$edge.length}
        if (paste(type,numden,sep="") %in% colnames(dt_per_edge)){
          dt_per_edge[,paste(type,numden,sep="")] = tree$edge.length + dt_per_edge[,paste(type,numden,sep="")]
        } else {
          dt_per_edge[,paste(type,numden,sep="")] = tree$edge.length 
        }
      }
    }
  }
  
  tree$edge.length = dt_per_edge$dN / dt_per_edge$dN_norm
  write.tree(tree,paste("data/dnds_phylo/",name,"/dnds/dN.nwk",sep=""))
  
  tree$edge.length = dt_per_edge$dS / dt_per_edge$dS_norm
  write.tree(tree,paste("data/dnds_phylo/",name,"/dnds/dS.nwk",sep=""))
  
  data = data.frame(species = tree$tip.label,
                    NCBI.taxid = list_species[tree$tip.label,]$NCBI.taxid,
                    dN = get_newick_value(read.tree(paste("data/dnds_phylo/",name,"/dnds/dN.nwk",sep="")))[tree$tip.label],
                    dS = get_newick_value(read.tree(paste("data/dnds_phylo/",name,"/dnds/dS.nwk",sep="")))[tree$tip.label]
  )
  data$dNdS = data$dN / data$dS
  
  write.table(data , paste("data/dnds_phylo/",name,"/dnds/dNdS.tab",sep=""),quote=F,row.names = F,sep="\t")
}

for (clade in c("Aves","Hymenoptera","Lepido_Diptera","Mammalia","Nematoda","Other_Insecta","Other_Invertebrates","Other_Vertebrates","Teleostei")){
  compute_files(name =  paste("per_clade/",clade,sep=""))
}



#### Merged dN/dS
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

ref_group_list = list("Other_Invertebrates"=c("Other_Insecta","Other_Vertebrates","Nematoda"),"Other_Vertebrates"=c("Mammalia","Aves","Teleostei"),"Other_Insecta"=c("Hymenoptera","Lepido_Diptera"))

order_clade = c("Other_Invertebrates","Other_Vertebrates","Mammalia","Aves","Teleostei","Other_Insecta", "Nematoda","Hymenoptera","Lepido_Diptera"    )


for (type in c("dS","dN")){
  clade_ref = order_clade[1]
  clade_ref_group = order_clade[1]
  original_tree <- read.tree(paste("data/dnds_phylo/per_clade/",clade_ref,"/dnds/",type,".nwk",sep=""))
  
  for (clade in order_clade[2:9]){print(clade)
    clade_togreff = clade
    clade_togreff_group = names(ref_group_list)[grepl(clade,ref_group_list)]
    
    out_name = paste(clade_ref,clade_togreff,sep=";")
    
    subtree_to_add <- read.tree(paste("data/dnds_phylo/per_clade/",clade_togreff,"/dnds/",type,".nwk",sep=""))
    
    species_1 = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & clade_dt$clade_group == clade_ref_group  ,]$species
    species_2 = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & clade_dt$clade_group == clade  ,]$species
    
    max_distance = max(measure_distance(original_tree,species_1,species_2),
                       measure_distance(subtree_to_add,species_1,species_2))
    
    print( unlist(ref_group_list[clade_togreff]))
    species_to_remove_from_original = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & clade_dt$clade_group %in% unlist(ref_group_list[clade_togreff])  ,]$species
    print(species_to_remove_from_original)
    species_to_remove_from_greff = clade_dt[clade_dt$species %in% original_tree$tip.label & clade_dt$species %in% subtree_to_add$tip.label & !clade_dt$clade_group %in% unlist(c(clade,ref_group_list[clade_togreff]))  ,]$species
    print(species_to_remove_from_greff)
    
    tree_to_greff <- drop.tip(subtree_to_add, species_to_remove_from_greff)
    original_tree <- drop.tip(original_tree, species_to_remove_from_original)
    
    tip_node <- which(tree_to_greff$tip.label == species_2)
    dist_from_root = get_all_distances_to_root(tree_to_greff)[tip_node]
    
    tip_node <- which(original_tree$tip.label == species_2)
    original_tree$edge.length[which(original_tree$edge[,2]==tip_node)] = 0
    
    distance = measure_distance(original_tree,species_1,species_2)
    
    print(max_distance - dist_from_root - distance)
    if ( max_distance - dist_from_root - distance < 0){
      original_tree$edge.length[which(original_tree$edge[,2]==tip_node)] = 0.001
    } else {
      original_tree$edge.length[which(original_tree$edge[,2]==tip_node)] = max_distance - dist_from_root - distance
    }
    node_to_attach_to <- which(original_tree$tip.label == species_2)
    
    original_tree <- bind.tree(original_tree, tree_to_greff, where = node_to_attach_to)
    
    measure_distance(original_tree,species_1,species_2)
    
    clade_ref = out_name
  }
  
  
  write.tree(original_tree, file = paste("database/dNdS/newick/per_clade_",type,".nwk",sep=""))
}





data = data.frame(species = original_tree$tip.label,
                  NCBI.taxid = list_species[original_tree$tip.label,]$NCBI.taxid,
                  dN = get_newick_value(read.tree(paste("database/dNdS/newick/per_clade_dN.nwk",sep="")))[original_tree$tip.label],
                  dS = get_newick_value(read.tree(paste("database/dNdS/newick/per_clade_dS.nwk",sep="")))[original_tree$tip.label]
)
data$dNdS = data$dN / data$dS

write.table(data , paste("database/dNdS/per_clade.tab",sep=""),quote=F,row.names = F,sep="\t")



