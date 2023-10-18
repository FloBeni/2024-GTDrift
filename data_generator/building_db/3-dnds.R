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
  write.tree(tree,paste("database/dNdS/newick/",name,"_dN.nwk",sep=""))
  
  tree$edge.length = dt_per_edge$dS / dt_per_edge$dS_norm
  write.tree(tree,paste("database/dNdS/newick/",name,"_dS.nwk",sep=""))
  
  
  data = data.frame(species = tree$tip.label,
                    NCBI.taxid = list_species[tree$tip.label,]$NCBI.taxid,
                    dN = get_newick_value(read.tree(paste("database/dNdS/newick/",name,"_dN.nwk",sep="")))[tree$tip.label],
                    dS = get_newick_value(read.tree(paste("database/dNdS/newick/",name,"_dS.nwk",sep="")))[tree$tip.label]
  )
  data$dNdS = data$dN / data$dS
  
  write.table(data , paste("database/dNdS/",name,".tab",sep=""),quote=F,row.names = F,sep="\t")
}


compute_files(name = "Eukaryota")
compute_files(name = "Embryophyta")
compute_files(name = "Metazoa")
