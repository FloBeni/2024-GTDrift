library(stringr)

# abstract
list_species = read.delim("database/list_species.tab")

paste(" for ",nrow(list_species)," eukaryotic species, including ",sum(list_species$clade_group != "Embryophyta")," animals and ",sum(list_species$clade_group == "Embryophyta")," green plants",sep="")

paste(" mass for a set of ",sum(list_species$lht_data)," species",sep="")

paste(" in protein-coding sequences for ",sum(list_species$dnds_data)," species.",sep="")

paste("estimates for ",sum(list_species$Ne_data)," species.",sep="")

paste(" for more than ",sum(list_species$nb_rnaseq)," RNA-seq samples across ",sum(list_species$expression_data)," species",sep="")



# Background \& Summary

paste("These resources have been compiled for ",sum(list_species$nb_rnaseq)," RNA-seq samples spanning ",nrow(list_species)," multicellular eukaryotic species.",sep="")

paste(" with ",sum(list_species$clade_group == "Embryophyta")," green plant species, ",
      sum(!list_species$clade_group %in% c("Other Vertebrates","Teleostei","Aves","Mammalia","Embryophyta"))," invertebrates and ",
      sum(list_species$clade_group %in% c("Other Vertebrates","Teleostei","Aves","Mammalia"))," vertebrates.",sep="")


# Methods
# Figure 1

paste("Not all species studied are present (N=",length(read.tree("data/dnds_phylo/timetree.nwk")$tip.label),")",sep="")

# Species selection

paste("We included ",nrow(list_species)," multicellular eukaryotic organisms",sep="")
paste("This collection encompasses ",sum(list_species$clade_group != "Embryophyta")," animal species as well as ",sum(list_species$clade_group == "Embryophyta")," species of green plants",sep="")


# Collecting life history traits

paste("the acquisition of life history traits for ",sum(list_species$lht_data)," metazoan species.",sep="")


# Phylogenetic tree reconstruction
list_readme = list.files("data/dnds_phylo",pattern = "readme",recursive = T,full.names = T)
list_readme = list_readme[!grepl("per_clade",list_readme)]
dt=data.frame()
for (i in list_readme){
  
  
  di = read.delim(i,header=F)
  rownames(di) = di$V1
  di = data.frame(t(di))
  print(list.dirs(str_replace_all(i,"readme","dnds")))
  di[,"nb_cluster"] = length(list.dirs(str_replace_all(i,"readme","dnds")))-1
  dt = rbind(dt,
             di
  )
}
dt = dt[!duplicated(dt$busco_set),]
rownames(dt) = dt$busco_set


paste(" reducing the eukaryota set to ",dt["eukaryota_odb9",]$nb_genes_at_least_85percent_species,
      " genes (embryophyta Ngenes=",dt["embryophyta_odb9",]$nb_genes_at_least_85percent_species,
      ", metazoa Ngenes=",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species,
      ").",sep="")

paste(" selecting alignments with the highest number of species (eukaryota Ngenes=",dt["eukaryota_odb9",]$nb_genes_raxml,
      ", embryophyta Ngenes=",dt["embryophyta_odb9",]$nb_genes_raxml
      ,", metazoa Ngenes=",dt["metazoa_odb9",]$nb_genes_raxml,").",sep="")

paste(" final alignment for the eukaryota BUSCO dataset included ",dt["eukaryota_odb9",]$nb_species_analyzed
      ," taxa (embryophyta Nsp=",dt["embryophyta_odb9",]$nb_species_analyzed
      ,", metazoa Nsp=",dt["metazoa_odb9",]$nb_species_analyzed
      ,") taxa and ",as.numeric(dt["eukaryota_odb9",]$length_raxml_concat)/1000,
      " sites (embryophyta N=",as.numeric(dt["embryophyta_odb9",]$length_raxml_concat)/1000,
      ", metazoa N=",as.numeric(dt["metazoa_odb9",]$length_raxml_concat)/1000,
      ").",sep="")

paste("We used as a basis for the analysis ",round(as.numeric(dt["metazoa_odb9",]$nb_genes_at_least_85percent_species)/10),
      " highly prevalent metazoan BUSCO genes among the ",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species,
      " already preselected in the metazoa analysis.",sep="")


# dN/dS computation

paste("BUSCO gene sets: eukaryota (Ngenes=",dt["eukaryota_odb9",]$nb_genes_at_least_85percent_species
      ,"), embryophyta (Ngenes=",dt["embryophyta_odb9",]$nb_genes_at_least_85percent_species
      ,"), metazoa and '\textit{per} clade' (Ngenes=",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species
      ,").",sep="")

paste("This process yielded ",dt["eukaryota_odb9",]$nb_cluster,
      " groups for eukaryota (embryophyta Ngroup=",dt["embryophyta_odb9",]$nb_cluster,
      ", metazoa Ngroup=",dt["metazoa_odb9",]$nb_cluster,
      ").",sep="")


# Selection of the RNA-seq

paste("We included more than 50 samples for ",sum(list_species$nb_rnaseq >= 50)," species (",sum(list_species$nb_rnaseq >= 50 & list_species$clade == "Embryophyta"),
      " embryophyta, ",sum(list_species$nb_rnaseq >= 50 & list_species$clade != "Embryophyta")," metazoa), for which we performed more detailed analyses, considering various tissues or developmental stages.",sep="")

paste(" of ",sum(list_species$expression_data)," distinct species, including ",
      sum(list_species[list_species$expression_data,]$clade_group == "Embryophyta"),
      " plants and ",
      sum(list_species[list_species$expression_data,]$clade_group != "Embryophyta")," animals",sep="")


# Results

# Life History Traits

paste("(longevity, body mass, and body weight), for ",sum(list_species$lht_data)," species.",sep="")


# Protein-coding sequence evolution features

paste("(longevity, body mass, and body weight), for ",sum(list_species$dnds_data)," species.",sep="")


# Technical Validation

library(stringr)

manual_truth = read.delim(paste("data/life_history_traits/all_life_history_traits.tab",sep=""))
manual_truth[is.na(manual_truth$db),]$db = "none"
manual_truth$id = paste(manual_truth$species,sapply(manual_truth$db,function(x) str_split_1(x," ")[1]),sapply(manual_truth$life_history_traits,function(x) str_split_1(x,"_")[1]),sep=";")
rownames(manual_truth) = manual_truth$id

screen_data = read.table("data/life_history_traits/screen_db/screened_life_history_traits.tab",sep="\t",header=T)
rownames(screen_data) = screen_data$id

ml_data =  read.table("data/life_history_traits/ADW_ML/ml_life_history_traits.tab",sep="\t",header=T)
rownames(ml_data) = ml_data$id


manual_truth$screen_value = screen_data[manual_truth$id,]$value_used
screen_data$manual_value = manual_truth[screen_data$id,]$value

screen_data$true_ornot =  as.character(screen_data$value_used) == as.character(screen_data$manual_value) 
manual_truth$screen_true_ornot =  as.character(manual_truth$value) == as.character(manual_truth$screen_value) 


manual_truth$ml_value = ml_data[manual_truth$id,]$value_used
ml_data$manual_value = manual_truth[ml_data$id,]$value

ml_data$true_ornot =  as.character(ml_data$value_used) == as.character(ml_data$manual_value) 
manual_truth$ml_true_ornot =  as.character(manual_truth$value) == as.character(manual_truth$ml_value) 

table(manual_truth$ml_true_ornot,manual_truth$screen_true_ornot)



for (db in c("AnAge","fishbase","EOL","ADW")){print(db)
  manual_truth_sub = manual_truth[grepl(db,manual_truth$db),]
  screen_data_sub = screen_data[grepl(db,screen_data$db),]
  ml_data_sub = ml_data[grepl(db,ml_data$db),]
  
  print("Prop data retrieved screen:")
  print(sum(manual_truth_sub$screen_true_ornot,na.rm = T ) / nrow(manual_truth_sub)*100)
  print("Prop screen error:")
  print((1-sum(screen_data_sub$true_ornot,na.rm = T ) / nrow(screen_data_sub))*100)
  
  if (db=="ADW"){
    print("Prop data retrieved ml:")
    print(100*sum(manual_truth_sub$ml_true_ornot,na.rm = T ) / nrow(manual_truth_sub))
    print("Prop screen ml:")
    print((1-sum(ml_data_sub$true_ornot,na.rm = T ) / nrow(ml_data_sub))*100)
    
    print(100*sum(manual_truth_sub$ml_true_ornot | manual_truth_sub$screen_true_ornot,na.rm = T ) / nrow(manual_truth_sub))
    print((1-(sum(ml_data_sub$true_ornot,na.rm = T )+sum(screen_data_sub$true_ornot,na.rm = T )) / (nrow(ml_data_sub)+nrow(screen_data_sub)))*100)
  }
}






