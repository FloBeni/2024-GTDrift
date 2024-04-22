library(stringr)
library(ape)
{
  # abstract
  print("Abstract")
  list_species = read.delim("database/list_species.tab")
  
  print(paste(" for ",nrow(list_species)," eukaryotic species, including ",sum(list_species$clade_group != "Embryophyta")," animals and ",sum(list_species$clade_group == "Embryophyta")," green plants",sep=""))
  
  print(paste(" mass for a set of ",sum(list_species$lht_data)," species",sep=""))
  
  print(paste(" in protein-coding sequences for ",sum(list_species$dnds_data)," species.",sep=""))
  
  print(paste("estimates for ",sum(list_species$Ne_data)," species.",sep=""))
  
  print(paste(" for more than ",sum(list_species$nb_rnaseq)," RNA-seq samples across ",sum(list_species$expression_data)," species",sep=""))
  
  
  
  # Introduction
  print("Introduction")
  
  print(paste("These resources have been compiled for ",sum(list_species$nb_rnaseq)," RNA-seq samples spanning ",nrow(list_species)," multicellular eukaryotic species.",sep=""))
  
  print(paste(" with ",sum(list_species$clade_group == "Embryophyta")," green plant species, ",
              sum(!list_species$clade_group %in% c("Other Vertebrates","Teleostei","Aves","Mammalia","Embryophyta"))," invertebrates and ",
              sum(list_species$clade_group %in% c("Other Vertebrates","Teleostei","Aves","Mammalia"))," vertebrates.",sep=""))
  
  
  # Methods
  print("Methods")
  
  print(paste("We included ",nrow(list_species)," multicellular eukaryotic species. This collection encompasses ",sum(list_species$clade_group != "Embryophyta"),
              " animal species as well as ",sum(list_species$clade_group == "Embryophyta")," species of green plant",sep=""))
  # Figure 1
  print("Figure 1")
  print(paste("Not all species studied are present (N=",length(read.tree("data/dnds_phylo/timetree.nwk")$tip.label),")",sep=""))
  
  # Collecting life history traits
  print("Collecting life history traits")
  print(paste("In total, our data collection effort resulted in the acquisition of life history traits for ",sum(list_species$lht_data)," metazoan species.",sep=""))
  
  # Phylogenetic tree reconstruction
  print("Collecting life history traits")
  
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
  
  
  print(paste(" reducing the eukaryota set to ",dt["eukaryota_odb9",]$nb_genes_at_least_85percent_species,
              " genes (embryophyta N=",dt["embryophyta_odb9",]$nb_genes_at_least_85percent_species,
              " genes, metazoa N=",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species,
              " genes).",sep=""))
  
  print(paste(" reducing the number of studied species from ",dt["eukaryota_odb9",]$nb_species_initially,
              " to ",dt["eukaryota_odb9",]$nb_species_analyzed,
              " for the eukaryota BUSCO dataset (embryophyta ",dt["embryophyta_odb9",]$nb_species_initially,
              " to ",dt["embryophyta_odb9",]$nb_species_analyzed,
              " species, metazoa ",dt["metazoa_odb9",]$nb_species_initially,
              " to ",dt["metazoa_odb9",]$nb_species_analyzed,
              " species).",sep=""))
  
  
  print(paste(" selecting alignments with the highest number of species (eukaryota N=",dt["eukaryota_odb9",]$nb_genes_raxml,
              " genes, embryophyta N=",dt["embryophyta_odb9",]$nb_genes_raxml
              ," genes, metazoa N=",dt["metazoa_odb9",]$nb_genes_raxml," genes).",sep=""))
  
  print(paste(" final alignment for the eukaryota BUSCO dataset included ",dt["eukaryota_odb9",]$nb_species_analyzed
              ," taxa (embryophyta N=",dt["embryophyta_odb9",]$nb_species_analyzed
              ," species, metazoa N=",dt["metazoa_odb9",]$nb_species_analyzed
              ," species) taxa and ",as.numeric(dt["eukaryota_odb9",]$length_raxml_concat)/1000,
              " sites (embryophyta N=",as.numeric(dt["embryophyta_odb9",]$length_raxml_concat)/1000,
              " sites, metazoa N=",as.numeric(dt["metazoa_odb9",]$length_raxml_concat)/1000,
              " sites).",sep=""))
  
  print(paste("We ranked the ",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species,
              " metazoan BUSCO genes in decreasing order of the number of species in which they were annotated. We then selected as a basis for the analyses the ",round(as.numeric(dt["metazoa_odb9",]$nb_genes_at_least_85percent_species)/10),
              " genes at the top of this list",sep=""))
  
  
  # dN/dS computation
  print(paste("dN/dS computation"))
  
  print(paste("BUSCO gene sets: eukaryota (N=",dt["eukaryota_odb9",]$nb_genes_at_least_85percent_species
              ," genes), embryophyta (N=",dt["embryophyta_odb9",]$nb_genes_at_least_85percent_species
              ," genes), metazoa and metazoa (N=",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species
              ," genes).",sep=""))
  
  print(paste("tree reconstruction, using the same ",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species
              ," genes preselected in the metazoa analysis.",sep=""))
  
  print(paste("obtaining alignments that were 200 kb long on average. This process yielded ",dt["eukaryota_odb9",]$nb_cluster
              ," groups for eukaryota (",dt["embryophyta_odb9",]$nb_cluster
              ," for embryophyta and ",dt["metazoa_odb9",]$nb_cluster
              ," for metazoa)",sep=""))
  
  # Polymorphism-derived N e estimates
  print(paste("Polymorphism-derived Ne estimates"))
  
  dt_supp_lynch_2023 = read.delim("data/supp_lynch_2023.tab",comment.char = "#")
  rownames(dt_supp_lynch_2023) = dt_supp_lynch_2023$Species
  dt_supp_lynch_2023 = dt_supp_lynch_2023[!is.na(dt_supp_lynch_2023$Ne),]
  print(paste("his more direct estimates of Ne was calculated for ",sum(list_species$species %in% dt_supp_lynch_2023$Species)-1," of the species in our dataset"))
  
  print(paste("N e estimate for C. nigoni by including πs = ",dt_supp_lynch_2023["Caenorhabditis_nigoni",]$Silent.site.diversity," (Asher Cutter, personal communication) and μ = ",
              dt_supp_lynch_2023["Caenorhabditis_nigoni",]$Base.substitution.Mutation.rate,""))
  
  
  # Selection of the RNA-seq samples
  print(paste("Selection of the RNA-seq samples"))
  
  print(paste("We included more than 50 samples for ",sum(list_species$nb_rnaseq >= 50)," species (",sum(list_species$nb_rnaseq >= 50 & list_species$clade == "Embryophyta"),
              " embryophyta, ",sum(list_species$nb_rnaseq >= 50 & list_species$clade != "Embryophyta"),
              " metazoa), for which we performed more detailed analyses, considering various tissues or developmental stages.",sep=""))
  
  print(paste(" of ",sum(list_species$expression_data)," distinct species, including ",
              sum(list_species[list_species$expression_data,]$clade_group == "Embryophyta"),
              " plants and ",
              sum(list_species[list_species$expression_data,]$clade_group != "Embryophyta")," animals",sep=""))
  
  
  # Results
  print(paste("Results"))
  
  # Description of the data available in GTDrift
  print(paste("Description of the data available in GTDrift"))
  
  print(paste("At the time of publication, the database contained over ",sum(list_species$nb_rnaseq)," RNA-seq samples distributed over ",sum(list_species$expression_data)," embryophytes and metazoan",sep=""))
  
  print(paste("However, here we provide considerably more data, for ",nrow(list_species)," species compared to 53 in this publication.",sep=""))
  
  
  # Life History Traits
  print(paste("Life History Traits"))
  
  paste("distinct traits (body mass, longevity, and body length), for ",sum(list_species$lht_data)," species.",sep="")
  
  paste("dditionally, this table contains polymorphism-derived Ne estimates for ",sum(list_species$Ne_data)," species.",sep="")
  
  
  # Protein-coding sequence evolution features
  print(paste("Protein-coding sequence evolution "))
  
  paste("ratio for most species (N=",sum(list_species$dnds_data)," species after filtering for a sufficient number of annotated orthologous genes",sep="")
  
  
  # Data quality validation
  print(paste("Data quality validation"))
  
  # Acquiring life history traits
  print(paste("Acquiring life history traits"))
  
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
  
  db_data = data.frame()
  for (db in c("AnAge","fishbase","EOL","ADW")){
    print(db)
    manual_truth_sub = manual_truth[grepl(db,manual_truth$db),]
    screen_data_sub = screen_data[grepl(db,screen_data$db),]
    ml_data_sub = ml_data[grepl(db,ml_data$db),]
    
    db_data = rbind(db_data,data.frame(db,
                                       dt_screened_identified = sum(manual_truth_sub$screen_true_ornot,na.rm = T ) / nrow(manual_truth_sub),
                                       screened_error = (nrow(screen_data_sub) - sum(screen_data_sub$true_ornot,na.rm = T ))/nrow(screen_data_sub) ,
                                       dt_ml_identified = sum(manual_truth_sub$ml_true_ornot,na.rm = T )/nrow(manual_truth_sub),
                                       ml_error =(nrow(ml_data_sub) - sum(ml_data_sub$true_ornot,na.rm = T ))/nrow(ml_data_sub),
                                       dt_identified = sum(manual_truth_sub$ml_true_ornot | manual_truth_sub$screen_true_ornot,na.rm = T ) / nrow(manual_truth_sub),
                                       dt_error = (1-(sum(ml_data_sub$true_ornot,na.rm = T )+sum(screen_data_sub$true_ornot,na.rm = T )) / (nrow(ml_data_sub)+nrow(screen_data_sub)))
    ))
  }
  rownames(db_data) = db_data$db
  
  
  print(paste("he screening procedure yielded accurate information with varying false positive rates depending on the source database, as follows:AnAge (",
              round(db_data["AnAge",]$dt_screened_identified*100,1),"% accuracy; ",  round(db_data["AnAge",]$screened_error*100,1),"% false positive), fishbase (",
              round(db_data["fishbase",]$dt_screened_identified*100,1),"%; ",  round(db_data["fishbase",]$screened_error*100,1),"%), EOL (",
              round(db_data["EOL",]$dt_screened_identified*100,1),"%; ",  round(db_data["EOL",]$screened_error*100,1),"%), and ADW (",
              round(db_data["ADW",]$dt_screened_identified*100,1),"%;",  round(db_data["ADW",]$screened_error*100,1),"%).", sep=""))
  
  print(paste("Specifically, for life history traits, the ML approach correctly retrieved ",
              round(db_data["ADW",]$dt_ml_identified*100,1),"% of the results obtained through the manual approach, while introducing a ",
              round(db_data["ADW",]$ml_error*100,1),"% false positive rate.",sep=""))
  
  print(paste("When combining both the ML approach and the screening process, we achieved a ",
              round(db_data["ADW",]$dt_identified*100,1),"% accuracy rate in identifying positive cases. However, a ",
              round(db_data["ADW",]$dt_error*100,1),"% error rate persisted in this merged approach."
              ,sep=""))
  
  
  
  # Discussion
  print(paste("Discussion"))
  
  
  print(paste("life history traits, including longevity, adult body length, and body mass, for a curated set of ",sum(list_species$lht_data)," species",sep=""))
  print(paste("over synonymous substitutions (dN /dS) for ",sum(list_species$dnds_data)," species and a polymorphism-derived Ne estimates for ",sum(list_species$Ne_data)," species.",sep=""))
  print(paste("encompassing more than ",sum(list_species$nb_rnaseq)," RNA-seq samples across ",sum(list_species$expression_data)," species.",sep=""))
  print(paste("encompassing a total of ",nrow(list_species)," eukaryotic species, including ",sum(list_species$clade_group != "Embryophyta")," animals and ",sum(list_species$clade_group == "Embryophyta")," green plants",sep=""))
  
}




