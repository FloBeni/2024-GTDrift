library(stringr)

# abstract
list_species = read.delim("database/list_species.tab")

paste("of ",nrow(list_species)," distinct species, including ",sum(list_species$clade_group != "Embryophyta")," animal and ",sum(list_species$clade_group == "Embryophyta")," green plant species",sep="")

paste("mass for a subset of ",sum(list_species$lht_data)," species",sep="")

paste("performed on ",sum(list_species$dnds_data)," species using",sep="")

paste("for more than ",sum(list_species$nb_rnaseq)," RNA-seq samples across ",sum(list_species$expression_data)," species",sep="")



# Background \& Summary

paste("These resources have been compiled for ",sum(list_species$nb_rnaseq)," RNA-seq samples spanning ",nrow(list_species)," multicellular eukaryotic species.",sep="")

paste(" with ",sum(list_species$clade_group == "Embryophyta")," green plant species, ",
      sum(!list_species$clade_group %in% c("Other Vertebrates","Teleostei","Aves","Mammalia","Embryophyta"))," invertebrates and ",
      sum(list_species$clade_group %in% c("Other Vertebrates","Teleostei","Aves","Mammalia"))," vertebrates.",sep="")


# Methods

paste("compilation of ",nrow(list_species)," multicellular eukaryotic organisms",sep="")
paste(", comprising ",sum(list_species$clade_group != "Embryophyta")," animal species and ",sum(list_species$clade_group == "Embryophyta")," green plant ",sep="")


# Collecting life history traits

paste("life history traits for a total of ",sum(list_species$lht_data)," metazoan species.",sep="")


# Selection of the Assembly version

paste("This approach was applied successfully to a cohort of ",sum(list_species$clade_group != "Embryophyta")," metazoan and ",sum(list_species$clade_group == "Embryophyta")," embryophyta samples.",sep="")


# Phylogenetic tree reconstruction
list_readme = list.files("data/dnds_phylo",pattern = "readme",recursive = T,full.names = T)
list_readme = list_readme[!grepl("per_clade",list_readme)]
dt=data.frame()
for (i in list_readme){
  di = read.delim(i,header=F)
  rownames(di) = di$V1
  di = t(di)
  dt = rbind(dt,
             di
  )
}
dt = dt[!duplicated(dt$busco_set),]
rownames(dt) = dt$busco_set

paste("selecting alignments with the highest number of species (eukaryota Ngenes=",dt["eukaryota_odb9",]$nb_genes_raxml,
      ", embryophyta Ngenes=",dt["embryophyta_odb9",]$nb_genes_raxml
      ,", metazoa Ngenes=",dt["metazoa_odb9",]$nb_genes_raxml,").",sep="")




paste(" with a final alignment of (eukaryota Nsp=",dt["eukaryota_odb9",]$nb_species_analyzed,
      ", embryophyta Nsp=",dt["embryophyta_odb9",]$nb_species_analyzed
      ,", metazoa Nsp=",dt["metazoa_odb9",]$nb_species_analyzed,")",sep="")


paste("and (eukaryota Nsite=",as.numeric(dt["eukaryota_odb9",]$length_raxml_concat)/1000,
      ", embryophyta Nsite=",as.numeric(dt["embryophyta_odb9",]$length_raxml_concat)/1000
      ,", metazoa Nsite=",as.numeric(dt["metazoa_odb9",]$length_raxml_concat)/1000,") sites (amino acids)",sep="")



# dN/dS computation
paste("least 85 percent of the species (eukaryota Ngenes=",dt["eukaryota_odb9",]$nb_genes_at_least_85percent_species,
      ", embryophyta Ngenes=",dt["embryophyta_odb9",]$nb_genes_at_least_85percent_species
      ,", metazoa Ngenes=",dt["metazoa_odb9",]$nb_genes_at_least_85percent_species,"),",sep="")




# Selection of the RNA-seq
paste("are ",sum(list_species$expression_data)," species represented, ",sum(list_species[list_species$expression_data,]$clade_group == "Embryophyta"),
      " plant and ",sum(list_species[list_species$expression_data,]$clade_group != "Embryophyta")," animal,",sep="")


# Data Records
paste("At the time of publication, the database contained over ",sum(list_species$nb_rnaseq)," RNA-seq distributed over ",nrow(list_species)," embryophytes and metazoans ",sep="")


# Life History Traits
paste("body mass, and body weight for ",sum(list_species$lht_data)," species",sep="")



# Technical Validation



