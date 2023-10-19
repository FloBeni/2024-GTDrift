

library(ape)
library(seqinr)
library(stringr)

data_reference_path = "/beegfs/data/fbenitiere/Projet-SplicedVariants/DnDs/Metazoa_v11/gene_No_aas_cds"
pathData = "/beegfs/data/fbenitiere/Projet-SplicedVariants/"
fileName = "busco_to_gene_id_metazoa"
phylogenetic_tree_path = "/beegfs/data/fbenitiere/Projet-SplicedVariants/DnDs/Metazoa_v11/RAxML/concatenatAAS_cons150.aln.raxml"
list_species_metazoa_path = "/beegfs/data/fbenitiere/Projet-SplicedVariants/Fichiers-data/metazoa_species_clade_lht.tab"
list_species_metazoa2_path = "/beegfs/data/fbenitiere/Projet-SplicedVariants/Fichiers-data/metazoa_species2_clade_lht.tab"
list_species_metazoa_dnds_path = "/beegfs/data/fbenitiere/Projet-SplicedVariants/Fichiers-data/metazoa_requisit_for_dnds_clade_lht.tab"
study_dir_path =  "/beegfs/data/fbenitiere/Projet-SplicedVariants/DnDs/Metazoa_clades_v2/"


data_reference = read.delim(data_reference_path)


list_species_metazoa = rbind(read.delim(list_species_metazoa_path),read.delim(list_species_metazoa2_path),read.delim(list_species_metazoa_dnds_path))
list_species_metazoa = list_species_metazoa[!duplicated(list_species_metazoa$species),]
rownames(list_species_metazoa) = list_species_metazoa$species

list_species_metazoa$protein_file = sapply(list_species_metazoa$species,function(species) file.exists(paste(pathData,"Annotations/",species,"/data_source/protein.faa", sep = "")) )
list_species_metazoa$cds_from_genomic_file = sapply(list_species_metazoa$species,function(species) file.exists(paste(pathData,"Annotations/",species,"/data_source/cds_from_genomic.fna", sep = "")) )
list_species_metazoa$busco_file = sapply(list_species_metazoa$species,function(species) file.exists(paste(pathData,"Annotations/",species,"/busco_analysis/",fileName, sep = "")) )


list_species_metazoa$in_data_ref = list_species_metazoa$species %in% data_reference$species

phylogenetic_tree = read.tree(phylogenetic_tree_path)

list_species_metazoa$in_tree_ref = list_species_metazoa$species %in% phylogenetic_tree$tip.label

list_species_metazoa = list_species_metazoa[
  list_species_metazoa$busco_file &
    list_species_metazoa$cds_from_genomic_file &
    list_species_metazoa$protein_file,]

list_species_metazoa = list_species_metazoa[!is.na(list_species_metazoa$clade),]

table(list_species_metazoa$clade)
# list_species_metazoa[,"clade_group"] = "Other Invertebrates"
# list_species_metazoa[list_species_metazoa$clade %in% c("Vertebrates","Amphibia","Mammalia","Aves","Lepidosauria","Testudines","Crocodylia","Teleostei","Anura","Chondrichthyes"),"clade_group"] = "Other Vertebrates"
# list_species_metazoa[list_species_metazoa$clade %in% c("Blattodea","Coleoptera","Ephemeroptera","Hemiptera","Thysanoptera"),"clade_group"] = "Other Insecta"
# list_species_metazoa[list_species_metazoa$clade %in% c("Diptera","Lepidoptera"),"clade_group"] = "Lepido Diptera"
# list_species_metazoa[list_species_metazoa$clade %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),"clade_group"] = list_species_metazoa[list_species_metazoa$clade %in% c("Hymenoptera","Nematoda","Mammalia","Aves","Teleostei","Embryophyta"),"clade"]
# list_species_metazoa$clade_group = factor(list_species_metazoa$clade_group, levels = c("Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Mammalia","Aves","Teleostei","Other Vertebrates","Embryophyta"))

df = data.frame()
for (species_name in list_species_metazoa$species){print(species_name)
  table_ncbi = read.delim(paste(pathData ,"Annotations/" , species_name , "/taxonomy_ncbi.tab" , sep = ""))
  table_ncbi$species = species_name
  df = rbind(df,table_ncbi)
}

list_species_metazoa[ df[df$name == "Metazoa",]$species,"clade_group"] = "Other Invertebrates"
list_species_metazoa[ df[df$name == "Vertebrata",]$species,]$clade_group = "Other Vertebrates"
list_species_metazoa[ df[df$name == "Insecta",]$species,]$clade_group = "Other Insecta"
list_species_metazoa[ df[df$name %in% c("Diptera","Lepidoptera"),]$species,]$clade_group = "Lepido Diptera"
list_species_metazoa[ df[df$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade_group =
  list_species_metazoa[ df[df$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade


table(list_species_metazoa$clade_group)

list_species_metazoa$clade_group = str_replace_all(list_species_metazoa$clade_group," ","_")
table(list_species_metazoa$clade_group)

write.table(list_species_metazoa,paste(study_dir_path , "candidate_species",sep=""), row.names=F, col.names=T, sep="\t", quote=F)
