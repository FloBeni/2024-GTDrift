
#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))

options(stringsAsFactors = F, scipen = 999)
library(ape)
library(seqinr)


args = (commandArgs(TRUE))
clade = args[1]
table_species_path = args[2]
prank_path = args[3]
reference_tree_path = args[4]
# reference_tree_path = paste("/home/fbenitiere/data/Projet-SplicedVariants/DnDs/Metazoa_v9/RAxML/concatenatAAS_cons150.aln.raxml",sep="")
gene_No_aas_cds_path = args[5]
tree_output = args[6]

all.files = list.dirs(prank_path,full.names = F , recursive = F)

gene_No_aas_cds = read.table(gene_No_aas_cds_path,header=T)
rownames(gene_No_aas_cds) = gene_No_aas_cds$species

list_species_metazoa = read.delim( table_species_path , header = T)
tot_species = gene_No_aas_cds$species
list_species_metazoa$nb_gene_to_study = gene_No_aas_cds[list_species_metazoa$species ,]$nb_gene_to_study
list_species_metazoa = list_species_metazoa[order(list_species_metazoa$nb_gene_to_study,decreasing = T),]
list_species_metazoa = list_species_metazoa[ list_species_metazoa$clade_group == clade | list_species_metazoa$in_tree_ref ,]
list_species_metazoa = list_species_metazoa[ list_species_metazoa$clade_group == clade | !duplicated(list_species_metazoa$clade_group ) ,]


list_species = list_species_metazoa$species[list_species_metazoa$nb_gene_to_study > ( 0.8 * length(all.files) )]
print(table(list_species != "Erythrura_gouldiae"))
list_species = list_species[list_species != "Erythrura_gouldiae"]
print(list_species)
print(length(list_species))

reference_tree = read.tree( reference_tree_path )

table(list_species %in% reference_tree$tip.label)

tree_to_save = keep.tip(reference_tree, list_species[list_species %in% reference_tree$tip.label])

write.tree(tree_to_save, paste(tree_output,sep="") )
