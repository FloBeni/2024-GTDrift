
#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))

options(stringsAsFactors = F, scipen = 999)
library(seqinr)
library(stringr)

args = (commandArgs(TRUE))

clade = args[1]
gene_fasta_cds = args[2]
gene_No_aas_cds_path = args[3]
table_species_path = args[4]
gene_fasta_out = args[5]


gene_No_aas_cds = read.table(gene_No_aas_cds_path,header=T)
rownames(gene_No_aas_cds) = gene_No_aas_cds$species

list_species_metazoa = read.delim( table_species_path , header = T)
list_species_metazoa$nb_gene_to_study = gene_No_aas_cds[list_species_metazoa$species ,]$nb_gene_to_study
list_species_metazoa = list_species_metazoa[order(list_species_metazoa$nb_gene_to_study,decreasing = T),]
list_species_metazoa = list_species_metazoa[ list_species_metazoa$clade_group == clade | list_species_metazoa$in_tree_ref,]
list_species_metazoa = list_species_metazoa[ list_species_metazoa$clade_group == clade | !duplicated(list_species_metazoa$clade_group ) ,]

alignment = read.fasta(paste(gene_fasta_cds,sep=""))
alignment = alignment[sapply(names(alignment),function(x) str_split(x,"_buscoid")[[1]][1]) %in% list_species_metazoa$species]

write.fasta(alignment,
            names(alignment),
            file.out =paste(gene_fasta_out,sep=""))


