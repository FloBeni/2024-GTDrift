
#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))

options(stringsAsFactors = F, scipen = 999)
library(stringr)
library(seqinr)


args = (commandArgs(TRUE))
table_species_path = args[1]
cds_path = args[2]
infoPath = args[3]
gene_No_aas_cds_path = args[4]
summary_output = args[5]

############################################################################


all.files = list.files(cds_path,full.names = F , recursive = F)
all.files = str_replace_all(all.files,".fa","")

gene_No_aas_cds = read.table(gene_No_aas_cds_path,header=T)
rownames(gene_No_aas_cds) = gene_No_aas_cds$species

list_species_metazoa = read.delim( table_species_path , header = T)
tot_species = gene_No_aas_cds$species
list_species_metazoa$nb_gene_to_study = gene_No_aas_cds[list_species_metazoa$species ,]$nb_gene_to_study

list_species = list_species_metazoa$species[list_species_metazoa$nb_gene_to_study > ( 0.8 * length(all.files) )]
print(table(list_species != "Erythrura_gouldiae"))
list_species = list_species[list_species != "Erythrura_gouldiae"]
print(list_species)
print(length(list_species))

sequence_dict = data.frame(species = list_species)
rownames(sequence_dict) = sequence_dict$species
list.gene = c()
size.gene = c()
list.sp.by.gene = c()

# Through all sequence
for (file in all.files){print(file)

  print( length(list.gene) )
  list.gene = append(list.gene,file)

  fasta_seq = read.fasta(paste(cds_path , file , ".fa", sep = ""), seqtype = "DNA")
  fasta_seq = fasta_seq[grepl(paste(sequence_dict$species,collapse="|"),names(fasta_seq) )]
  ids.fasta_seq = names(fasta_seq)

  list.sp.by.gene = append(list.sp.by.gene,length(ids.fasta_seq))
  size.gene = append(size.gene, length(fasta_seq[[ids.fasta_seq[1]]]))
}

print(list.gene)
print(size.gene)
print(list.sp.by.gene)

info.table = data.frame(gene.id=list.gene,
                        gene.size=size.gene,
                        nb.sp=list.sp.by.gene)

info.table = rbind(info.table,data.frame(
  gene.id = paste("No genes:",length(list.gene),
                  "  total size:",print(length(sequence_dict[sapply(rownames(sequence_dict)[1],function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]])),"pb"),
  gene.size="",
  nb.sp=""))

write.table(info.table,infoPath, row.names=F, col.names=T, sep="\t", quote=F)


write.table(
  c(paste("Number of genes analyzed:",length(all.files)),
    paste("Total number of species:",length(tot_species)),
    paste("Number of species analyzed:",length(list_species)))
  ,summary_output, row.names=F, col.names=F, sep="\t", quote=F)


