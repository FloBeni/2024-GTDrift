
#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))


options(stringsAsFactors = F, scipen = 999)
library(seqinr)


args = (commandArgs(TRUE))
clade = args[1]
table_species_path = args[2]
prank_path = args[3]
infoPath = args[4]
gene_No_aas_cds_path = args[5]
summary_output = args[6]
concatenatePath_raxml = args[7]
concatenatePathAAS_raxml = args[8]
infoPath_raxml = args[9]
concatenatePathAAS_raxmlcons = args[10]
concatenatePath_raxmlcons = args[11]
nb_species_consensus = as.numeric(args[12])
infoPath_total = args[13]

############################################################################

print(nb_species_consensus)

all.files = list.dirs(prank_path , full.names = F , recursive = F)

gene_No_aas_cds = read.table(gene_No_aas_cds_path,header=T)
rownames(gene_No_aas_cds) = gene_No_aas_cds$species

list_species_metazoa = read.delim( table_species_path , header = T)
tot_species = gene_No_aas_cds$species
list_species_metazoa$nb_gene_to_study = gene_No_aas_cds[list_species_metazoa$species ,]$nb_gene_to_study
list_species_metazoa = list_species_metazoa[order(list_species_metazoa$nb_gene_to_study , decreasing = T),]
list_species_metazoa = list_species_metazoa[ list_species_metazoa$clade_group == clade | list_species_metazoa$in_tree_ref,]
list_species_metazoa = list_species_metazoa[ list_species_metazoa$clade_group == clade | !duplicated(list_species_metazoa$clade_group ) ,]


list_species = list_species_metazoa$species[list_species_metazoa$nb_gene_to_study > ( 0.8 * length(all.files) )]


# nb_species_consensus = nb_species_consensus * length(list_species)


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

  fasta_seq = read.fasta(paste(prank_path , file , "/" , clade , ".fa-prank.aln", sep = ""), seqtype = "DNA")
  fasta_seq = fasta_seq[grepl(paste(sequence_dict$species,collapse = "|"),names(fasta_seq) )]
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

info.table=rbind(info.table,data.frame(
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




### FOR THE RAXML-NG TREE
list_genes_RAXML = list.sp.by.gene
names(list_genes_RAXML) = all.files
list_genes_RAXML = list_genes_RAXML[ order(list_genes_RAXML,decreasing=T)]

print( 'list_genes_RAXML' )
print( list_genes_RAXML )


sequence_dict = data.frame(species = list_species)
rownames(sequence_dict) = sequence_dict$species

list.gene = c()
size.gene = c()
size.gene_consensus = c()
list.sp.by.gene = c()


table_all_gene_nb = read.delim(infoPath_total)
print(head(table_all_gene_nb))
table_all_gene_nb = table_all_gene_nb[-c(nrow(table_all_gene_nb)),]
table_all_gene_nb = table_all_gene_nb[order(table_all_gene_nb$nb.sp,decreasing = T),]

# Through 10% of the sequences
for (file in table_all_gene_nb$gene.id[1:(nrow(table_all_gene_nb) * 0.1)] ){
#for (file in names(list_genes_RAXML)[1:(length(list_genes_RAXML) * 0.1)] ){

  print( length( sequence_dict[rownames(sequence_dict)[1], 'sequence'][[1]] ) )
  print( length(list.gene) )

  list.gene = append(list.gene,file)

  fasta_seq = read.fasta(paste(prank_path , file , "/" , clade , ".fa-prank.aln" , sep = ""), seqtype = "DNA")
  fasta_seq = fasta_seq[grepl(paste(sequence_dict$species,collapse="|"),names(fasta_seq) )]
  ids.fasta_seq = names(fasta_seq)

  table_consensus = data.frame(lapply(fasta_seq,function(seq) !seq %in% "-"))

  consensus = rowSums(table_consensus)



  list.sp.by.gene = append(list.sp.by.gene,length(ids.fasta_seq))
  size.gene = append(size.gene, length(fasta_seq[[ids.fasta_seq[1]]]))
  size.gene_consensus = append( size.gene_consensus, sum( consensus >= nb_species_consensus ) ) ## 30 Pour papier Spliced Variants
  print(table( consensus >= nb_species_consensus )) ## 30 Pour papier Spliced Variants

  for (species in sequence_dict$species) {
    if (species %in% sapply(ids.fasta_seq,function(x) strsplit(x,"_busco")[[1]][1])) {
      id = ids.fasta_seq[which(species == sapply(ids.fasta_seq,function(x) strsplit(x,"_busco")[[1]][1]))]
      sequence_dict[species, 'sequence'][[1]] = list(append(sequence_dict[species, 'sequence'][[1]], fasta_seq[[id]]))
      sequence_dict[species, 'sequence'][[1]] = list(sequence_dict[species, 'sequence'][[1]][sequence_dict[species, 'sequence'][[1]] != " "])


      sequence_dict[species, 'sequence_consensus'][[1]] = list(append(sequence_dict[species, 'sequence_consensus'][[1]], fasta_seq[[id]][which(consensus >= nb_species_consensus)]))
      sequence_dict[species, 'sequence_consensus'][[1]] = list(sequence_dict[species, 'sequence_consensus'][[1]][sequence_dict[species, 'sequence_consensus'][[1]] != " "])
    } else {
      id_length = ids.fasta_seq[1]
      sequence_dict[species, 'sequence'][[1]] = list(append(sequence_dict[species, 'sequence'][[1]], rep('-', length(fasta_seq[[id_length]]))))
      sequence_dict[species, 'sequence_consensus'][[1]] = list(append(sequence_dict[species, 'sequence_consensus'][[1]], rep('-', sum( consensus >= nb_species_consensus ))))
    }
  }
}

print(list.gene)
print(size.gene)
print(size.gene_consensus)
print(list.sp.by.gene)

write.fasta(sequence_dict$sequence, sequence_dict$species, file.out = concatenatePath_raxml)
write.fasta(lapply(sequence_dict$sequence,translate), sequence_dict$species, file.out = concatenatePathAAS_raxml)
write.fasta(sequence_dict$sequence_consensus, sequence_dict$species, file.out = concatenatePath_raxmlcons)
write.fasta(lapply(sequence_dict$sequence_consensus,translate), sequence_dict$species, file.out = concatenatePathAAS_raxmlcons)

info.table = data.frame(gene.id = list.gene,
                        gene.size = size.gene,
                        gene_cons.size = size.gene_consensus,
                        nb.sp = list.sp.by.gene)

info.table = rbind(info.table,data.frame(
  gene.id=paste("No genes:",length(list.gene),
                "  total size:",print(length(sequence_dict[sapply(rownames(sequence_dict)[1],function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]])),"pb"),
  gene.size="",
  gene_cons.size="",
  nb.sp=""))
print(info.table)
write.table(info.table , infoPath_raxml , row.names=F, col.names=T, sep="\t", quote=F)
