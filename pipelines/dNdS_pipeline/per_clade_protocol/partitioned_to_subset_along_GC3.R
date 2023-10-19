# Créer des concatenats de plus de 200000 sites

#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))
options(stringsAsFactors = F, scipen = 999)
library(seqinr)

args = (commandArgs(TRUE))
prank_clade_dir = args[1]
clade = args[2]
output_path = args[3]
# data_sequence = "/home/fbenitiere/data/Projet-SplicedVariants/DnDs/test_v12/GC_content.tab"
data_sequence = args[4]
# candidate_species = "/home/fbenitiere/data/Projet-SplicedVariants/DnDs/test_v12/candidate_species"
candidate_species = args[5]
concatenatePathAAS_raxmlcons = args[6]
nb_sites = as.numeric(args[7])

candidate_species = read.delim(candidate_species,header=T)
rownames(candidate_species) = candidate_species$species

data_sequence = read.table(data_sequence,header=T)
table(duplicated(paste(data_sequence$species,data_sequence$busco_gene)))

data_sequence$clade_group = candidate_species[data_sequence$species,]$clade_group

data_sequence = data_sequence[data_sequence$clade == data_sequence$clade_group , ]

data_sequence = data_sequence[data_sequence$species_filtered , ]


concatenate_aas = read.fasta(concatenatePathAAS_raxmlcons)
list_species = names(concatenate_aas)

gene_dt = data.frame(
  busco_gene = names(tapply(data_sequence$GC3_ratio,data_sequence$busco_gene,mean)),
  mean_gc3 = tapply(data_sequence$GC3_ratio,data_sequence$busco_gene,mean),
  length_prank = tapply(data_sequence$length_prank,data_sequence$busco_gene,mean),
  med_AA_length = tapply(data_sequence$AA_length,data_sequence$busco_gene,median)
)

gene_dt$name_align = gene_dt$busco_gene
rownames(gene_dt) = gene_dt$name_align


k = nb_sites / mean(gene_dt$length_prank)
nb_group = floor(nrow(gene_dt)/k)

group = rep(1:nb_group,500)[1:nrow(gene_dt)]
gene_dt = gene_dt[order(gene_dt$mean_gc3),]
gene_dt$concatenate = group[order(group)]

all.files = gene_dt$busco_gene

dir.create(paste(output_path,sep=""), showWarnings = FALSE)

for (replicate.id in unique(gene_dt$concatenate)){
  dir.create(paste(output_path, replicate.id,sep=""), showWarnings = FALSE)
  output.file = paste(output_path, replicate.id,"/concatenatCDS.aln", sep = "")
  info.file = paste(output_path, replicate.id,"/info", sep = "")



  prot = data.frame(protein = list_species)
  rownames(prot) = prot$protein
  list.gene = c()
  size.gene = c()
  GC3.gene = c()
  list.sp.by.gene = c()

  for (file in gene_dt[gene_dt$concatenate == replicate.id,]$name_align){
    print(length(prot[sapply(rownames(prot)[1],function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]]))
    list.gene=append(list.gene,substr(file,1,11))
    protein = read.fasta(paste(prank_clade_dir , file,'/' , clade , ".fa-prank.aln", sep = ""), seqtype = "DNA")
    ids.protein = names(protein)

    list.sp.by.gene = append(list.sp.by.gene,length(ids.protein))
    size.gene = append(size.gene, length(protein[[ids.protein[1]]]))
    GC3.gene = append(GC3.gene,gene_dt[file,"mean_gc3"] )

    for (name in prot$protein) {
      if (name %in% sapply(ids.protein,function(x) strsplit(x,"_busco")[[1]][1])) {
        id = ids.protein[which(name == sapply(ids.protein,function(x) strsplit(x,"_busco")[[1]][1]))]
        prot[sapply(id,function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]] = list(append(prot[sapply(id,function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]], protein[[id]]))
        prot[sapply(id,function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]] = list(prot[sapply(id,function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]][prot[sapply(id,function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]] != " "])
      } else {
        id_length = ids.protein[1]
        prot[name, 'sequence'][[1]] = list(append(prot[name, 'sequence'][[1]], rep('-', length(protein[[id_length]]))))
      }
    }
  }

  print(list.gene)
  print(size.gene)
  print(list.sp.by.gene)

  write.fasta(prot$sequence, prot$protein, file.out = output.file)

  info.table=data.frame(gene.id=list.gene,
                        gene.size.nucl=size.gene,
                        average.GC3 = GC3.gene,
                        nb.sp=list.sp.by.gene)

  info.table=rbind(info.table,data.frame(
    gene.id=paste("No genes:",length(list.gene),
                  "  total size:",print(length(prot[sapply(rownames(prot)[1],function(x) strsplit(x,"_busco")[[1]][1]), 'sequence'][[1]])),"pb"),
    gene.size.nucl="",
    average.GC3="",
    nb.sp=""))

  write.table(info.table,info.file, row.names=F, col.names=T, sep="\t", quote=F)
}
