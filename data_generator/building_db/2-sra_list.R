# Generate Data 10
options(stringsAsFactors = F, scipen = 999)
library(readxl)

list_species = data.frame(sp_txid = list.dirs("database/Transcriptomic/",recursive = F,full.names = F))
list_species$species = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][1])
list_species$NCBI.txid = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][2])
rownames(list_species) = list_species$species

sra_tab = data.frame()
for (species in sp_studied ){
  table_sra = read.delim(paste("/home/fbenitiere/data/Projet-SplicedVariants/Annotations/",species,"/SRAruninfo.tab",sep=""))
  table_sra$species = species
  sra_tab = rbind(sra_tab , table_sra)
}

write.table(sra_tab,paste("data/sra.tab",sep=""), row.names=F, col.names=T, sep="\t", quote=F)
