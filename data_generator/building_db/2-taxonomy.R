

# library(taxize)


list_species = data.frame(sp_txid = list.dirs("database/BUSCO_annotations/",recursive = F,full.names = F))
list_species$species = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][1])
list_species$NCBI.txid = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][2])
rownames(list_species) = list_species$species

taxonomy = data.frame()
for (species in list_species$species){
  # taxID =  get_uid_(species)[[1]]["uid"] # Get TaxID
  # table_ncbi = classification(taxID[[1]], db = 'ncbi')[[1]] # Get taxonomy
  table_ncbi = read.delim(paste("/home/fbenitiere/data/Projet-SplicedVariants/Annotations/",species,"/taxonomy_ncbi.tab",sep=""))
  table_ncbi$rank_no = rownames(table_ncbi)
  table_ncbi$species = species
  table_ncbi = table_ncbi[,c("species","rank_no","name","rank","id")]
  
  taxonomy = rbind(taxonomy,table_ncbi)
}

write.table(taxonomy , paste("database/taxonomy.tab",sep=""),quote=F,row.names = F,sep="\t")
