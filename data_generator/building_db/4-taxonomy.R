

library(taxize)


list_species = list.dirs("/home/fbenitiere/LBBE-Projects/Projet SplicedVariants/article/2024-icasdb/database/BUSCO_annotations",recursive = F,full.names = F)

taxonomy = data.frame()
for (species in list_species){
  taxID =  get_uid_(str_split(species,"_NCBI.txid")[[1]][1])[[1]]["uid"] # Get TaxID
  table_ncbi = classification(taxID[[1]], db = 'ncbi')[[1]] # Get taxonomy
  table_ncbi$rank_no = rownames(table_ncbi)
  table_ncbi$species = str_split(species,"_NCBI.txid")[[1]][1]
  table_ncbi = table_ncbi[,c("species","rank_no","name","rank","id")]
  
  taxonomy = rbind(taxonomy,table_ncbi)
}

write.table(max_lht , paste("database/taxonomy.tab",sep=""),quote=F,row.names = F,sep="\t")