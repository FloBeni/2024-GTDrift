# Collect the entire taxonomy from the NCBI for each species.
library(stringr)

list_species = data.frame(sp_taxid = list.dirs("database/BUSCO_annotations/",recursive = F,full.names = F)) # Get species.
list_species$species = sapply(list_species$sp_taxid,function(x) str_split(x,"_NCBI.taxid")[[1]][1]) # Extract scientific name.
rownames(list_species) = list_species$species

taxonomy = data.frame() # Initiate the table.
for (species in list_species$species){print(species)
  # taxID =  get_uid_(species)[[1]]["uid"] # Get TaxID.
  # table_ncbi = classification(taxID[[1]], db = 'ncbi')[[1]] # Get taxonomy.
  table_ncbi = read.delim(paste("/home/fbenitiere/data/Projet-SplicedVariants/Annotations/",species,"/taxonomy_ncbi.tab",sep="")) # Extract the taxonomy already saved from the two previous command lines.
  table_ncbi$rank_no = rownames(table_ncbi)
  table_ncbi$species = species
  table_ncbi = table_ncbi[,c("species","rank_no","name","rank","id")]
  
  taxonomy = rbind(taxonomy,table_ncbi) # Add lines to the table.
}

write.table(taxonomy , paste("data/taxonomy.tab",sep=""),quote=F,row.names = F,sep="\t") # Save the table
