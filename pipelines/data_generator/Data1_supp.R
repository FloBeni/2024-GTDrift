# Generate Data 1
options(stringsAsFactors = F, scipen = 999)
library(stringr)
library(ape)

data1 = read.delim("database/list_species.tab")
rownames(data1) = data1$species

data_lht = read.delim("database/life_history_traits.tab")
rownames(data_lht) = paste(data_lht$species,data_lht$life_history_traits)

data1$max_lifespan_days = data_lht[paste(data1$species,"lifespan_days"),]$value
data1$max_length_cm = data_lht[paste(data1$species,"length_cm"),]$value
data1$max_weight_kg = data_lht[paste(data1$species,"weight_kg"),]$value

out_busco = apply(data1,1,function(x){  
  busco_tab = read.delim(paste("database/BUSCO_annotations/",x["species"],"_NCBI.taxid",as.numeric(x["NCBI.taxid"]),"/",x["assembly_accession"],"/busco_to_gene_id_eukaryota.gz",sep=""))
  
  busco_to_gene = busco_tab[!(duplicated(busco_tab$busco_id,fromLast = FALSE) | duplicated(busco_tab$busco_id,fromLast = TRUE)) &
                                  !(duplicated(busco_tab$gene_id,fromLast = FALSE) | duplicated(busco_tab$gene_id,fromLast = TRUE)) ,]
  
  print(c(length(unique(busco_tab$busco_id)) , length(busco_to_gene$busco_id) ))
})

data1[,c("nb_busco_gene_eukaryota","nb_busco_gene_unique_eukaryota")] = t(data.frame(out_busco))


write.table(data1 , "data/data1.tab",quote=F,row.names = F,sep="\t")

