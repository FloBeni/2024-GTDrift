library(stringr)
library(ape)

data1 = read.delim("database/list_species.tab")
rownames(data1) = data1$species

data_lht = read.delim("database/lht.tab")
rownames(data_lht) = paste(data_lht$species,data_lht$lht)

data1$max_lifespan_days = data_lht[paste(data1$species,"lifespan_days"),]$max_value
data1$max_length_cm = data_lht[paste(data1$species,"length_cm"),]$max_value
data1$max_weight_kg = data_lht[paste(data1$species,"weight_kg"),]$max_value


# data1$nb_rnaseq = NA
# data1[data1$expression_data,]$nb_rnaseq = sapply(data1[data1$expression_data,]$species,
#                                                  function(x) length(read.delim(paste("/home/fbenitiere/data/Projet-SplicedVariants/RNAseq_table/",x,"/list_Acc.tab",sep=""))$SRA_accession_ID)
# )

# table(tapply(dt$TaxID,dt$ScientificName,function(x) length(unique(x))))
# table(tapply(dt$ScientificName,dt$TaxID,function(x) length(unique(x))))

out_busco = apply(data1,1,function(x){   print(x)
  busco_tab = read.delim(paste("database/BUSCO_annotations/",x["species"],"_NCBI.txid",x["NCBI.txid"],"/",x["assembly_accession"],"/busco_to_gene_id_eukaryota",sep=""))
  c(length(unique(busco_tab$busco_id)) , sum(table(busco_tab$busco_id) == 1 ))
})

data1[,c("nb_busco_gene_eukaryota","nb_busco_gene_unique_eukaryota")] = t(data.frame(out_busco))


write.table(data1 , "data/data1.tab",quote=F,row.names = F,sep="\t")

# table(!is.na(data1$max_lifespan_days)|!is.na(data1$max_length_cm)|!is.na(data1$max_weight_kg))
# table(data1$expression_data)
# table(data1$dnds_data)


