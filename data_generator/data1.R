library(stringr)
library(ape)

data1 = read.delim("database/list_species.tab")

data_lht = read.delim("database/lht.tab")
rownames(data_lht) = paste(data_lht$species,data_lht$lht)

data1$max_lifespan_days = data_lht[paste(data1$species,"lifespan_days"),]$max_value
data1$max_length_cm = data_lht[paste(data1$species,"length_cm"),]$max_value
data1$max_weight_kg = data_lht[paste(data1$species,"weight_kg"),]$max_value


data1$nb_rnaseq = NA
data1[data1$expression_data,]$nb_rnaseq = sapply(data1[data1$expression_data,]$species,
                                                 function(x) length(read.delim(paste("/home/fbenitiere/data/Projet-SplicedVariants/RNAseq_table/",x,"/list_Acc.tab",sep=""))$SRA_accession_ID)
)

# table(tapply(dt$TaxID,dt$ScientificName,function(x) length(unique(x))))
# table(tapply(dt$ScientificName,dt$TaxID,function(x) length(unique(x))))


data1$nb_busco_gene_eukaryota = sapply(paste(data1$species,data1$NCBI.txid,sep="_NCBI.txid"),function(species){   print(species)
  if (file.exists(paste("database/BUSCO_annotations/",species,"/busco_to_gene_id_eukaryota",sep=""))){
    busco_tab = read.delim(paste("database/BUSCO_annotations/",species,"/busco_to_gene_id_eukaryota",sep=""))
    length(unique(busco_tab$busco_id)) 
  } else {return(NA)}
})

data1$nb_busco_gene_unique_eukaryota =  sapply(paste(data1$species,data1$NCBI.txid,sep="_NCBI.txid"),function(species){   print(species)
  if (file.exists(paste("database/BUSCO_annotations/",species,"/busco_to_gene_id_eukaryota",sep=""))){
    busco_tab = read.delim(paste("database/BUSCO_annotations/",species,"/busco_to_gene_id_eukaryota",sep=""))
    sum(table(busco_tab$busco_id) == 1 )
  } else {return(NA)}
})



write.table(data1 , "data/data1.tab",quote=F,row.names = F,sep="\t")

# table(!is.na(data1$max_lifespan_days)|!is.na(data1$max_length_cm)|!is.na(data1$max_weight_kg))
# table(data1$expression_data)
# table(data1$dnds_data)
