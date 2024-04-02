# Generate Data 1
options(stringsAsFactors = F, scipen = 999)
library(stringr)
library(ape)

data1 = read.delim("database/list_species.tab")
rownames(data1) = data1$species

data_lht = read.delim("database/life_history_traits_and_polymorphism_derived_Ne.tab")
rownames(data_lht) = data_lht$species

data1$max_lifespan_days = data_lht[data1$species,"lifespan_days"]
data1$max_length_cm = data_lht[data1$species,"length_cm"]
data1$max_mass_kg = data_lht[data1$species,"mass_kg"]
data1$polymorphism_derived_Ne = data_lht[data1$species,"polymorphism_derived_Ne"]

out_busco = apply(data1,1,function(x){  
  busco_tab = read.delim(paste("database/BUSCO_annotations/",x["species"],"_NCBI.taxid",as.numeric(x["NCBI.taxid"]),"/",x["assembly_accession"],"/busco_to_gene_id_eukaryota.gz",sep=""))
  
  busco_to_gene = busco_tab[!(duplicated(busco_tab$busco_id,fromLast = FALSE) | duplicated(busco_tab$busco_id,fromLast = TRUE)) &
                                  !(duplicated(busco_tab$gene_id,fromLast = FALSE) | duplicated(busco_tab$gene_id,fromLast = TRUE)) ,]
  
  print(c(length(unique(busco_tab$busco_id)) , length(busco_to_gene$busco_id) ))
})

data1[,c("nb_busco_gene_eukaryota","nb_busco_gene_unique_eukaryota")] = t(data.frame(out_busco))


write.table(data1 , "data/data1.tab",quote=F,row.names = F,sep="\t")

