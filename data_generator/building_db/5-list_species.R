options(stringsAsFactors = F, scipen = 999)
library(readxl)
library(xlsx)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


species_clade = data.frame()
for (file in c(
  "data/life_history_traits/metazoa.xls",
  "data/life_history_traits/embryophyta.xls"
)){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)) {
    species_clade = rbind(species_clade,data.frame(
      species,
      clade = mysheets[[species]]$Clade[1],
      NCBI.taxid = mysheets[[species]]$NCBI.taxid[1]
    ))
  }
}
rownames(species_clade) = species_clade$species

table_ncbi = read.delim(paste("data/taxonomy.tab",sep=""))

species_clade$clade_group = "Other Invertebrates"
species_clade[ table_ncbi[table_ncbi$name == "Vertebrata",]$species,]$clade_group = "Other Vertebrates"
species_clade[ table_ncbi[table_ncbi$name == "Insecta",]$species,]$clade_group = "Other Insecta"
species_clade[ table_ncbi[table_ncbi$name %in% c("Diptera","Lepidoptera"),]$species,]$clade_group = "Lepido Diptera"
species_clade[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade_group =
  species_clade[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade


species_clade$expression_data = species_clade$species %in% list.dirs("database/Transcriptomic",recursive = F,full.names = F)
list_trnascriptomic = list.dirs(paste("database/Transcriptomic/",sep=""),recursive = F,full.names = F)
species_clade$expression_data = sapply(paste(species_clade$species,"_NCBI.taxid",species_clade$NCBI.taxid,sep=""),function(x) x %in% list_trnascriptomic )


dnds_data = data.frame()
for (file in list.files(paste("database/dNdS",sep=""),recursive = F,full.names = T,pattern=".tab")){
  dnds_data = rbind(dnds_data,read.delim(file,header = T))
}

species_clade$dnds_data = species_clade$species %in% dnds_data$species

lht_tab = read.delim("database/life_history_traits.tab")
species_clade$lht_data = species_clade$species %in% lht_tab$species

species_clade$assembly_accession = sapply(paste(species_clade$species,"_NCBI.taxid",species_clade$NCBI.taxid,sep=""),function(x)  list.dirs(paste("database/BUSCO_annotations/",x,sep=""),recursive = F,full.names = F) )
species_clade$nb_rnaseq = NA
species_clade$nb_rnaseq = apply(species_clade,1,
                                function(x) length( list.dirs(paste("database/Transcriptomic/",x["species"],"_NCBI.taxid",x["NCBI.taxid"],"/",x["assembly_accession"],"/Run",sep=""),recursive = F,full.names = F))
)

write.table( species_clade , paste("database/list_species.tab",sep=""),quote=F,row.names = F,sep="\t")
