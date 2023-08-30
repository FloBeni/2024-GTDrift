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
  "data/lht_collect/metazoa.xls",
  "data/lht_collect/embryophyta.xls"
)){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)) {
    species_clade = rbind(species_clade,data.frame(
      species,
      clade = mysheets[[species]]$Clade[1],
      NCBI.txid = mysheets[[species]]$NCBI.txid[1]
    ))
  }
}
rownames(species_clade) = species_clade$species

table_ncbi = read.delim(paste("database/taxonomy.tab",sep=""))

species_clade$clade_group = "Other Invertebrates"
species_clade[ table_ncbi[table_ncbi$name == "Vertebrata",]$species,]$clade_group = "Other Vertebrates"
species_clade[ table_ncbi[table_ncbi$name == "Insecta",]$species,]$clade_group = "Other Insecta"
species_clade[ table_ncbi[table_ncbi$name %in% c("Diptera","Lepidoptera"),]$species,]$clade_group = "Lepido Diptera"
species_clade[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade_group =
  species_clade[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade


species_clade$expression_data = species_clade$species %in% list.dirs("/home/fbenitiere/data/Projet-SplicedVariants/Analyses",recursive = F,full.names = F)

dnds_data = data.frame()
for (file in list.files(paste("database/dNdS",sep=""),recursive = F,full.names = T,pattern=".tab")){
  dnds_data = rbind(dnds_data,read.delim(file,header = T))
}

species_clade$dnds_data = species_clade$species %in% dnds_data$species
species_clade = species_clade[,-c(1)]


lht_tab = read.delim("database/lht.tab")
species_clade$lht_data = species_clade$species %in% lht_tab$species


write.table(species_clade , paste("database/list_species.tab",sep=""),quote=F,row.names = F,sep="\t")
