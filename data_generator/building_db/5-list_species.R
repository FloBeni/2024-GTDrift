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


list_species = data.frame(sp_txid = list.dirs("database/BUSCO_annotations/",recursive = F,full.names = F))
list_species$species = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][1])
list_species$NCBI.txid = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][2])
rownames(list_species) = list_species$species

species_clade = data.frame()
for (file in c(
  "/home/fbenitiere/Documents/embryophyta_species.xls",
  "/home/fbenitiere/Documents/metazoa_requisit_for_dnds.xls",
  "/home/fbenitiere/Documents/metazoa_species.xls",
  "/home/fbenitiere/Documents/metazoa_species2.xls"
)){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)) {
    species_clade = rbind(species_clade,data.frame(
      species,
      clade = mysheets[[species]]$Clade[1]
    ))
  }
}
rownames(species_clade) = species_clade$species
list_species$clade = species_clade[list_species$species , ]$clade

table_ncbi = data.frame()
sapply(list_species$species,function(x) {
  dt = read.delim(paste("/home/fbenitiere/data/Projet-SplicedVariants/Annotations/",x,"/taxonomy_ncbi.tab",sep=""))
  dt$species = x
  table_ncbi <<- rbind(table_ncbi,dt)
})

list_species$clade_group = "Other Invertebrates"
list_species[ table_ncbi[table_ncbi$name == "Vertebrata",]$species,]$clade_group = "Other Vertebrates"
list_species[ table_ncbi[table_ncbi$name == "Insecta",]$species,]$clade_group = "Other Insecta"
list_species[ table_ncbi[table_ncbi$name %in% c("Diptera","Lepidoptera"),]$species,]$clade_group = "Lepido Diptera"
list_species[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade_group =
  list_species[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade


list_species$expression_data = list_species$species %in% list.dirs("/home/fbenitiere/data/Projet-SplicedVariants/Analyses",recursive = F,full.names = F)

dnds_data = data.frame()
for (file in list.files(paste("database/dNdS",sep=""),recursive = F,full.names = T,pattern=".tab")){
  dnds_data = rbind(dnds_data,read.delim(file,header = T))
}

list_species$dnds_data = list_species$species %in% dnds_data$species
list_species = list_species[,-c(1)]


lht_tab = read.delim("database/lht.tab")
list_species$lht_data = list_species$species %in% lht_tab$species


write.table(list_species , paste("database/list_species.tab",sep=""),quote=F,row.names = F,sep="\t")
