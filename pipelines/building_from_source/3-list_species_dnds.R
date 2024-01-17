# Identify to which 'clade_dnds' each species belongs. This is used for the dNdS calculation parallelization.
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


# Extract every species, the NCBI taxID and the clade.
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

# Identify to which 'clade_dnds' each species belongs. This is used for the dNdS calculation parallelization.
table_ncbi = read.delim(paste("data/taxonomy.tab",sep=""))
species_clade$clade_dnds = "Other Invertebrates"
species_clade[ table_ncbi[table_ncbi$name == "Vertebrata",]$species,]$clade_dnds = "Other Vertebrates"
species_clade[ table_ncbi[table_ncbi$name == "Insecta",]$species,]$clade_dnds = "Other Insecta"
species_clade[ table_ncbi[table_ncbi$name %in% c("Diptera","Lepidoptera"),]$species,]$clade_dnds = "Mecopterida"
species_clade[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade_dnds =
  species_clade[ table_ncbi[table_ncbi$name %in% c("Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade

write.table( species_clade , paste("data/dnds_phylo/list_species_dnds.tab",sep=""),quote=F,row.names = F,sep="\t") # Save the table.
