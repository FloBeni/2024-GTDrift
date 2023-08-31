options(stringsAsFactors = F, scipen = 999)
library(readxl)
library(stringr)

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


# pathData="/home/XXXXX/data/Projet-SplicedVariants/"
# pathData="/beegfs/data/XXXXX/Projet-SplicedVariants/"

species_clade = data.frame()
list_sp = c()
for (file in c(
  "data/lht_collect/metazoa.xls",
  "data/lht_collect/embryophyta.xls"
)){
  mysheets <- read_excel_allsheets(file)
  list_sp = append(list_sp,names(mysheets))
  for (species in names(mysheets)) {
    print(species)
    clade = mysheets[[species]]$Clade[1]
    if (length(mysheets[[species]]$`Longevity (days)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        clade ,
        value = as.numeric(mysheets[[species]]$`Longevity (days)`),
        ref = mysheets[[species]]$`Ref longevity`,
        db = as.character(sapply(mysheets[[species]]$`Ref longevity`,function(x) str_split(x," ")[[1]][1])),
        categorie="lifespan"
      ))
    }
    
    if (length(mysheets[[species]]$`Length (cm)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        clade ,
        value = as.numeric(mysheets[[species]]$`Length (cm)`),
        ref = mysheets[[species]]$`Ref length`,
        db = as.character(sapply(mysheets[[species]]$`Ref length`,function(x) str_split(x," ")[[1]][1])),
        categorie="length"
      ))
    }
    
    if (length(mysheets[[species]]$`Weight (kg)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        clade ,
        value = as.numeric(mysheets[[species]]$`Weight (kg)`),
        ref = mysheets[[species]]$`Ref weight`,
        db = as.character(sapply(mysheets[[species]]$`Ref weight`,function(x) str_split(x," ")[[1]][1])),
        categorie="weight"
      ))
    }
    
    species_clade = species_clade[!is.na(species_clade$db),]
  }
}


list_species = read.table( paste("database/list_species.tab",sep=""),header = T,sep="\t")
rownames(list_species) = list_species$species
species_clade$clade_group = list_species[species_clade$species,]$clade_group
species_clade = species_clade[species_clade$species %in%  list_species$species,]


write.table(species_clade , "data/data3.tab",quote=F,row.names = F,sep="\t")


