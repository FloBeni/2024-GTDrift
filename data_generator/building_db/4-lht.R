
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
list_sp = c()
for (file in c(
  "data/lht_collect/metazoa.xls",
  "data/lht_collect/embryophyta.xls"
)){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)) {
    print(species)
    study = mysheets[[species]]$Group_study[1]
    NCBI.txid = mysheets[[species]]$NCBI.txid[1]
    if (length(mysheets[[species]]$`Longevity (days)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        NCBI.txid ,
        max_value = as.numeric(mysheets[[species]]$`Longevity (days)`),
        db = mysheets[[species]]$`Ref longevity`,
        lht="lifespan_days"
      ))
    }
    
    if (length(mysheets[[species]]$`Length (cm)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        NCBI.txid ,
        max_value = as.numeric(mysheets[[species]]$`Length (cm)`),
        db = mysheets[[species]]$`Ref length`,
        lht="length_cm"
      ))
    }
    
    if (length(mysheets[[species]]$`Weight (kg)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        NCBI.txid ,
        max_value = as.numeric(mysheets[[species]]$`Weight (kg)`),
        db = mysheets[[species]]$`Ref weight`,
        lht="weight_kg"
      ))
    }
  }
}
species_clade = species_clade[!is.na(species_clade$max_value),]

species_clade = species_clade[order(species_clade$max_value , decreasing = T) , ]

max_clade = tapply(species_clade$max_value,paste(species_clade$species,species_clade$lht),max)
species_clade = species_clade[max_clade[paste(species_clade$species,species_clade$lht)] == species_clade$max_value,]

max_lht = species_clade[!duplicated(paste(species_clade$lht , species_clade$species)) , ]
max_lht$db = tapply(species_clade$db,paste(species_clade$species , species_clade$lht) , function(x) paste(x,collapse = ";"))[paste(max_lht$species,max_lht$lht)]


write.table(max_lht , paste("database/lht.tab",sep=""),quote=F,row.names = F,sep="\t")

