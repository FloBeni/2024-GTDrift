
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
  "data/life_history_traits/metazoa.xls",
  "data/life_history_traits/embryophyta.xls"
)){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)) {
    print(species)
    study = mysheets[[species]]$Group_study[1]
    NCBI.taxid = mysheets[[species]]$NCBI.taxid[1]
    if (length(mysheets[[species]]$`Longevity (days)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        NCBI.taxid ,
        value = as.numeric(mysheets[[species]]$`Longevity (days)`),
        db = mysheets[[species]]$`Ref longevity`,
        life_history_traits="lifespan_days"
      ))
    }
    
    if (length(mysheets[[species]]$`Length (cm)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        NCBI.taxid ,
        value = as.numeric(mysheets[[species]]$`Length (cm)`),
        db = mysheets[[species]]$`Ref length`,
        life_history_traits="length_cm"
      ))
    }
    
    if (length(mysheets[[species]]$`Weight (kg)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        NCBI.taxid ,
        value = as.numeric(mysheets[[species]]$`Weight (kg)`),
        db = mysheets[[species]]$`Ref weight`,
        life_history_traits="weight_kg"
      ))
    }
  }
}
species_clade = species_clade[!is.na(species_clade$value),]

species_clade = species_clade[order(species_clade$value , decreasing = T) , ]



write.table(species_clade , paste("data/life_history_traits/all_life_history_traits.tab",sep=""),quote=F,row.names = F,sep="\t")

