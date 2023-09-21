options(stringsAsFactors = F, scipen = 999)
options(java.parameters = "-Xmx1024m")
library(stringr)
library(xlsx)

list_species = data.frame(sp_txid = list.dirs("database/BUSCO_annotations/",recursive = F,full.names = F))
list_species$species = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][1])
list_species$NCBI.txid = sapply(list_species$sp_txid,function(x) str_split(x,"_NCBI.txid")[[1]][2])
rownames(list_species) = list_species$species

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # fonction de lecture de fichier excel
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

excel_file = "data/lht_collect/metazoa.xls"

for (file in c(
  "/home/fbenitiere/Documents/metazoa_requisit_for_dnds.xls",
  "/home/fbenitiere/Documents/metazoa_species.xls",
  "/home/fbenitiere/Documents/metazoa_species2.xls")){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)[368:500]){
    if (species %in% list_species$species){print(species)
      dt_sample = mysheets[[species]]
      dt_sample$NCBI.txid = NA
      dt_sample$NCBI.txid[1] = list_species[species,]$NCBI.txid
      dt_sample = dt_sample[,c("Species","NCBI.txid","Socialite","Clade", "Length (cm)","Ref length","Longevity (days)", "Ref longevity","Weight (kg)","Ref weight")]
      
      dt_sample = dt_sample[rowSums(is.na(dt_sample))!= ncol(dt_sample),]
      
      write.xlsx2(dt_sample,excel_file,sheetName = species,append=T,row.names = F)
    }
  }
}

excel_file = "data/lht_collect/embryophyta.xls"
for (file in c(
  "/home/fbenitiere/Documents/embryophyta_species.xls")){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)){
    if (species %in% list_species$species){print(species)
      dt_sample = mysheets[[species]]
      dt_sample$NCBI.txid = NA
      dt_sample$NCBI.txid[1] = list_species[species,]$NCBI.txid
      dt_sample = dt_sample[,c("Species","NCBI.txid","Socialite","Clade", "Length (cm)","Ref length","Longevity (days)", "Ref longevity","Weight (kg)","Ref weight")]
      write.xlsx2(dt_sample,excel_file,sheetName = species,append=T,row.names = F)
    }
  }
}
