# Extract from the original excel files the relevant data.
options(stringsAsFactors = F, scipen = 999)
options(java.parameters = "-Xmx1024m")
library(stringr)
library(xlsx)

list_species = data.frame(sp_taxid = list.dirs("database/BUSCO_annotations/",recursive = F,full.names = F)) # Get species.
list_species$species = sapply(list_species$sp_taxid,function(x) str_split(x,"_NCBI.taxid")[[1]][1]) # Extract scientific name.
list_species$NCBI.taxid = sapply(list_species$sp_taxid,function(x) str_split(x,"_NCBI.taxid")[[1]][2]) # Extract NCBI taxID.
rownames(list_species) = list_species$species

read_excel_allsheets <- function(filename, tibble = FALSE) { # Function to read excel files.
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# For embryophyta.
excel_file = "data/life_history_traits/embryophyta.xls"
for (file in c(
  "/home/fbenitiere/Documents/embryophyta_species.xls")){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)){
    if (species %in% list_species$species){print(species)
      dt_sample = mysheets[[species]]
      dt_sample$NCBI.taxid = NA
      dt_sample$NCBI.taxid[1] = list_species[species,]$NCBI.taxid
      dt_sample = dt_sample[,c("Species","NCBI.taxid","Socialite","Clade", "Length (cm)","Ref length","Longevity (days)", "Ref longevity","Weight (kg)","Ref weight")]
      write.xlsx2(dt_sample,excel_file,sheetName = species,append=T,row.names = F) # Save the excel file.
    }
  }
}


# For metazoa.
excel_file = "data/life_history_traits/metazoa.xls"

for (file in c(
  "/home/fbenitiere/Documents/metazoa_requisit_for_dnds.xls",
  "/home/fbenitiere/Documents/metazoa_species.xls",
  "/home/fbenitiere/Documents/metazoa_species2.xls")){
  mysheets <- read_excel_allsheets(file)
  print(file)
  for (species in names(mysheets)){
    if (species %in% list_species$species){print(species)
      dt_sample = mysheets[[species]]
      dt_sample$NCBI.taxid = NA
      dt_sample$NCBI.taxid[1] = list_species[species,]$NCBI.taxid
      dt_sample = dt_sample[,c("Species","NCBI.taxid","Socialite","Clade", "Length (cm)","Ref length","Longevity (days)", "Ref longevity","Weight (kg)","Ref weight")]
      dt_sample = dt_sample[rowSums(is.na(dt_sample))!= ncol(dt_sample),]
      write.xlsx2(dt_sample,excel_file,sheetName = species,append=T,row.names = F) # Save the excel file.
    }
  }
}
