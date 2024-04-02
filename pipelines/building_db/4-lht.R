# Collect the maximum values for each life history trait.
options(stringsAsFactors = F, scipen = 999)
species_clade = read.delim(paste("data/life_history_traits/all_life_history_traits.tab",sep="")) # Download all life history traits.

max_clade = tapply(species_clade$value,paste(species_clade$species,species_clade$life_history_traits),max) # Keep the maximum per species and per life history traits.
species_clade = species_clade[max_clade[paste(species_clade$species,species_clade$life_history_traits)] == species_clade$value,]

max_lht = species_clade[!duplicated(paste(species_clade$life_history_traits , species_clade$species)) , ]
max_lht$db = tapply(species_clade$db,paste(species_clade$species , species_clade$life_history_traits) , function(x) paste(x,collapse = ";"))[paste(max_lht$species,max_lht$life_history_traits)] # Get all the db that refer this maximum value.


data_lifehistory_traits = data.frame(species = unique(species_clade$species))
mass_dt = max_lht[max_lht$life_history_traits == "mass_kg",]
rownames(mass_dt) = mass_dt$species
data_lifehistory_traits$mass_kg = mass_dt[data_lifehistory_traits$species,]$value
data_lifehistory_traits$mass_db = mass_dt[data_lifehistory_traits$species,]$db

mass_dt = max_lht[max_lht$life_history_traits == "lifespan_days",]
rownames(mass_dt) = mass_dt$species
data_lifehistory_traits$lifespan_days = mass_dt[data_lifehistory_traits$species,]$value
data_lifehistory_traits$lifespan_db = mass_dt[data_lifehistory_traits$species,]$db

mass_dt = max_lht[max_lht$life_history_traits == "length_cm",]
rownames(mass_dt) = mass_dt$species
data_lifehistory_traits$length_cm = mass_dt[data_lifehistory_traits$species,]$value
data_lifehistory_traits$length_db = mass_dt[data_lifehistory_traits$species,]$db

# Generate a list of the species within the database and which characteristics is available from excel original excel files.
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


# Extract every species, the NCBI taxID and the clade. Detect the assembly accession used from the GFF.
species_clade = data.frame()
for (file in c(
  "data/life_history_traits/metazoa.xls",
  "data/life_history_traits/embryophyta.xls"
)){
  mysheets <- read_excel_allsheets(file)
  for (species in names(mysheets)) {
    species_clade = rbind(species_clade,data.frame(
      species,
      NCBI.taxid = mysheets[[species]]$NCBI.taxid[1]
    ))
  }
}

data_lifehistory_traits = merge.data.frame(x=species_clade,y=data_lifehistory_traits, by = "species" ,all=T)

supp_lynch_2023 = read.delim(paste("data/supp_lynch_2023.tab",sep=""),comment="#") # Download all life history traits.
rownames(supp_lynch_2023) = supp_lynch_2023$Species

data_lifehistory_traits$polymorphism_derived_Ne = supp_lynch_2023[data_lifehistory_traits$species,]$Ne

data_lifehistory_traits = data_lifehistory_traits[!is.na(data_lifehistory_traits$lifespan_days) | 
                                                    !is.na(data_lifehistory_traits$length_cm) |
                                                    !is.na(data_lifehistory_traits$mass_kg) |
                                                    !is.na(data_lifehistory_traits$polymorphism_derived_Ne),]

data_lifehistory_traits = data_lifehistory_traits[order(data_lifehistory_traits$species),]
write.table(data_lifehistory_traits , paste("database/life_history_traits_and_polymorphism_derived_Ne.tab",sep=""), quote=F , row.names = F,sep="\t") # Save the table.

