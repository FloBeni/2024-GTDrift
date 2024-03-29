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
      NCBI.taxid = mysheets[[species]]$NCBI.taxid[1],
      assembly_accession = sapply(paste(species,"_NCBI.taxid",mysheets[[species]]$NCBI.taxid[1],sep=""),function(x)  list.dirs(paste("database/BUSCO_annotations/",x,sep=""),recursive = F,full.names = F) ),
      clade = mysheets[[species]]$Clade[1]
    ))
  }
}
rownames(species_clade) = species_clade$species

# Identify to which 'clade_group' each species belongs.
table_ncbi = read.delim(paste("data/taxonomy.tab",sep=""))
entire_taxonomy = tapply(paste(table_ncbi$rank,table_ncbi$name,sep=":"),table_ncbi$species,function(x) paste(x,collapse = ";"))
species_clade$clade_group = "Other Metazoans"
species_clade[ table_ncbi[table_ncbi$name == "Vertebrata",]$species,]$clade_group = "Other Vertebrates"
species_clade[ table_ncbi[table_ncbi$name == "Insecta",]$species,]$clade_group = "Other Insects"
species_clade[ table_ncbi[table_ncbi$name %in% c("Diptera","Lepidoptera","Coleoptera","Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade_group =
  species_clade[ table_ncbi[table_ncbi$name %in% c("Diptera","Lepidoptera","Coleoptera","Nematoda","Hymenoptera","Mammalia","Aves","Teleostei","Embryophyta"),]$species,]$clade

# Identify which species has transcriptomic data in the database.
species_clade$expression_data = species_clade$species %in% list.dirs("database/Transcriptomic",recursive = F,full.names = F)
list_trnascriptomic = list.dirs(paste("database/Transcriptomic/",sep=""),recursive = F,full.names = F)
species_clade$expression_data = sapply(paste(species_clade$species,"_NCBI.taxid",species_clade$NCBI.taxid,sep=""),function(x) x %in% list_trnascriptomic )


# Identify which species has dN/dS data in the database.
dnds_data = data.frame()
for (file in list.files(paste("database/dNdS",sep=""),recursive = F,full.names = T,pattern=".tab")){
  dnds_data = rbind(dnds_data,read.delim(file,header = T))
}
species_clade$dnds_data = species_clade$species %in% dnds_data$species

# Identify which species has life history traits data in the database.
lht_tab = read.delim("database/life_history_traits_Ne.tab")
species_clade$lht_data = species_clade$species %in% lht_tab[!is.na(lht_tab$lifespan_days) | 
                                                             !is.na(lht_tab$length_cm) |
                                                             !is.na(lht_tab$mass_kg),]$species
species_clade$Ne_data = species_clade$species %in% lht_tab[!is.na(lht_tab$Ne),]$species

# Quantify the number of RNA-seq used in the study.
species_clade$nb_rnaseq = NA
species_clade$nb_rnaseq = apply(species_clade,1,
                                function(x) length( list.dirs(paste("database/Transcriptomic/",x["species"],"_NCBI.taxid",x["NCBI.taxid"],"/",x["assembly_accession"],"/Run",sep=""),recursive = F,full.names = F))
)

# Entire taxonomy
species_clade$entire_taxonomy = entire_taxonomy[species_clade$species]

species_clade = species_clade[order(species_clade$species),]
write.table( species_clade , paste("database/list_species.tab",sep=""),quote=F,row.names = F,sep="\t") # Save the table.
