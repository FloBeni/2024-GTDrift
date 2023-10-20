# Collect the maximum values for each life history trait.
options(stringsAsFactors = F, scipen = 999)
species_clade = read.delim(paste("data/life_history_traits/all_life_history_traits.tab",sep="")) # Download all life history traits.

max_clade = tapply(species_clade$value,paste(species_clade$species,species_clade$life_history_traits),max) # Keep the maximum per species and per life history traits.
species_clade = species_clade[max_clade[paste(species_clade$species,species_clade$life_history_traits)] == species_clade$value,]

max_lht = species_clade[!duplicated(paste(species_clade$life_history_traits , species_clade$species)) , ]
max_lht$db = tapply(species_clade$db,paste(species_clade$species , species_clade$life_history_traits) , function(x) paste(x,collapse = ";"))[paste(max_lht$species,max_lht$life_history_traits)] # Get all the db that refer this maximum value.


write.table(max_lht , paste("database/life_history_traits.tab",sep=""),quote=F,row.names = F,sep="\t") # Save the table.

