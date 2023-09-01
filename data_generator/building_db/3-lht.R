
options(stringsAsFactors = F, scipen = 999)
species_clade = read.delim(paste("data/lht_collect/all_lht.tab",sep=""))

max_clade = tapply(species_clade$max_value,paste(species_clade$species,species_clade$lht),max)
species_clade = species_clade[max_clade[paste(species_clade$species,species_clade$lht)] == species_clade$max_value,]

max_lht = species_clade[!duplicated(paste(species_clade$lht , species_clade$species)) , ]
max_lht$db = tapply(species_clade$db,paste(species_clade$species , species_clade$lht) , function(x) paste(x,collapse = ";"))[paste(max_lht$species,max_lht$lht)]


write.table(max_lht , paste("database/lht.tab",sep=""),quote=F,row.names = F,sep="\t")

