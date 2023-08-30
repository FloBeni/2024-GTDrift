
library(stringr)

lht = "weight"

real = read.delim("/home/fbenitiere/data/Projet-SplicedVariants/Fichiers-data/metazoa_requisit_for_dnds_clade_lht.tab")
real = read.delim("/home/fbenitiere/data/Projet-SplicedVariants/Fichiers-data/metazoa_species2_clade_lht.tab")

all_dt_lhtmod = data.frame()
for (lht in c("weight","lifespan","length")){print(lht)
  # for (lht in c("length")){print(lht)
  lht_auto_df = read.csv(paste("/home/fbenitiere/LBBE-Projects/Projet SplicedVariants/analyses/auto_add_species_to_db/life_history_traits/",lht,".tab",sep=""),header = F,sep="\n")
  
  lht_auto_df$V1 = sapply(lht_auto_df$V1,function(x) {
    if (substr(x,nchar(x),nchar(x)) == " "){
      substr(x,1,nchar(x)-1)
    } else {x}
  } )
  
  lht_auto_df$V1 = str_replace_all(lht_auto_df$V1,",","")
  
  
  lht_auto_df$nchar = sapply(lht_auto_df$V1,nchar)
  
  ## EOL remove condition
  lht_auto_df = lht_auto_df[!grepl("middle toe",lht_auto_df$V1) ,]
  lht_auto_df = lht_auto_df[!grepl("tarsus length",lht_auto_df$V1) ,]
  lht_auto_df = lht_auto_df[!grepl("bill length",lht_auto_df$V1) ,]
  lht_auto_df = lht_auto_df[!grepl("bill lht_auto_df",lht_auto_df$V1) ,]
  lht_auto_df = lht_auto_df[!grepl("may reach upto",lht_auto_df$V1) ,]
  lht_auto_df = lht_auto_df[!grepl("extends",lht_auto_df$V1) ,]
  ###
  # lht_auto_df = lht_auto_df[lht_auto_df$nchar < 500,]
  
  lht_auto_df$species = sapply(lht_auto_df$V1,function(x) str_split(x,"\t")[[1]][1])
  lht_auto_df$db = sapply(lht_auto_df$V1,function(x) str_split(str_split(x," ")[[1]][1],"\t")[[1]][2])
  
  
  lht_auto_df$value = sapply(lht_auto_df$V1,function(x) {
    numeric_positions <- gregexpr("[0-9]", x)[[1]]
    list_num = c()
    num_value=""
    position = numeric_positions[1]-2
    while (position != nchar(x)){
      position = position + 1
      value = substr(x,position,position)
      if ( value %in% as.character(c(0:9,"."))){
        num_value = paste(num_value,value,sep="")
      } else {
        list_num = append(list_num,as.numeric(num_value))
        num_value=""
      }
    }
    list_num = list_num[!is.na(list_num)]
    if (grepl("\\(female",x) & grepl("fishbase",x)){
      return(unlist(list_num[length(list_num)]))
    } else {
      return(unlist(max(list_num)))
    }
  })
  
  lht_auto_df = lht_auto_df[ lht_auto_df$value != -Inf & sapply(lht_auto_df$value,length) != 0,]
  lht_auto_df$value = unlist(lht_auto_df$value )
  
  lht_auto_df$unity = sapply(lht_auto_df$V1,function(x) str_split(x," ")[[1]][length( str_split(x," ")[[1]])])
  lht_auto_df$unity = str_replace_all(lht_auto_df$unity,"\\.","")
  lht_auto_df$unity = sapply(lht_auto_df$unity,function(x) paste(regmatches(x, gregexpr("[a-µ]", x))[[1]],collapse = ""))
  
  
  lht_auto_df$unity[lht_auto_df$unity == "female"] = "cm"
  
  lht_auto_df$value_used = apply(lht_auto_df , 1 , function(x){
    if (x["unity"] == "year" | x["unity"] == "years"){
      return(as.numeric(x["value"]) * 365)
    } else if (x["unity"] == "month" | x["unity"] == "months"){
      return(as.numeric(x["value"]) * 30)
    } else if (x["unity"] == "day" | x["unity"] == "days"){
      return(as.numeric(x["value"]) )
    } else if (x["unity"] == "cm" ){
      return(as.numeric(x["value"]) )
    } else if (x["unity"] == "mm" ){
      return(as.numeric(x["value"])/10 )
    } else if (x["unity"] == "m" ){
      return(as.numeric(x["value"])*100 )
    } else if (x["unity"] == "µm" ){
      return(as.numeric(x["value"])/10000 )
    } else if (x["unity"] == "g" ){
      return(as.numeric(x["value"])/1000 )
    } else if (x["unity"] == "kg" ){
      return(as.numeric(x["value"]) )
    } else if (x["unity"] == "tons" | x["unity"] == "t" ){
      return(as.numeric(x["value"])*1000 )
    }
  })
  
  if (lht == "length"){
    lht_auto_df = lht_auto_df[lht_auto_df$unity %in% c("m","cm","mm","µm") , ]
  } else if (lht == "lifespan"){
    lht_auto_df = lht_auto_df[lht_auto_df$unity %in% c("month","year","day","months","years","days") , ]
  }   else if (lht == "weight"){
    lht_auto_df = lht_auto_df[lht_auto_df$unity %in% c("kg","g","tons","t") , ]
  } 
  
  
  lht_auto_df = lht_auto_df[ !sapply(lht_auto_df$value_used,is.null),]
  lht_auto_df$value_used = unlist(lht_auto_df$value_used)
  
  
  lht_auto_df = lht_auto_df[order(lht_auto_df$value_used,decreasing = T),]
  lht_auto_df = lht_auto_df[!duplicated(paste(lht_auto_df$species,lht_auto_df$db)),]
  
  lht_auto_df$categorie = lht
  all_dt_lhtmod = rbind(all_dt_lhtmod,lht_auto_df)
  
  lht_auto_df = lht_auto_df[!duplicated(paste(lht_auto_df$species)),]
  rownames(lht_auto_df) = lht_auto_df$species
  
  
  real[,paste("value_auto",lht,sep="_")] = lht_auto_df[real$species ,]$value_used
  real[,paste("check",lht,sep="_")] = as.character(real[,paste("value_auto",lht,sep="_")]) == as.character(real[,lht])
  print(table(real[,paste("check",lht,sep="_")]))
  
}

all_dt_lht = all_dt_lht[order(all_dt_lht$value_used,decreasing = T),]
all_dt_lht = all_dt_lht[!duplicated(paste(all_dt_lht$species,all_dt_lht$db,all_dt_lht$categorie)),]

# write.table(list.dirs("/home/fbenitiere/data/Projet-SplicedVariants/Annotations",recursive = F,full.names = F),"/home/fbenitiere/LBBE-Projects/Projet SplicedVariants/analyses/auto_add_species_to_db/life_history_traits/list_species.tab",quote=F,row.names = F,col.names = F)
# write.table(real$species,"/home/fbenitiere/LBBE-Projects/Projet SplicedVariants/analyses/auto_add_species_to_db/life_history_traits/list_species.tab",quote=F,row.names = F,col.names = F)


##########
# Generate
# Generate RNAseq_list files for each species and the Species.txt file which is used to follow the advancement of the pipeline
options(stringsAsFactors = F, scipen = 999)
library(readxl)
library(xlsx)

# This function permit to read an excel file
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

# Reading of the excel file
mysheets <- read_excel_allsheets("/home/fbenitiere/data/Projet-SplicedVariants/Fichiers-data/metazoa_requisit_for_dnds.xls")
mysheets <- read_excel_allsheets("/home/fbenitiere/data/Projet-SplicedVariants/Fichiers-data/metazoa_species2.xls")
mysheets <- read_excel_allsheets("/home/fbenitiere/Documents/metazoa_species.xls")
mysheets <- read_excel_allsheets("/home/fbenitiere/Documents/metazoa_species2.xls")
mysheets <- read_excel_allsheets("/home/fbenitiere/Documents/metazoa_requisit_for_dnds.xls")

# Extract SRA and species names
species_clade = data.frame()
list_sp = c()
for (file in c(
  "/home/fbenitiere/Documents/metazoa_requisit_for_dnds.xls",
  # "/home/fbenitiere/Documents/metazoa_species.xls",
  "/home/fbenitiere/Documents/metazoa_species2.xls"
)){
  mysheets <- read_excel_allsheets(file)
  list_sp = append(list_sp,names(mysheets))
  for (species in names(mysheets)) {
    print(species)
    clade = mysheets[[species]]$Clade[1]
    study = mysheets[[species]]$Group_study[1]
    if (length(mysheets[[species]]$`Longevity (days)`) != 0 ){
      species_clade = rbind(species_clade,data.frame(
        species,
        clade ,
        file,
        study,
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
        file,
        study,
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
        file,
        study,
        value = as.numeric(mysheets[[species]]$`Weight (kg)`),
        ref = mysheets[[species]]$`Ref weight`,
        db = as.character(sapply(mysheets[[species]]$`Ref weight`,function(x) str_split(x," ")[[1]][1])),
        categorie="weight"
      ))
    }
    
    species_clade = species_clade[!is.na(species_clade$db),]
  }
}
species_clade = species_clade[!species_clade$study %in% c("53_sp","69_sp"),]
table(species_clade$study)

species_clade = species_clade[!species_clade$species %in% species_clade$species[which(duplicated(paste(species_clade$species,species_clade$ref,species_clade$categorie)))],]

species_clade$is_in = (
  paste(species_clade$species,species_clade$db,species_clade$value,species_clade$categorie , sep=";") %in% 
    paste(all_dt_lht$species,all_dt_lht$db,all_dt_lht$value_used,all_dt_lht$categorie , sep=";")
)

rownames(all_dt_lht) = paste(all_dt_lht$species,all_dt_lht$db,all_dt_lht$categorie , sep=";")
species_clade$auto_value = all_dt_lht[paste(species_clade$species,species_clade$db,species_clade$categorie , sep=";"),]$value_used

species_clade = species_clade[grepl("AnAge|EOL|fishbase|ADW",species_clade$ref),]
species_clade = species_clade[grepl("EOL",species_clade$ref),]
table( species_clade$is_in, species_clade$ref)



rownames(species_clade) = paste(species_clade$species,species_clade$db,species_clade$categorie , sep=";")
all_dt_lht$found = paste(all_dt_lht$species,all_dt_lht$db,all_dt_lht$value_used,all_dt_lht$categorie , sep=";") %in% paste(species_clade$species,species_clade$db,species_clade$value,species_clade$categorie , sep=";")

all_dt_lht$ref = species_clade[paste(all_dt_lht$species,all_dt_lht$db,all_dt_lht$categorie , sep=";"),]$ref
all_dt_lht = all_dt_lht[all_dt_lht$species %in% list_sp,]
all_dt_lht = all_dt_lht[all_dt_lht$species %in% species_clade$species,]
table(all_dt_lht$found,all_dt_lht$ref)

#### Remplir excel
for (species in names(mysheets)) { print(species)
  dt_sample = mysheets[[species]]
  
  all_dt_lht[all_dt_lht$species == species ,]
  
  if (nrow(dt_sample) != 0 ){
    
    dt = all_dt_lht[all_dt_lht$species == species ,]
    df = merge.data.frame(x=dt[dt$categorie=="lifespan",],y=dt[dt$categorie=="length",],all = T,by.x="db",by.y="db")
    df = merge.data.frame(x=df,y=dt[dt$categorie=="weight",],all = T,by.x="db",by.y="db")
    
    colnames(df)[colnames(df) %in% c("value_used.x","value_used.y","value_used")] = c("Longevity (days)","Length (cm)","Weight (kg)")
    if (nrow(df) != 0){
      df[,c("Ref length","Ref longevity","Ref weight")] = df$db
      
      df[is.na(df$`Longevity (days)`),"Ref longevity"] = NA
      df[is.na(df$`Length (cm)`),"Ref length"] = NA
      df[is.na(df$`Weight (kg)`),"Ref weight"] = NA
      
      df = df[colnames(df) %in% colnames(dt_sample) ]
      
      df[,colnames(dt_sample)[!colnames(dt_sample) %in% colnames(df)]] = NA
      
      dt_sample = rbind(dt_sample, df[colnames(dt_sample)])
      
      dt_sample[duplicated(dt_sample[,c("Longevity (days)","Ref longevity")]),c("Longevity (days)","Ref longevity")] = NA
      dt_sample[duplicated(dt_sample[,c("Length (cm)","Ref length")]),c("Length (cm)","Ref length")] = NA
      dt_sample[duplicated(dt_sample[,c("Weight (kg)","Ref weight")]),c("Weight (kg)","Ref weight")] = NA
      
      dt_sample[,c("Longevity (days)","Ref longevity")] = dt_sample[order(dt_sample$`Longevity (days)`,decreasing = T),c("Longevity (days)","Ref longevity")]
      dt_sample[,c("Length (cm)","Ref length")] = dt_sample[order(dt_sample$`Length (cm)`,decreasing = T),c("Length (cm)","Ref length")]
      dt_sample[,c("Weight (kg)","Ref weight")] = dt_sample[order(dt_sample$`Weight (kg)`,decreasing = T),c("Weight (kg)","Ref weight")]
      
    } 
    write.xlsx2(dt_sample,"/home/fbenitiere/Téléchargements/metazoa_requisit_for_dnds.xls",sheetName = species,append=T,row.names = F)
  }
}
