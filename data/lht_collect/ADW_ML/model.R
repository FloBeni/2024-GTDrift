library(stringr)

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


cut_string_at_positions <- function(input_string) {
  
  pattern = "[^a-z](kg|g|mg|ft|pounds|meter|meters|centimeters|inches|grams|tons|t|month|year|day|months|years|days|m|cm|mm|µm|μm)[^a-z]"
  gregepr = gregexpr(pattern , input_string,ignore.case=T)
  positions = c(1,gregepr[[1]])
  matches <- c(";;;",regmatches(input_string, gregepr)[[1]])
  
  cut_strings <- vector()
  for (pos in c(1:(length(positions)-1))) {
    cut_strings <- c(cut_strings, str_replace(paste(substr(input_string, positions[pos], positions[pos+1]-1) , matches[pos+1],sep = ""),matches[pos],""))
  }
  return(cut_strings)
}


model_name = "deepset/roberta-large-squad2-hp"
model_name = "deepset/tinyroberta-squad2"


lht = 'weight'

all_dt_lht = data.frame()
for (lht in c("weight","lifespan","length")){print(lht)
  # for (lht in c("length")){print(lht)
  
  # for (lht in c("length")){print(lht)
  lht_auto_df = read.csv(paste("/home/fbenitiere/LBBE-Projects/Projet SplicedVariants/analyses/auto_add_species_to_db/life_history_traits_ML/",model_name,"/",lht,".tab",sep="") ,quote = "",header = F,sep="\t")
  
  lht_auto_df$clade = sapply(lht_auto_df$V1 ,function(x) species_clade[species_clade$species == x,]$clade[1])
  
  # library(ggplot2)
  # ggplot(lht_auto_df,aes(x=V3)) + geom_histogram() + scale_x_log10()
  
  lht_auto_df = lht_auto_df[ lht_auto_df$V1 %in% list_sp, ]
  lht_auto_df$V4 = paste(lht_auto_df$V4," ")
  lht_auto_df$V4 = str_replace_all(lht_auto_df$V4,",","")
  
  
  lht_auto_df$list = sapply(lht_auto_df$V4,cut_string_at_positions)
  df=data.frame()
  apply(lht_auto_df,1,function(x){
    di = data.frame(x)
    di$V4 = di$list
    df <<- rbind(df,di)
    "a"
  })
  
  lht_auto_df = df
  
  lht_auto_df = lht_auto_df[!grepl("%|percent",lht_auto_df$V4),]
  
  lht_auto_df$value = sapply(lht_auto_df$V4,function(x) {
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
    return(unlist(max(list_num)))
  })
  
  
  lht_auto_df = lht_auto_df[ lht_auto_df$value != -Inf & sapply(lht_auto_df$value,length) != 0,]
  lht_auto_df$value = unlist(lht_auto_df$value )
  
  lht_auto_df$unity = sapply(lht_auto_df$V4,function(x) str_split(x,"[^a-µμ]")[[1]][length( str_split(x,"[^a-µμ]")[[1]])-1])
  
  lht_auto_df$value_used = apply(lht_auto_df , 1 , function(x){
    if (x["unity"] == "year" | x["unity"] == "years"){
      return(as.numeric(x["value"]) * 365)
    } else if (x["unity"] == "month" | x["unity"] == "months"){
      return(as.numeric(x["value"]) * 30)
    } else if (x["unity"] == "day" | x["unity"] == "days"){
      return(as.numeric(x["value"]) )
    } else if (x["unity"] == "cm" |x["unity"] == "centimeters" ){
      return(as.numeric(x["value"]) )
    } else if (x["unity"] == "mm" ){
      return(as.numeric(x["value"])/10 )
    } else if (x["unity"] == "m" | x["unity"] == "meters" | x["unity"] == "meter" ){
      return(as.numeric(x["value"])*100 )
    } else if (x["unity"] == "μm" | x["unity"] == "µm"){
      return(as.numeric(x["value"])/10000 )
    } else if (x["unity"] == "inches" ){
      return(as.numeric(x["value"])*2.54 )
    } else if (x["unity"] == "g" | x["unity"] == "grams" ){
      return(as.numeric(x["value"])/1000 )
    } else if (x["unity"] == "kg" ){
      return(as.numeric(x["value"]) )
    } else if (x["unity"] == "tons" | x["unity"] == "t" ){
      return(as.numeric(x["value"])*1000 )
    }
  })
  
  
  if (lht == "length"){
    lht_auto_df = lht_auto_df[lht_auto_df$unity %in% c("m","inches","meter","meters","cm","cm","mm","centimeters","µm","μm") , ]
  } else if (lht == "lifespan"){
    lht_auto_df = lht_auto_df[lht_auto_df$unity %in% c("month","year","day","months","years","days") , ]
  }   else if (lht == "weight"){
    lht_auto_df = lht_auto_df[lht_auto_df$unity %in% c("kg","g","tons","t","mg","grams") , ]
  } 
  
  lht_auto_df$occurences  <- sapply(seq_along(lht_auto_df$V1), function(i) sum(lht_auto_df$V1[1:i] == lht_auto_df$V1[i]))
  if ( lht == "length"){
    lht_auto_df = lht_auto_df[ lht_auto_df$occurences <= 1 | !lht_auto_df$clade %in% c("Aves","Coleoptera"),] # oiseau coleoptere la wingspan va etre catch si on prend tout tail aussi parfois
  }
  
  lht_auto_df = lht_auto_df[ !sapply(lht_auto_df$value_used,is.null),]
  lht_auto_df$value_used = unlist(lht_auto_df$value_used)
  
  
  lht_auto_df = lht_auto_df[order(lht_auto_df$value_used,decreasing = T),]
  # lht_auto_df = lht_auto_df[!duplicated(paste(lht_auto_df$V1)) & lht_auto_df$V1 %in% unique(lht_auto_df[lht_auto_df$V2 == lht,]$V1),] # ne prendre reproduction section que si lifespan section existe
  lht_auto_df = lht_auto_df[ lht_auto_df$V2 == lht,] # ne prendre reproduction section que si lifespan section existe
  lht_auto_df = lht_auto_df[!duplicated(paste(lht_auto_df$V1)) & lht_auto_df$V2 == lht,] # ne prendre reproduction section que si lifespan section existe
  
  
  species_clade_sub = species_clade[grepl("ADW",species_clade$db) & species_clade$categorie == lht ,]
  
  
  dt = merge.data.frame(x=species_clade_sub,y=lht_auto_df,by.x="species",by.y="V1",all = T)
  all_dt_lht = rbind(all_dt_lht,dt)
  
}

## success
all_dt_lht$true_ornot =  as.character(all_dt_lht$value_used) == as.character(all_dt_lht$value.x) 
table(all_dt_lht$true_ornot,all_dt_lht$categorie)
print(sum(all_dt_lht$true_ornot,na.rm = T)/sum(!is.na(all_dt_lht$value.x) ,na.rm = T))

all_dt_lht$true_ornot =  as.character(all_dt_lht$value_used) == as.character(all_dt_lht$value.x) 
table(all_dt_lht$true_ornot,all_dt_lht$categorie)
print(sum(all_dt_lht$true_ornot,na.rm = T)/sum(!is.na(all_dt_lht$value_used),na.rm = T))

