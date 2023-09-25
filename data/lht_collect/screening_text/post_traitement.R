
library(stringr)


real = data.frame()
screen_data = data.frame()
# for (lht in c("weight","lifespan","length")){print(lht)
for (lht in c("weight","lifespan","length")){print(lht)
  lht_auto_df = read.csv(paste("data/lht_collect/screen_db/",lht,".tab",sep=""),header = F,sep="\n")
  # lht_auto_df = read.csv(paste("data/lht_collect/screening_text/",lht,".tab",sep=""),header = F,sep="\n")
  # lht_auto_df = read.csv(paste("/home/fbenitiere/LBBE-Projects/Projet SplicedVariants/analyses/auto_add_species_to_db/life_history_traits_EOL_performed/",lht,".tab",sep=""),header = F,sep="\n")
  
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
  
  lht_auto_df = lht_auto_df[-1,]
  
  lht_auto_df$species = sapply(lht_auto_df$V1,function(x) str_split(x,"\t")[[1]][1])
  lht_auto_df$db = sapply(lht_auto_df$V1,function(x) str_split(str_split(x," ")[[1]][1],"\t")[[1]][2])
  
  lht_auto_df[,"speciesEOL"] = sapply(lht_auto_df$V1,function(x) str_split(x,"\t")[[1]][length(str_split(x,"\t")[[1]])])
  lht_auto_df$V1= apply(lht_auto_df,1,function(x) {
    result_string <- gsub("\\(", "\\\\\\(", x["speciesEOL"])
    result_string = gsub("\\)", "\\\\\\)", result_string)
    str_replace(x["V1"],paste("\t",result_string,sep=""),"")
  }
  )
  
  
  lht_auto_df$V1 = sapply(lht_auto_df$V1,function(x) {
    if (substr(x,nchar(x),nchar(x)) == " "){
      substr(x,1,nchar(x)-1)
    } else {x}
  } )
  
  # lht_auto_df[lht_auto_df$db != "EOL","speciesEOL"] = lht_auto_df[lht_auto_df$db != "EOL","species"]
  # lht_auto_df[,"speciesEOL"]  = lht_auto_df[,"species"] 
  lht_auto_df$speciesEOL = str_replace_all(lht_auto_df$speciesEOL,"\\(|\\)|</i>|<i>","")
  lht_auto_df = lht_auto_df[apply(lht_auto_df,1,function(x) grepl(x["species"],str_replace_all(x["speciesEOL"]," ","_"),ignore.case=T)),]
  
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
  screen_data = rbind(screen_data,lht_auto_df)
  
  lht_auto_df = lht_auto_df[!duplicated(paste(lht_auto_df$species)),]
  rownames(lht_auto_df) = lht_auto_df$species
  
  
  # real[,paste("value_auto",lht,sep="_")] = lht_auto_df[real$species ,]$value_used
  # real[,paste("check",lht,sep="_")] = as.character(real[,paste("value_auto",lht,sep="_")]) == as.character(real[,lht])
  # print(table(real[,paste("check",lht,sep="_")]))
  
}
data1 = read.delim("database/list_species.tab")
rownames(data1) = data1$species
data1= data1[data1$clade != "Embryophyta",]
screen_data = screen_data[screen_data$species %in% data1$species,]
screen_data$id = paste(screen_data$species,screen_data$db,screen_data$categorie,sep=";")
rownames(screen_data) = screen_data$id

####
# screen_data = screen_data[grepl("EOL",screen_data$db),]
####



species_clade = read.delim(paste("data/lht_collect/all_lht.tab",sep=""))
manual_truth = species_clade[grepl("ADW|fishbase|EOL|AnAge",species_clade$db),]
# manual_truth = species_clade[grepl("EOL",species_clade$db),]

manual_truth$id = paste(manual_truth$species,sapply(manual_truth$db,function(x) str_split_1(x," ")[1]),sapply(manual_truth$lht,function(x) str_split_1(x,"_")[1]),sep=";")
rownames(manual_truth) = manual_truth$id

manual_truth$ml_value = screen_data[manual_truth$id,]$value_used
screen_data$manual_value = manual_truth[screen_data$id,]$max_value

screen_data$true_ornot =  as.character(screen_data$value_used) == as.character(screen_data$manual_value) 
manual_truth$true_ornot =  as.character(manual_truth$max_value) == as.character(manual_truth$ml_value) 

## success
print("Prop data retrieved:")
sum(manual_truth$true_ornot,na.rm = T ) / nrow(manual_truth)
print("Prop screen error:")
sum(screen_data$true_ornot,na.rm = T ) / nrow(screen_data)

table(manual_truth$true_ornot & !is.na(manual_truth$true_ornot),manual_truth$db)

