

manual_truth = read.delim(paste("data/life_history_traits/all_life_history_traits.tab",sep=""))
manual_truth[is.na(manual_truth$db),]$db = "none"
manual_truth$id = paste(manual_truth$species,sapply(manual_truth$db,function(x) str_split_1(x," ")[1]),sapply(manual_truth$life_history_traits,function(x) str_split_1(x,"_")[1]),sep=";")
rownames(manual_truth) = manual_truth$id

screen_data = read.table("data/life_history_traits/screen_db/screened_life_history_traits.tab",sep="\t",header=T)
rownames(screen_data) = screen_data$id

ml_data =  read.table("data/life_history_traits/ADW_ML/ml_life_history_traits.tab",sep="\t",header=T)
rownames(ml_data) = ml_data$id


manual_truth$screen_value = screen_data[manual_truth$id,]$value_used
screen_data$manual_value = manual_truth[screen_data$id,]$value

screen_data$true_ornot =  as.character(screen_data$value_used) == as.character(screen_data$manual_value) 
manual_truth$screen_true_ornot =  as.character(manual_truth$value) == as.character(manual_truth$screen_value) 


manual_truth$ml_value = ml_data[manual_truth$id,]$value_used
ml_data$manual_value = manual_truth[ml_data$id,]$value

ml_data$true_ornot =  as.character(ml_data$value_used) == as.character(ml_data$manual_value) 
manual_truth$ml_true_ornot =  as.character(manual_truth$value) == as.character(manual_truth$ml_value) 

table(manual_truth$ml_true_ornot,manual_truth$screen_true_ornot)



for (db in c("AnAge","fishbase","EOL","ADW")){print(db)
  manual_truth_sub = manual_truth[grepl(db,manual_truth$db),]
  screen_data_sub = screen_data[grepl(db,screen_data$db),]
  ml_data_sub = ml_data[grepl(db,ml_data$db),]
  
  print("Prop data retrieved screen:")
  print(sum(manual_truth_sub$screen_true_ornot,na.rm = T ) / nrow(manual_truth_sub)*100)
  print("Prop screen error:")
  print((1-sum(screen_data_sub$true_ornot,na.rm = T ) / nrow(screen_data_sub))*100)
  
  if (db=="ADW"){
    print("Prop data retrieved ml:")
    print(100*sum(manual_truth_sub$ml_true_ornot,na.rm = T ) / nrow(manual_truth_sub))
    print("Prop screen ml:")
    print((1-sum(ml_data_sub$true_ornot,na.rm = T ) / nrow(ml_data_sub))*100)
    
    print(100*sum(manual_truth_sub$ml_true_ornot | manual_truth_sub$screen_true_ornot,na.rm = T ) / nrow(manual_truth_sub))
    print((1-(sum(ml_data_sub$true_ornot,na.rm = T )+sum(screen_data_sub$true_ornot,na.rm = T )) / (nrow(ml_data_sub)+nrow(screen_data_sub)))*100)
  }
}

## success
print("Prop data retrieved:")
print(sum(manual_truth$true_ornot,na.rm = T ) / nrow(manual_truth))
print("Prop screen error:")
print(1-sum(screen_data$true_ornot,na.rm = T ) / nrow(screen_data))

table(manual_truth$true_ornot & !is.na(manual_truth$true_ornot),manual_truth$db)

# write.table(unique(manual_truth[is.na(manual_truth$true_ornot),]$species),"/home/fbenitiere/2024-EukGTDrift/data/life_history_traits/screen_db/list_species2.tab",col.names = F,row.names = F,quote=F)


