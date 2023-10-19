source("figure/figure_main generator/library_path.R")

data3 = read.delim("data/life_history_traits/all_life_history_traits.tab",header = T,sep="\t")
data3$db = sapply(data3$db,function(x) str_split(x," ")[[1]][1])

dt_db = data.frame()
dt_db = rbind(dt_db , t(data.frame(
  ADW = table(data3$db,data3$lht)["ADW",]
)))
dt_db = rbind(dt_db , t(data.frame(
  EOL = table(data3$db,data3$lht)["EOL",]
)))
dt_db = rbind(dt_db , t(data.frame(
  fishbase = table(data3$db,data3$lht)["fishbase",]
)))
dt_db = rbind(dt_db , t(data.frame(
  AnAge = table(data3$db,data3$lht)["AnAge",]
)))
dt_db = rbind(dt_db , t(data.frame(
  other = colSums(table(data3[!data3$db %in% c("AnAge","fishbase","EOL","ADW") ,]$db,data3[!data3$db %in% c("AnAge","fishbase","EOL","ADW") ,]$lht))
)))
dt_db$db = rownames(dt_db)

fill_set = set_color[c(2,4,6,8,10)-1]
names(fill_set) = unique(dt_db$db)


# PANNEL A
pA = ggplot(dt_db[dt_db$lifespan_days != 0,], aes(x = "", y = lifespan_days/sum(dt_db$lifespan_days), fill = db)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("source",values = fill_set) +  # Use the custom color palette
  geom_text(aes(label = paste0(round(lifespan_days), "")), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("lifespan, Nspecies=",length(unique(data3[data3$lht == "lifespan_days",]$species)),sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="left")

jpeg(paste(path_pannel,"F2pA.jpg",sep=""), width = 8200/resolution, height = 5500/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B

pB = ggplot(dt_db[dt_db$length_cm != 0,], aes(x = "", y = length_cm/sum(dt_db$length_cm), fill = db)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("source",values = fill_set) +  # Use the custom color palette
  geom_text(aes(label = paste0(round(length_cm), "")), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("Body length, Nspecies=",length(unique(data3[data3$lht == "length_cm",]$species)),sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")

jpeg(paste(path_pannel,"F2pB.jpg",sep=""), width = 8200/resolution, height = 5500/resolution,res=700/resolution)
print(pB)
dev.off()


# PANNEL C

pC = ggplot(dt_db[dt_db$weight_kg != 0,], aes(x = "", y = weight_kg/sum(dt_db$weight_kg), fill = db)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("source",values = fill_set) +  # Use the custom color palette
  # geom_text(aes(label = paste0(round(weight_kg), "")), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  geom_text(aes(label = ifelse(weight_kg >= 10, paste0(round(weight_kg), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("Body mass, Nspecies=",length(unique(data3[data3$lht == "weight_kg",]$species)),sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")

jpeg(paste(path_pannel,"F2pC.jpg",sep=""), width = 8200/resolution, height = 5500/resolution,res=700/resolution)
print(pC)
dev.off()



# PANNEL D
data3$clade_group = list_species[data3$species,]$clade_group
table(data3$clade_group,data3$db)[,"EOL"]

dt_db = data.frame()
for (clade in unique(data3$clade_group)){
  dt = data.frame(
    clade = table(data3[!duplicated(paste(data3$species,data3$db)),]$clade_group,data3[!duplicated(paste(data3$species,data3$db)),]$db)[clade,]
  )
  colnames(dt) = clade
  dt_db = rbind(dt_db , t(dt))
}
dt_db$clade = rownames(dt_db)



dt_db$clade = factor(dt_db$clade, levels = c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))



pD = ggplot(dt_db[dt_db$ADW != 0,], aes(x = "", y = ADW/sum(dt_db$ADW), fill = clade)) +
  geom_bar(stat = "identity", width = 1, color = "white",alpha=0.8) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Clades",values = Clade_color) +  # Use the custom color palette
  geom_text(aes(label = ifelse(ADW >= 20, paste0(round(ADW), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("ADW, Nspecies=",length(unique(data3[data3$db == "ADW",]$species)),sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="left")

jpeg(paste(path_pannel,"F2pD.jpg",sep=""), width = 8200/resolution, height = 5500/resolution,res=700/resolution)
print(pD)
dev.off()


# PANNEL E

pE = ggplot(dt_db[dt_db$EOL != 0,], aes(x = "", y = EOL/sum(dt_db$EOL), fill = clade)) +
  geom_bar(stat = "identity", width = 1, color = "white",alpha=0.8) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Clades",values = Clade_color) +  # Use the custom color palette
  geom_text(aes(label = ifelse(EOL >= 20, paste0(round(EOL), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("EOL, Nspecies=",length(unique(data3[data3$db == "EOL",]$species)),sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")

jpeg(paste(path_pannel,"F2pE.jpg",sep=""), width = 8200/resolution, height = 5500/resolution,res=700/resolution)
print(pE)
dev.off()


# PANNEL F

pF = ggplot(dt_db[dt_db$AnAge != 0,], aes(x = "", y = AnAge/sum(dt_db$AnAge), fill = clade)) +
  geom_bar(stat = "identity", width = 1, color = "white",alpha=0.8) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Clades",values = Clade_color) +  # Use the custom color palette
  geom_text(aes(label = ifelse(AnAge >= 20, paste0(round(AnAge), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("AnAge, Nspecies=",length(unique(data3[data3$db == "AnAge",]$species)),sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")


jpeg(paste(path_pannel,"F2pF.jpg",sep=""), width = 8200/resolution, height = 5500/resolution,res=700/resolution)
print(pF)
dev.off()



# FIGURE 2

imgA = load.image(paste(path_pannel,"F2pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F2pB.jpg",sep=""))
imgC = load.image(paste(path_pannel,"F2pC.jpg",sep=""))
imgD = load.image(paste(path_pannel,"F2pD.jpg",sep=""))
imgE = load.image(paste(path_pannel,"F2pE.jpg",sep=""))
imgF = load.image(paste(path_pannel,"F2pF.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure2.pdf",sep=""), width=6.75*5/2, height=2.75*3)
  m=matrix(rep(NA,15*2), nrow=2)
  
  m[1,]=c(rep(1,5),rep(2,5),rep(3,5))
  m[2,]=c(rep(4,5),rep(5,5),rep(6,5))
  
  m
  layout(m)
  
  par(mar=c(0, 0, 0, 0))
  plot(imgA, axes=F)
  mtext("A",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  # par(mar=c(0, 0, 0, 4))
  plot(imgB, axes=F)
  mtext("B",at=20,adj=0, side=2, line=1, font=2, cex=2,las=2)
  plot(imgC, axes=F)
  mtext("C",at=20,adj=0, side=2, line=1, font=2, cex=2,las=2)
  plot(imgD, axes=F)
  mtext("D",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  plot(imgE, axes=F)
  mtext("E",at=20,adj=0, side=2, line=1, font=2, cex=2,las=2)
  plot(imgF, axes=F)
  mtext("F",at=20,adj=0, side=2, line=1, font=2, cex=2,las=2)
  
  dev.off()
}
