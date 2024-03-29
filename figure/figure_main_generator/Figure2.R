source("figure/figure_main_generator/library_path.R")

all_lht = read.delim("data/life_history_traits/all_life_history_traits.tab",header = T,sep="\t")
all_lht$db = sapply(all_lht$db,function(x) str_split(x," ")[[1]][1])

dt_db = data.frame()
dt_db = rbind(dt_db , t(data.frame(
  ADW = table(all_lht$db,all_lht$life_history_traits)["ADW",]
)))
dt_db = rbind(dt_db , t(data.frame(
  EOL = table(all_lht$db,all_lht$life_history_traits)["EOL",]
)))
dt_db = rbind(dt_db , t(data.frame(
  fishbase = table(all_lht$db,all_lht$life_history_traits)["fishbase",]
)))
dt_db = rbind(dt_db , t(data.frame(
  AnAge = table(all_lht$db,all_lht$life_history_traits)["AnAge",]
)))
dt_db = rbind(dt_db , t(data.frame(
  other = colSums(table(all_lht[!all_lht$db %in% c("AnAge","fishbase","EOL","ADW") ,]$db,all_lht[!all_lht$db %in% c("AnAge","fishbase","EOL","ADW") ,]$life_history_traits))
)))
dt_db$db = rownames(dt_db)

fill_set = set_color[c(2,4,6,8,10)-1]
names(fill_set) = unique(dt_db$db)


# PANNEL A
pA = ggplot(dt_db[dt_db$lifespan_days != 0,], aes(x = "", y = lifespan_days/sum(dt_db$lifespan_days), fill = db)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Source",values = fill_set) +  # Use the custom color palette
  geom_text(aes( label = ifelse(lifespan_days >= 45, paste0(round(lifespan_days), ""), NA_character_)) , position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("lifespan, N = ",length(unique(all_lht[all_lht$life_history_traits == "lifespan_days",]$species))," species",sep=""))+
  theme(
    legend.title =  element_text(color="black", size=35, family="economica"),
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="left")
pA

jpeg(paste(path_pannel,"F2pA.jpg",sep=""), width = 6200/resolution, height = 5500/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B

pB = ggplot(dt_db[dt_db$length_cm != 0,], aes(x = "", y = length_cm/sum(dt_db$length_cm), fill = db)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("source",values = fill_set) +  # Use the custom color palette
  geom_text(aes(label = ifelse(length_cm >= 45, paste0(round(length_cm), ""), NA_character_)) , position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("Body length, N = ",length(unique(all_lht[all_lht$life_history_traits == "length_cm",]$species))," species",sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")

jpeg(paste(path_pannel,"F2pB.jpg",sep=""), width = 5500/resolution, height = 5500/resolution,res=700/resolution)
print(pB)
dev.off()


# PANNEL C

pC = ggplot(dt_db[dt_db$mass_kg != 0,], aes(x = "", y = mass_kg/sum(dt_db$mass_kg), fill = db)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("source",values = fill_set) +  # Use the custom color palette
  # geom_text(aes(label = paste0(round(mass_kg), "")), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  geom_text(aes(label = ifelse(mass_kg >= 45, paste0(round(mass_kg), ""), NA_character_)) , position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("Body mass, N = ",length(unique(all_lht[all_lht$life_history_traits == "mass_kg",]$species))," species",sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")

jpeg(paste(path_pannel,"F2pC.jpg",sep=""), width = 5500/resolution, height = 5500/resolution,res=700/resolution)
print(pC)
dev.off()



# PANNEL D
all_lht$clade_group = data1[all_lht$species,]$clade_group
table(all_lht$clade_group,all_lht$db)[,"EOL"]

dt_db = data.frame()
for (clade in unique(all_lht$clade_group)){
  dt = data.frame(
    clade = table(all_lht[!duplicated(paste(all_lht$species,all_lht$db)),]$clade_group,all_lht[!duplicated(paste(all_lht$species,all_lht$db)),]$db)[clade,]
  )
  colnames(dt) = clade
  dt_db = rbind(dt_db , t(dt))
}
dt_db$clade = rownames(dt_db)



dt_db$clade = factor(dt_db$clade, levels = names(Clade_color))



pD = ggplot(dt_db[dt_db$ADW != 0,], aes(x = "", y = ADW/sum(dt_db$ADW), fill = clade)) +
  geom_bar(stat = "identity", width = 1, color = "white",alpha=0.8) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Clades",values = Clade_color) +  # Use the custom color palette
  geom_text(aes(label = ifelse(ADW >= 20, paste0(round(ADW), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("ADW, N = ",length(unique(all_lht[all_lht$db == "ADW",]$species))," species",sep=""))+
  theme(
    legend.title =  element_text(color="black", size=35, family="economica"),
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="left")

jpeg(paste(path_pannel,"F2pD.jpg",sep=""), width = 6800/resolution, height = 5000/resolution,res=700/resolution)
print(pD)
dev.off()


# PANNEL E

pE = ggplot(dt_db[dt_db$EOL != 0,], aes(x = "", y = EOL/sum(dt_db$EOL), fill = clade)) +
  geom_bar(stat = "identity", width = 1, color = "white",alpha=0.8) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Clades",values = Clade_color) +  # Use the custom color palette
  geom_text(aes(label = ifelse(EOL >= 20, paste0(round(EOL), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("EOL, N = ",length(unique(all_lht[all_lht$db == "EOL",]$species))," species",sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")

jpeg(paste(path_pannel,"F2pE.jpg",sep=""), width = 5500/resolution, height = 5500/resolution,res=700/resolution)
print(pE)
dev.off()


# PANNEL F

pF = ggplot(dt_db[dt_db$AnAge != 0,], aes(x = "", y = AnAge/sum(dt_db$AnAge), fill = clade)) +
  geom_bar(stat = "identity", width = 1, color = "white",alpha=0.8) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Clades",values = Clade_color) +  # Use the custom color palette
  geom_text(aes(label = ifelse(AnAge >= 20, paste0(round(AnAge), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("AnAge, N = ",length(unique(all_lht[all_lht$db == "AnAge",]$species))," species",sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="none")


jpeg(paste(path_pannel,"F2pF.jpg",sep=""), width = 5500/resolution, height = 5500/resolution,res=700/resolution)
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
  pdf(file= paste(path_figure,"Figure2.pdf",sep=""), width=5.5*5/2, height=2.75*3)
  m=matrix(rep(NA,15*10*2), nrow=2)
  
  m[1,]=c(rep(1,54),rep(2,48),rep(3,48))
  m[2,]=c(rep(4,60),rep(5,45),rep(6,45))
  
  m
  layout(m)
  
  par(mar=c(0, 2, 1, 2))
  plot(imgA, axes=F)
  mtext("A",at=50,adj=-1, side=2, line=1, font=2, cex=2.3,las=2)
  plot(imgB, axes=F)
  mtext("B",at=50,adj=2, side=2, line=1, font=2, cex=2.3,las=2)
  plot(imgC, axes=F)
  mtext("C",at=50,adj=2, side=2, line=1, font=2, cex=2.3,las=2)
  par(mar=c(0, 2, 0, 0))
  plot(imgD, axes=F)
  mtext("D",at=-50,adj=-1, side=2, line=1, font=2, cex=2.3,las=2)
  plot(imgE, axes=F)
  mtext("E",at=-50,adj=2, side=2, line=1, font=2, cex=2.3,las=2)
  plot(imgF, axes=F)
  mtext("F",at=-50,adj=2, side=2, line=1, font=2, cex=2.3,las=2)
  
  dev.off()
}
