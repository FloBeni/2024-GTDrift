source("figure/figure_main_generator/library_path.R")

# PANNEL A

dt_db = data.frame(  table(data1[data1$expression_data,]$clade_group))
dt_db$clade = dt_db$Var1

dt_db$clade = factor(dt_db$clade, levels = c("Embryophyta","Mecopterida","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))


pA = ggplot(dt_db, aes(x = "", y = Freq/sum(Freq), fill = clade)) +
  geom_bar(stat = "identity", width = 1, color = "white",alpha=0.8) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove unnecessary background elements
  scale_fill_manual("Clades",values = Clade_color) +  # Use the custom color palette
  geom_text(aes(label = ifelse(Freq >= 20, paste0(round(Freq), ""), NA_character_)), position = position_stack(vjust = 0.5),size=10, family="economica")  +
  ggtitle(paste("Transcriptomic data for N = ",sum(dt_db$Freq)," species",sep=""))+
  theme(
    title =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=30, family="economica",vjust = 1.5,margin = margin(t = 10))
  ) + theme(legend.position="left")

jpeg(paste(path_pannel,"F3pA.jpg",sep=""), width = 8200/resolution, height = 5500/resolution,res=700/resolution)
print(pA)
dev.off()



# FIGURE 3

imgA = load.image(paste(path_pannel,"F3pA.jpg",sep=""))
imgB = load.image(paste(path_require,"ns_na_nu.png",sep=""))
imgC = load.image(paste(path_require,"acronyms.png",sep=""))

{
  pdf(file=paste(path_figure,"Figure3.pdf",sep=""), width=6.75, height=7.5/1.1)
  m=matrix(rep(c(rep(1,4),rep(2,3),rep(3,2)),9*1), nrow=9)
  
  m
  layout(m)
  
  par(mar=c(0, 0, 1, 0))
  plot(imgA, axes = F)
  mtext("A", side=2,at=20,adj=-2, line=1, font=2, cex=1.7,las=2)
  par(mar=c(0, 0, 1, 0))
  plot(imgB, axes = F)
  mtext("B",side=2,at=-40,adj=-2,  line=1, font=2, cex=1.7,las=2)
  par(mar=c(0, 0, 1, 0))
  plot(imgC, axes = F)
  mtext("C", side=2,at=-40,adj=-1.5, line=1, font=2, cex=1.7,las=2)
  dev.off()
}
