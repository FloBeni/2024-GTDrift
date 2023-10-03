source("figure/figure_main generator/library_path.R")


euk_dnds = read.delim("database/dNdS/Eukaryota.tab")
met_dnds = read.delim("database/dNdS/Mammalia.tab")

dt_graph = merge.data.frame(x=euk_dnds,y=met_dnds,by= "species",all=T,suffixes = c("_euk","_met"))

emb_dnds = read.delim("database/dNdS/Embryophyta.tab")
dt_graph = merge.data.frame(x=dt_graph,y=emb_dnds,by= "species",all=T,suffixes = c("","_emb"))

dt_graph$clade_group = list_species[dt_graph$species,]$clade_group
dt_graph$clade_group = factor(dt_graph$clade_group, levels = c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))

# PANNEL A
pA=ggplot(dt_graph[dt_graph$dS_met>0.1 & dt_graph$dS_euk>0.1,], aes(x=dNdS_met,y=dNdS_euk,fill=clade_group,label=species)) + geom_point(pch=21,size=3,alpha=.8)  + 
  geom_vline(xintercept=0.5,linetype="dashed",col="#FB9A99",lwd=1.2) +
  geom_vline(xintercept=1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  geom_vline(xintercept=0.1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  geom_hline(yintercept=0.5,linetype="dashed",col="#FB9A99",lwd=1.2) +
  geom_hline(yintercept=1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  geom_hline(yintercept=0.1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=25, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=31, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  ) + guides(fill = guide_legend(override.aes = list(size=5))) +  ggtitle(paste("Nspecies=",nrow(dt_graph[!is.na(dt_graph$dNdS_euk) & !is.na(dt_graph$dNdS_met),]),sep="")) +
  ylab("Terminal branches dN/dS Eukaryota set") + xlab("Terminal branches dN/dS Metazoa set") + geom_abline()
pA
ggplotly(pA)

jpeg(paste(path_pannel,"F6pA.jpg",sep=""),width = 8500/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B

pB=ggplot(dt_graph , aes(x=dNdS,y=dNdS_euk,fill=clade_group)) + geom_point(pch=21,size=3,alpha=.8)  + 
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=25, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=31, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  ) + guides(fill = guide_legend(override.aes = list(size=5))) +  ggtitle(paste("Nspecies=",nrow(dt_graph[!is.na(dt_graph$dNdS_euk) & !is.na(dt_graph$dNdS),]),sep=""))+
  theme(legend.position="none")+
  ylab("Terminal branches dN/dS Eukaryota set") + xlab("Terminal branches dN/dS Embryophyta set")
pB

jpeg(paste(path_pannel,"F6pB.jpg",sep=""),width = 6800/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()



# FIGURE 6

imgA = load.image(paste(path_pannel,"F6pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F6pB.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure6.pdf",sep=""), width=4*5/2, height=2.75*3)
  
  m=matrix(rep(NA,2*1), nrow=2)
  
  m[,1]=c(rep(1,1),rep(2,1))
  
  m
  layout(m)
  
  par(mar=c(0, 0, 2, 2))
  plot(imgA, axes=F)
  mtext("A",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  par(mar=c(0, 0, 2, 10))
  plot(imgB, axes=F)
  mtext("B",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  
  dev.off()
}

