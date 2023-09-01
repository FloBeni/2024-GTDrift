source("figure/figure_main generator/library_path.R")

data1 = read.delim("data/data1.tab")

label_color = paste(names(Clade_color)," N=",table(data1$clade_group)[names(Clade_color)],sep='')
names(label_color) = names(Clade_color)

sum(table(data1$clade_group))

data1$clade_group = factor(data1$clade_group, levels = c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))

met_dnds = read.delim("database/dNdS/Metazoa.tab")

data1 = merge.data.frame(x=data1,y=met_dnds,by= "species",all=T,suffixes = c("","_met"))

# PANNEL A


pA=ggplot(data1 , aes(x=max_lifespan_days,y=dNdS,fill=clade_group)) + geom_point(pch=21,size=3,alpha=.8)  + 
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
  ) + guides(fill = guide_legend(override.aes = list(size=5))) + ggtitle(paste("Nspecies=",nrow(data1[!is.na(data1$dNdS) & !is.na(data1$max_lifespan_days),]),sep=""))+
  scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10,100,365,3650,36500),labels=c(0.05,0.1,0.5,1,5,10,100,365,3650,36500)) + xlab("Longevity (days log scale)")+ 
  ylab("Terminal branches dN/dS Metazoa set")
pA

jpeg(paste(path_pannel,"F7pA.jpg",sep=""),width = 8500/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B


pB=ggplot(data1 , aes(x=max_weight_kg,y=dNdS,fill=clade_group)) + geom_point(pch=21,size=3,alpha=.8)  + 
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
  ) + guides(fill = guide_legend(override.aes = list(size=5))) + ggtitle(paste("Nspecies=",nrow(data1[!is.na(data1$dNdS) & !is.na(data1$max_weight_kg),]),sep=""))+
  scale_x_log10(breaks=c(0.000001,0.001,0.1,1,10,100,5000,1000000),labels=c(0.000001,0.001,0.1,1,10,100,5000,1000000),limits = c(0.000001,1000000)) + xlab("Body Weight (kg log scale)")+ 
  ylab("Terminal branches dN/dS Metazoa set") +  theme(legend.position="none")
pB

jpeg(paste(path_pannel,"F7pB.jpg",sep=""),width = 6800/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()


# PANNEL C


pC=ggplot(data1 , aes(x=max_length_cm,y=dNdS,fill=clade_group)) + geom_point(pch=21,size=3,alpha=.8)  + 
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
  ) + guides(fill = guide_legend(override.aes = list(size=5))) + ggtitle(paste("Nspecies=",nrow(data1[!is.na(data1$dNdS) & !is.na(data1$max_length_cm),]),sep=""))+
  scale_x_log10(breaks=c(0.01,0.1,1,10,100,1000,5000),labels=c(0.01,0.1,1,10,100,1000,5000)) + xlab("Body length (cm log scale)") +
  ylab("Terminal branches dN/dS Metazoa set") +  theme(legend.position="none")
pC

jpeg(paste(path_pannel,"F7pC.jpg",sep=""),width = 6800/resolution, height = 4000/resolution,res=700/resolution)
print(pC)
dev.off()



# FIGURE 7

imgA = load.image(paste(path_pannel,"F7pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F7pB.jpg",sep=""))
imgC = load.image(paste(path_pannel,"F7pC.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure7.pdf",sep=""), width=4*5/2, height=2.75*3*1.33)
  
  m=matrix(rep(NA,3*1), nrow=3)
  
  m[,1]=c(rep(1,1),rep(2,1),rep(3,1))
  
  m
  layout(m)
  
  par(mar=c(0, 0, 2, 2))
  plot(imgA, axes=F)
  mtext("A",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  par(mar=c(0, 0, 2, 10))
  plot(imgB, axes=F)
  mtext("B",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  plot(imgC, axes=F)
  mtext("C",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  
  dev.off()
}
