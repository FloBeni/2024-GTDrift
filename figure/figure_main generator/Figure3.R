source("figure/figure_main generator/library_path.R")

label_color = paste(names(Clade_color)," N=",table(data1$clade_group)[names(Clade_color)],sep='')
names(label_color) = names(Clade_color)

sum(table(data1$clade_group))

data1$clade_group = factor(data1$clade_group, levels = c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))

# PANNEL A

pA=ggplot(data1 , aes(x=max_lifespan_days,y=max_length_cm,fill=clade_group)) + geom_point(pch=21,size=3,alpha=.8)  + 
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
  ) + guides(fill = guide_legend(override.aes = list(size=5))) + 
  scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10,100,365,3650,36500),labels=c(0.05,0.1,0.5,1,5,10,100,365,3650,36500)) + xlab("Longevity (days log scale)")+
  # scale_x_log10(breaks=c(0.000001,0.001,0.1,1,10,100,5000),labels=c("1 mg","1 g",0.1,1,10,100,"5 T"),limits = c(0.000001,50000)) + xlab("Body Weight (kg log scale)")+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,5000),labels=c(0.01,0.1,1,10,100,1000,5000)) + ylab("Body length (cm log scale)") 
pA

jpeg(paste(path_pannel,"F3pA.jpg",sep=""),width = 8500/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()

# PANNEL B

pB=ggplot(data1 , aes(x=max_weight_kg,y=max_length_cm,fill=clade_group)) + geom_point(pch=21,size=3,alpha=.8)  + 
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
  ) + guides(fill = guide_legend(override.aes = list(size=5))) + 
  # scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10,100,365,3650,36500),labels=c(0.05,0.1,0.5,1,5,10,100,"1 yrs","10 yrs","100 yrs")) + xlab("Longevity (days log scale)")+
  scale_x_log10(breaks=c(0.000001,0.001,0.1,1,10,100,5000,1000000),labels=c(0.000001,0.001,0.1,1,10,100,5000,1000000),limits = c(0.000001,1000000)) + xlab("Body Weight (kg log scale)")+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,5000),labels=c(0.01,0.1,1,10,100,1000,5000)) + ylab("Body length (cm log scale)") +
  theme(legend.position="none")
pB

jpeg(paste(path_pannel,"F3pB.jpg",sep=""),width = 6800/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()




# FIGURE 3

imgA = load.image(paste(path_pannel,"F3pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F3pB.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure3.pdf",sep=""), width=4*5/2, height=2.75*3)
  
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
