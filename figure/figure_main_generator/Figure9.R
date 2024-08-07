source("figure/figure_main_generator/library_path.R")


# PANNEL A

data3 = read.delim("data/data3.tab",comment.char = "#")
pA = ggplot(data3,aes(x=sequencing_depth,y = N1_sup0)) + 
  # geom_point(aes(fill="N1_sup10"),size=3,pch=21)  + 
  geom_point(aes(y = major,fill="Major-isoform"),size=3,pch=21,alpha=.7)+
  geom_point(aes(y = minor,fill="Minor-isoform"),size=3,pch=21,alpha=.7)+
  geom_point(aes(y = unclassified ,fill="Unclassified"),size=3,pch=21,alpha=.7)+
  theme_bw() + theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=25, family="economica"),
    axis.text.x =  element_text(color="black", size=25, family="economica"),
    title =  element_text(color="black", size=26, family="economica"),
    text =  element_text(color="black", size=26, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  ) + scale_fill_manual("Intron classes",values = set_color[c(4,6,2,8)] ) + ylab("Number of introns") + xlab("Sequencing depth")+
  labs(
    caption = "(median per-base read coverage BUSCO eukaryota genes)"
  ) + 
  geom_vline(xintercept=200, linetype="dashed", color =  "red", size=1,alpha=0.5)+
  labs(title=expression(paste(italic("Drosophila melanogaster"))))+
  guides(fill = guide_legend(override.aes = list(size = 5)))
pA

resolution=2
jpeg(paste(path_pannel,"F9pA.jpg",sep=""),width = 7000/resolution, height = 3500/resolution,res=600/resolution)
print(pA)
dev.off()


# PANNEL B

data2 = read.table("data/data2.tab",header = T,comment.char = "#")
data2 = data2[ data2$sequencing_depth > 1 , ]

dt = data2[,c("sequencing_depth","prop_annot_major")]
dt$group = "Major-isoform"
colnames(dt) = c("sequencing_depth","proportion","group")


da = data2[,c("sequencing_depth","prop_annot_minor")]
da$group = "Minor-isoform"
colnames(da) = c("sequencing_depth","proportion","group")
dt = rbind(dt , da)

da = data2[,c("sequencing_depth","prop_annot_unclassified")]
da$group = "Unclassified"
colnames(da) = c("sequencing_depth","proportion","group")
dt = rbind(dt , da)

dt$proportion = 100 * dt$proportion


scatterPlot <- ggplot(dt,aes(x=sequencing_depth, y=proportion, fill=group)) + 
  geom_point(pch=21,size=3,alpha=.7) + scale_fill_manual("",values=set_color[c(4,6,2)]) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=31, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=25, family="economica"),
    title =  element_text(color="black", size=31, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=20, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  ) + scale_x_log10() +
  geom_vline(xintercept=200, linetype="dashed", color = "red", size=1,alpha=0.5) +
  # theme(legend.position=c(0.1,0.4), legend.justification=c(0,1))+
  theme(legend.position = "none") +
  ylab("Percentage of annotated introns\n among each category") + xlab("Sequencing depth")+ scale_y_continuous(labels = paste(seq(0,100,25),"%")) +
  guides(fill = guide_legend(override.aes = list(size = 5)))
labs(
  caption = "(median per-base read coverage BUSCO eukaryota genes)"
)
scatterPlot


xdensity <- ggplot(dt, aes(x=sequencing_depth)) + 
  geom_density(alpha=.5,fill=set_color[8])  + scale_x_log10()+ theme_bw() + theme(
    axis.title.x = element_text(color="black", size=0,family="economica"),
    axis.title.y = element_text(color="black", size=25, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=0, family="economica"),
    title =  element_text(color="black", size=31, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.8, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  )+
  theme(legend.position = "none") + ylab("\n \n Density ")+ scale_y_continuous(breaks = c(0,.25,.5,.75,1),labels=function(x) paste0(" ",x))
xdensity

ydensity <- ggplot(dt, aes(y=proportion, fill=group)) + 
  # geom_histogram(alpha=.5,col="black") +
  geom_density(alpha=.5) + 
  scale_fill_manual("",values = set_color[c(4,6,2)]) + 
  theme(legend.position = "none") + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=0, family="economica"),
    axis.text.y =  element_text(color="black", size=0, family="economica"),
    axis.text.x =  element_text(color="black", size=25, family="economica"),
    title =  element_text(color="black", size=31, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  )+theme(legend.position=c(0.2,0.6), legend.justification=c(0,1))+
  # theme(legend.position = "none") +
  xlab("Density")
ydensity


blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )


resolution=2
jpeg(paste(path_pannel,"F9pB.jpg",sep=""),width = 7000/resolution, height = 3500/resolution,res=500/resolution)
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(5, 1.4), heights=c(1.4, 5))
dev.off()


# FIGURE 9

imgA = load.image(paste(path_pannel,"F9pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F9pB.jpg",sep=""))

fly<-readPNG(paste(path_require,"Drosophila_melanogaster.png",sep=""))

{
  pdf(file= paste(path_figure,"Figure9.pdf",sep=""), width=3*5/2, height=2.35*3)
  
  m=matrix(rep(NA,2*1), nrow=2)
  
  m[,1]=c(rep(1,1),rep(2,1))
  
  m
  layout(m)
  
  par(mar=c(1, 0, 1, 1))
  plot(imgA, axes=F)
  
  xaxis=1400
  yaxis=0
  rasterImage(fly,xleft=0+xaxis, ybottom=900/6+yaxis, xright=900/5+xaxis, ytop=0+yaxis)
  
  mtext("A",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  par(mar=c(1, 0, 1, 1))
  plot(imgB, axes=F)
  mtext("B",at=20,adj=-2, side=2, line=1, font=2, cex=2,las=2)
  
  dev.off()
}
