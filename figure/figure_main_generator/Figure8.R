source("figure/figure_main_generator/library_path.R")

# PANNEL A

label_color = paste(names(Clade_color)," N=",table(data1$clade_group)[names(Clade_color)],sep='')
names(label_color) = names(Clade_color)

sum(table(data1$clade_group))

data1$clade_group = factor(data1$clade_group, levels =  names(Clade_color))

pA = ggplot(data1 , aes(y = nb_busco_gene_eukaryota/303 * 100 , fill = clade_group, x = clade_group)) + 
  geom_point(pch=21,size=3,alpha=0.8) + geom_boxplot(outlier.shape = NA,alpha=0.8) + 
  scale_fill_manual("Clades",values = Clade_color,labels=label_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=30, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=25, family="economica",angle=60,hjust=1),
    title =  element_text(color="black", size=31, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 3)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  ) + xlab("") + ylab("Percentage of eukaryota BUSCO\ngenes identified") + scale_y_continuous(labels = paste(seq(0,100,25),"%"))

resolution = 2
pA

jpeg(paste(path_pannel,"F8pA.jpg",sep=""),width = 9000/resolution, height = 5000/resolution,res=700/resolution)
print(pA)
dev.off()


# FIGURE 8

imgA = load.image(paste(path_pannel,"F8pA.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure8.pdf",sep=""), width=6*5/2, height=3*3)
  
  m=matrix(rep(NA,1*1), nrow=1)
  
  m[,1]=c(rep(1,1))
  
  m
  layout(m)
  
  par(mar=c(0, 2, 2, 0))
  plot(imgA, axes=F)
  
  dev.off()
}

