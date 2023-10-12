source("figure/figure_main generator/library_path.R")

data1 = read.delim("data/data1.tab")

label_color = paste(names(Clade_color)," N=",table(data1$clade_group)[names(Clade_color)],sep='')
names(label_color) = names(Clade_color)

sum( table( data1$clade_group ) )

data1$clade_group = factor(data1$clade_group, levels = c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))
colnames(dt_graph)

# PANNEL A
dt_graph = data1
ylabel = "max_length_cm"
xlabel = "max_lifespan_days"
arbrePhylotips = read.tree( "/home/fbenitiere/data/papers/2024-EukGTDrift/data/dnds_phylo/per_clade/merged_clades_tree.nwk")
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = log10(dt_graph[,ylabel])
lm_x = log10(dt_graph[,xlabel])
shorebird <- comparative.data(arbrePhylotips, 
                              data.frame(species=dt_graph$species,
                                         pgls_x=lm_x,
                                         pgls_y=lm_y), species, vcv=TRUE)

pA = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.6)  + 
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=31, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=17, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  ) + guides(fill = guide_legend(override.aes = list(size=5))) +
  scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10,100,365,3650,36500),labels=c(0.05,0.1,0.5,1,5,10,100,365,3650,36500)) + xlab("Longevity (days, log scale)")+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,5000),labels=c(0.01,0.1,1,10,100,1000,5000)) + ylab("Body length (cm, log scale)")+
  ggtitle(paste("Nspecies = ",nrow(dt_graph),
                "\nLM: ",lm_eqn(lm(lm_y ~ lm_x)),
                " / PGLS: ",lm_eqn(pgls(pgls_y~pgls_x,shorebird)),
                sep=""  ))+ annotation_logticks(sides = "lb")
pA

jpeg(paste(path_pannel,"F5pA.jpg",sep=""),width = 7000/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()

# PANNEL B
dt_graph = data1
ylabel = "max_length_cm"
xlabel = "max_weight_kg"
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = log10(dt_graph[,ylabel])
lm_x = log10(dt_graph[,xlabel])
shorebird <- comparative.data(arbrePhylotips, 
                              data.frame(species=dt_graph$species,
                                         pgls_x=lm_x,
                                         pgls_y=lm_y), species, vcv=TRUE)

pB = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.6)  + 
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=31, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=17, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  )+ guides(fill = guide_legend(override.aes = list(size=5))) +
  scale_x_log10(breaks=c(0.000001,0.001,0.1,1,10,100,5000,1000000),labels=c(0.000001,0.001,0.1,1,10,100,5000,1000000),limits = c(0.000001,1000000)) + xlab("Body Weight (kg, log scale)")+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,5000),labels=c(0.01,0.1,1,10,100,1000,5000)) + ylab("Body length (cm, log scale)") +
  theme(legend.position="none")  +ggtitle(paste("Nspecies = ",nrow(dt_graph),
                                                "\nLM: ",lm_eqn(lm(lm_y ~ lm_x)),
                                                " / PGLS: ",lm_eqn(pgls(pgls_y~pgls_x,shorebird)),sep=""
  ))+ annotation_logticks(sides = "lb")
pB

jpeg(paste(path_pannel,"F5pB.jpg",sep=""),width = 5200/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()




# FIGURE 5

imgA = load.image(paste(path_pannel,"F5pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F5pB.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure5.pdf",sep=""), width=3*5/2, height=2.75*3)
  
  m=matrix(rep(NA,2*1), nrow=2)
  
  m[,1]=c(rep(1,1),rep(2,1))
  
  m
  layout(m)
  
  par(mar=c(0, 2, 2, 2))
  plot(imgA, axes=F)
  mtext("A",at=20,adj=0, side=2, line=1, font=2, cex=2,las=2)
  par(mar=c(0, 2, 2, 10))
  plot(imgB, axes=F)
  mtext("B",at=20,adj=0, side=2, line=1, font=2, cex=2,las=2)
  
  dev.off()
}
