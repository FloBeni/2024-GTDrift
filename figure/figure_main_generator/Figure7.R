source("figure/figure_main_generator/library_path.R")


label_color = paste(names(Clade_color)," N=",table(data1$clade_group)[names(Clade_color)],sep='')
names(label_color) = names(Clade_color)

sum(table(data1$clade_group))

data1$clade_group = factor(data1$clade_group, levels = c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))

per_clade_dnds = read.delim("database/dNdS/per_clade.tab")
data1 = merge.data.frame(x=data1,y=per_clade_dnds,by= "species",all=T,suffixes = c("","_perclade"))

# PANNEL A
dt_graph = data1
ylabel = "dNdS"
xlabel = "max_lifespan_days"
arbrePhylotips = read.tree( "data/dnds_phylo/per_clade/merged_clades_tree_root.nwk")
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = dt_graph[,ylabel]
lm_x = log10(dt_graph[,xlabel])
shorebird <- comparative.data(arbrePhylotips, 
                              data.frame(species=dt_graph$species,
                                         pgls_x=lm_x,
                                         pgls_y=lm_y), species, vcv=TRUE)

pA = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.6)  + 
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() +  theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=20, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.7, face= "italic", size=20, family="economica"),
    plot.caption.position =  "plot"
  ) + guides(fill = guide_legend(override.aes = list(size=5)))+  theme(legend.position="none")+  
  labs(
    caption = substitute(paste("LM: "," R"^2,lm_eqn," / PGLS:"," R"^2,pgls_eq), list(nbspecies=nrow(dt_graph),
                                                                                     lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                                                     pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird)))),
    title = substitute(paste("Nspecies = ",nbspecies), list(nbspecies=nrow(dt_graph),
                                                            lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                            pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird))))
  )+
  scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10,100,1000,10000),labels=c(0.05,0.1,0.5,1,5,10,100,1000,10000)) + xlab("Longevity (days, log scale)")+ 
  ylab("Terminal branches dN/dS per clade set") + annotation_logticks(sides = "b")
pA

jpeg(paste(path_pannel,"F7pA.jpg",sep=""),width = 5200/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B
dt_graph = data1
ylabel = "dNdS"
xlabel = "max_weight_kg"
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = (dt_graph[,ylabel])
lm_x = log10(dt_graph[,xlabel])
shorebird <- comparative.data(arbrePhylotips, 
                              data.frame(species=dt_graph$species,
                                         pgls_x=lm_x,
                                         pgls_y=lm_y), species, vcv=TRUE)


pB = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.6)  + 
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() +  theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=20, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.7, face= "italic", size=20, family="economica"),
    plot.caption.position =  "plot"
  ) + guides(fill = guide_legend(override.aes = list(size=5))) + 
  labs(
    caption = substitute(paste("LM: "," R"^2,lm_eqn," / PGLS:"," R"^2,pgls_eq), list(nbspecies=nrow(dt_graph),
                                                                                     lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                                                     pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird)))),
    title = substitute(paste("Nspecies = ",nbspecies), list(nbspecies=nrow(dt_graph),
                                                            lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                            pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird))))
  )+
  scale_x_log10(breaks=c(0.000001,0.001,0.1,1,10,100,5000,1000000),labels=c(0.000001,0.001,0.1,1,10,100,5000,1000000),limits = c(0.000001,1000000)) + xlab("Body Weight (kg, log scale)")+ 
  ylab("") +  theme(legend.position="none")+ annotation_logticks(sides = "b")
pB

jpeg(paste(path_pannel,"F7pB.jpg",sep=""),width = 5200/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()


# PANNEL C
dt_graph = data1
ylabel = "dNdS"
xlabel = "max_length_cm"
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = (dt_graph[,ylabel])
lm_x = log10(dt_graph[,xlabel])
shorebird <- comparative.data(arbrePhylotips, 
                              data.frame(species=dt_graph$species,
                                         pgls_x=lm_x,
                                         pgls_y=lm_y), species, vcv=TRUE)


pC = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.6)  + 
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() +  theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=20, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.25, face= "italic", size=20, family="economica"),
    plot.caption.position =  "plot"
  ) + guides(fill = guide_legend(override.aes = list(size=5))) +
  labs(
    caption = substitute(paste("LM: "," R"^2,lm_eqn," / PGLS:"," R"^2,pgls_eq), list(nbspecies=nrow(dt_graph),
                                                                                     lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                                                     pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird)))),
    title = substitute(paste("Nspecies = ",nbspecies), list(nbspecies=nrow(dt_graph),
                                                            lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                            pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird))))
  )+
  scale_x_log10(breaks=c(0.01,0.1,1,10,100,1000),labels=c(0.01,0.1,1,10,100,1000)) + xlab("Body length (cm, log scale)") +
  ylab("Terminal branches dN/dS per clade set") + annotation_logticks(sides = "b")
pC

jpeg(paste(path_pannel,"F7pC.jpg",sep=""),width = 7000/resolution, height = 4000/resolution,res=700/resolution)
print(pC)
dev.off()



# FIGURE 7

imgA = load.image(paste(path_pannel,"F7pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F7pB.jpg",sep=""))
imgC = load.image(paste(path_pannel,"F7pC.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure7.pdf",sep=""), width=3*4/2, height=2.25*2)
  
  m=matrix(rep(NA,4*1), nrow=2)
  
  m[1,]=c(rep(1,1),rep(2,1))
  m[2,]=c(rep(3,1),rep(3,1))
  
  m
  layout(m)
  
  par(mar=c(0, 1, 1, 0))
  plot(imgA, axes=F)
  mtext("A",at=-20,adj=-0, side=2, line=1, font=2, cex=1.3,las=2)
  par(mar=c(0, 0, 1, 1))
  plot(imgB, axes=F)
  mtext("B",at=-20,adj=-1, side=2, line=1, font=2, cex=1.3,las=2)
  plot(imgC, axes=F)
  mtext("C",at=20,adj=-5, side=2, line=1, font=2, cex=1.3,las=2)
  
  dev.off()
}
