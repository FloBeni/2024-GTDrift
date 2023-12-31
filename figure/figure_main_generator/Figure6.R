source("figure/figure_main_generator/library_path.R")


met_dnds = read.delim("database/dNdS/Metazoa.tab")
per_clade_dnds = read.delim("database/dNdS/per_clade.tab")
dt = merge.data.frame(x=met_dnds,y=per_clade_dnds,by= "species",all=T,suffixes = c("_met","_perclade"))

dt$clade_group = data1[dt$species,]$clade_group
dt$clade_group = factor(dt$clade_group, levels = 
                          c("Embryophyta","Mecopterida","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))



# PANNEL A
dt_graph = dt
ylabel = "dNdS_perclade"
xlabel = "dNdS_met"
arbrePhylotips = read.tree( "data/dnds_phylo/per_clade/merged_clades_tree_root.nwk")
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = (dt_graph[,ylabel])
lm_x = (dt_graph[,xlabel])
shorebird <- comparative.data(arbrePhylotips, 
                              data.frame(species=dt_graph$species,
                                         pgls_x=lm_x,
                                         pgls_y=lm_y), species, vcv=TRUE)

pA = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.8)  +  
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw()  + theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=20, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.6, face= "italic", size=20, family="economica"),
    plot.caption.position =  "plot"
  )+ guides(fill = guide_legend(override.aes = list(size=5))) + theme(legend.position="none")+
  labs(
    caption = substitute(paste("LM: "," R"^2,lm_eqn," / PGLS:"," R"^2,pgls_eq), list(nbspecies=nrow(dt_graph),
                                                                                     lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                                                     pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird)))),
    title = substitute(paste("N = ",nbspecies," species",sep=""), list(nbspecies=nrow(dt_graph),
                                                            lm_eqn=lm_eqn(lm(lm_y ~ lm_x)),
                                                            pgls_eq=lm_eqn(pgls(pgls_y~pgls_x,shorebird))))
  )  +  ylab("Terminal branches dN/dS per clade set") + xlab("Terminal branches dN/dS Metazoa set")
pA

jpeg(paste(path_pannel,"F6pA.jpg",sep=""),width = 5200/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B

met_dnds = read.delim("database/dNdS/Metazoa.tab")
emb_dnds = read.delim("database/dNdS/Embryophyta.tab")
emb_met_dnds = rbind(met_dnds,emb_dnds)

euk_dnds = read.delim("database/dNdS/Eukaryota.tab")
dt = merge.data.frame(x=euk_dnds,y=emb_met_dnds,by= "species",all=T,suffixes = c("_euk","_emb_met"))

dt$clade_group = data1[dt$species,]$clade_group
dt$clade_group = factor(dt$clade_group, levels = 
                          c("Embryophyta","Mecopterida","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))

dt_graph = dt
ylabel = "dNdS_euk"
xlabel = "dNdS_emb_met"
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) ,]
lm_y = (dt_graph[,ylabel])
lm_x = (dt_graph[,xlabel])

pB = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.8)  +  
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=20, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=20, family="economica"),
    plot.caption.position =  "plot"
  )+ guides(fill = guide_legend(override.aes = list(size=5))) +  
  labs(
    caption = substitute(paste("LM: "," R"^2,lm_eqn), list(nbspecies=nrow(dt_graph),
                                                           lm_eqn=lm_eqn(lm(lm_y ~ lm_x))
    )),
    title = substitute(paste("N = ",nbspecies," species",sep=""), list(nbspecies=nrow(dt_graph),
                                                            lm_eqn=lm_eqn(lm(lm_y ~ lm_x))))
  ) +  ylab("Terminal branches dN/dS Eukaryota set") + xlab("Terminal branches dN/dS Metazoa and Emrbyophyta sets")
pB

jpeg(paste(path_pannel,"F6pB.jpg",sep=""),width = 7000/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()



# FIGURE 6

imgA = load.image(paste(path_pannel,"F6pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F6pB.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure6.pdf",sep=""), width=3*5/2, height=2.55)
  
  m=matrix(rep(NA,2*10), nrow=1)
  
  m[1,]=c(rep(1,9),rep(2,11))
  
  m
  layout(m)
  
  par(mar=c(0, 1, 1, 2))
  plot(imgA, axes=F)
  mtext("A",at=-50,adj=0, side=2, line=1, font=2, cex=1.4,las=2)
  par(mar=c(0, 1, 1, 0))
  plot(imgB, axes=F)
  mtext("B",at=-50,adj=-0.5, side=2, line=1, font=2, cex=1.4,las=2)
  
  dev.off()
}

