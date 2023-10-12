source("figure/figure_main generator/library_path.R")


met_dnds = read.delim("database/dNdS/Metazoa.tab")
emb_dnds = read.delim("database/dNdS/Embryophyta.tab")
emb_met_dnds = rbind(met_dnds,emb_dnds)

euk_dnds = read.delim("database/dNdS/Eukaryota.tab")
dt = merge.data.frame(x=euk_dnds,y=emb_met_dnds,by= "species",all=T,suffixes = c("_euk","_emb_met"))

dt$clade_group = list_species[dt$species,]$clade_group
dt$clade_group = factor(dt$clade_group, levels = 
                          c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))


# PANNEL A
dt_graph = dt
ylabel = "dNdS_euk"
xlabel = "dNdS_emb_met"
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) ,]
lm_y = (dt_graph[,ylabel])
lm_x = (dt_graph[,xlabel])

pA = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.8)  +  
  # geom_vline(xintercept=0.5,linetype="dashed",col="#FB9A99",lwd=1.2) +
  # geom_vline(xintercept=1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  # geom_vline(xintercept=0.1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  # geom_hline(yintercept=0.5,linetype="dashed",col="#FB9A99",lwd=1.2) +
  # geom_hline(yintercept=1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  # geom_hline(yintercept=0.1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw()  + theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=17, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  )+ guides(fill = guide_legend(override.aes = list(size=5))) +
  ggtitle(paste("Nspecies = ",nrow(dt_graph),
                "\nLM: ",lm_eqn(lm(lm_y ~ lm_x)),
                # " / PGLS: ",lm_eqn(pgls(pgls_y~pgls_x,shorebird)),
                sep=""))+
  ylab("Terminal branches dN/dS Eukaryota set") + xlab("Terminal branches dN/dS Metazoa and Emrbyophyta sets")
pA

jpeg(paste(path_pannel,"F6pA.jpg",sep=""),width = 7000/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B

met_dnds = read.delim("database/dNdS/Metazoa.tab")
per_clade_dnds = read.delim("database/dNdS/per_clade.tab")
dt = merge.data.frame(x=met_dnds,y=per_clade_dnds,by= "species",all=T,suffixes = c("_met","_perclade"))

dt$clade_group = list_species[dt$species,]$clade_group
dt$clade_group = factor(dt$clade_group, levels = 
                          c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates"))


dt_graph = dt
ylabel = "dNdS_perclade"
xlabel = "dNdS_met"
arbrePhylotips = read.tree( "/home/fbenitiere/data/papers/2024-EukGTDrift/data/dnds_phylo/per_clade/merged_clades_tree.nwk")
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = (dt_graph[,ylabel])
lm_x = (dt_graph[,xlabel])
shorebird <- comparative.data(arbrePhylotips, 
                              data.frame(species=dt_graph$species,
                                         pgls_x=lm_x,
                                         pgls_y=lm_y), species, vcv=TRUE)

pB = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + geom_point(pch=21,size=3,alpha=.8)  +  
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=26,family="economica"),
    axis.title.y = element_text(color="black", size=26, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=17, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="economica"),
    plot.caption.position =  "plot"
  )+ guides(fill = guide_legend(override.aes = list(size=5))) +  
  ggtitle(paste("Nspecies = ",nrow(dt_graph),
                "\nLM: ",lm_eqn(lm(lm_y ~ lm_x)),
                " / PGLS: ",lm_eqn(pgls(pgls_y~pgls_x,shorebird)),
                sep="")) +
  theme(legend.position="none")+
  ylab("Terminal branches dN/dS per clade set") + xlab("Terminal branches dN/dS Metazoa set")
pB

jpeg(paste(path_pannel,"F6pB.jpg",sep=""),width = 5200/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()



# FIGURE 6

imgA = load.image(paste(path_pannel,"F6pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F6pB.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure6.pdf",sep=""), width=3*5/2, height=2.75*3)
  
  m=matrix(rep(NA,2*1), nrow=2)
  
  m[,1]=c(rep(1,1),rep(2,1))
  
  m
  layout(m)
  
  par(mar=c(0, 2, 2, 2))
  plot(imgA, axes=F)
  mtext("A",at=20,adj=-0, side=2, line=1, font=2, cex=2,las=2)
  par(mar=c(0, 2, 2, 10))
  plot(imgB, axes=F)
  mtext("B",at=20,adj=-0, side=2, line=1, font=2, cex=2,las=2)
  
  dev.off()
}

