source("figure/figure_main_generator/library_path.R")

# PANNEL A

listNomSpecies = tapply(str_replace_all(data1$species,"_" ," "),data1$clade_group,list)

tree_name <- "data/dnds_phylo/timetree.nwk"
tree <- read.tree(tree_name)

tree$tip.label <- str_replace_all(tree$tip.label,"_"," ")
edge_group <- str_replace_all(tree$tip.label,"_"," ")
edge_clade <- rep("branch",length(tree$edge[,2]))
for (group in unique(edge_group)){
  if (group %in% unlist(listNomSpecies)){
    edge_clade[tree$edge[,2] %in% grep(group,edge_group)] =
      names(listNomSpecies[unlist(lapply(listNomSpecies,function(x) group %in% x))])
  }
}
edge_clade_prev = edge_clade
list_inclusion =  list("Other Invertebrates"=c("Lepido Diptera","Other Insecta","Nematoda","Hymenoptera","Other Invertebrates"),
                       "Other Vertebrates"=c("Teleostei","Aves","Mammalia","Other Vertebrates"),"Other Insecta"=c("Lepido Diptera","Other Insecta","Hymenoptera"),
                       Nematoda="Nematoda",Teleostei="Teleostei",Hymenoptera="Hymenoptera",Aves="Aves",Mammalia="Mammalia","Lepido Diptera"="Lepido Diptera",Embryophyta="Embryophyta"
)


clade="Other Invertebrates"

for (clade in names(list_inclusion)){
  edge_clade[ which.edge(tree,  tree$edge[,2][edge_clade_prev %in% unlist(list_inclusion[clade])] ) ] = clade
}
node_metadata = data.frame(node=tree$edge[,2],color=edge_clade)

node_metadata$color = factor(node_metadata$color, levels = c("Embryophyta","Lepido Diptera","Hymenoptera","Other Insecta","Nematoda","Other Invertebrates","Teleostei","Mammalia","Aves","Other Vertebrates","branch"))

label_color = paste(names(Clade_color)," N=",table(data1$clade_group)[names(Clade_color)],sep='')
names(label_color) = names(Clade_color)
label_color["branch"] =  ""


pA = ggtree(tree, layout="ellipse",size=0.2)  
pA <- pA %<+% node_metadata  + aes(color=color) + 
  scale_color_manual("Clades",values=Clade_color[unique(edge_clade)]
                     ,label_color ) +    theme(
                       panel.background = element_rect(fill = "#f5f5f5", linetype = "dashed")
                     )  + theme(
                       title =  element_text(color="black", size=31, family="economica"),
                       panel.background = element_rect(fill = "white",
                                                       colour = "white",
                                                       size = 0.5, linetype = "solid"),
                       panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                       colour = "white"), 
                       panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                       colour = "white"),
                       legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 10)),
                     ) + guides(color = guide_legend(override.aes = list(lwd = 3))) + theme(legend.position="none")
pA

resolution=2
# jpeg(paste(path_pannel,"F1pA.jpg",sep=""),width = 6000/resolution, height = 6000/resolution,res=700/resolution)
jpeg(paste(path_pannel,"F1pA.jpg",sep=""),width = 3000/resolution, height = 6000/resolution,res=700/resolution)
print(pA)
dev.off()

# one of 'rectangular', 'dendrogram', 'slanted', 'ellipse', 'roundrect', 'fan', 'circular', 'inward_circular', 'radial', 'equal_angle', 'daylight' or 'ape'

# FIGURE 1

imgA <- image_read(paste(path_pannel,"F1pA.jpg",sep=""))
imgA <- image_flip(imgA)
legend<-image_read(paste(path_require,"legend_F1pA.jpg",sep=""))


aves<-readPNG(paste(path_require,"aves.png",sep=""))
teleostei<-readPNG(paste(path_require,"teleostei.png",sep=""))
monkey<-readPNG(paste(path_require,"monkey.png",sep=""))
tree<-readPNG(paste(path_require,"tree.png",sep=""))
fly<-readPNG(paste(path_require,"fly.png",sep=""))
bee<-readPNG(paste(path_require,"bee.png",sep=""))
cnidaria<-readPNG(paste(path_require,"cnidaria.png",sep=""))

{
  pdf(file= paste(path_figure,"Figure1.pdf",sep=""), width=3*5/2, height=2.75*3)
  m=matrix(rep(NA,2*1), nrow=1)
  m[1,]=c(1,2)
  
  m
  layout(m)
  
  par(mar=c(0, 0, 0, 0))
  plot(imgA, axes=F)
  
  xaxis=1000
  yaxis=2765
  rasterImage(tree,xleft=0+xaxis, ybottom=0+yaxis, xright=900/5+xaxis, ytop=900/5+yaxis)
  
  xaxis=550
  yaxis=2500
  rasterImage(cnidaria,xleft=0+xaxis, ybottom=0+yaxis, xright=900/4+xaxis, ytop=700/4+yaxis)
  
  xaxis=600
  yaxis=2270
  rasterImage(bee,xleft=0+xaxis, ybottom=0+yaxis, xright=900/5+xaxis, ytop=700/5+yaxis)
  
  xaxis=630
  yaxis=2000
  rasterImage(fly,xleft=0+xaxis, ybottom=0+yaxis, xright=900/7+xaxis, ytop=900/7+yaxis)
  
  xaxis=800
  yaxis=1000
  rasterImage(monkey,xleft=0+xaxis, ybottom=0+yaxis, xright=900/4+xaxis, ytop=900/4+yaxis)
  
  xaxis=700
  yaxis=1550
  rasterImage(teleostei,xleft=0+xaxis, ybottom=0+yaxis, xright=900/4+xaxis, ytop=500/4+yaxis)
  
  xaxis=1000
  yaxis=300
  rasterImage(aves,xleft=0+xaxis, ybottom=0+yaxis, xright=600/3+xaxis, ytop=750/3+yaxis)
  
  par(mar=c(0, 0, 0, 0))
  plot(legend, axes=F)
  
  dev.off()
}
