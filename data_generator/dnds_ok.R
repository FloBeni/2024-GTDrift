get_CM_dNdS<-function(D) {
  # Compute the cumulated number of substitutions over all genes
  cum_KS = sum(D$num_dS)
  cum_KN = sum(D$num_dN)
  cum_OS = sum(D$den_dS/D$branch_length)
  cum_ON = sum(D$den_dN/D$branch_length)
  # Compute cumulated DN, DS
  cum_dS = cum_KS/cum_OS
  cum_dN = cum_KN/cum_ON
  # Compute cumulated dN/dS
  cum_dNdS = cum_dN/cum_dS
  
  return(c(cum_dNdS,cum_dS,cum_dN,mean(D$branch_length)))
}

set = "Metazoa"


dt_all = data.frame()
# if (file.exists(paste(path,"Projet-SplicedVariants/DnDs/test_v12/dNdS_clade/",clade_studied,"/subset_200_ksites_GC3_root/data_calculation.tab",sep=""))){
method = paste("data/dnds_phylo/",set,sep="")

concatenate_list = list.dirs(paste(method,"/dnds/",sep=""),recursive = F,full.names = F)

big_data = data.frame()

concatenate = concatenate_list[1]
for (type in c("branch_length")){
  tree = read.tree(paste(method,"/dnds/",concatenate,"/raxml.dnd_1",sep=""))
  
  nodes <- sapply(tree$tip.label,function(x,y) which(y==x),y=tree$tip.label)
  
  for (species in tree$tip.label){
    noeud = nodes[species]
    i=0
    length_tot = 0
    while (i < 10){
      branch = sapply(noeud,function(x,y) which(y==x),y=tree$edge[,2])
      if (length(branch[[1]]) != 0){
        length_tot = tree$edge.length[branch] + length_tot
        big_data = rbind(big_data,data.frame(concatenate,
                                             type,
                                             species,
                                             branch_concat = i,
                                             to_add=tree$edge.length[branch],
                                             branch_length=length_tot
        ))
        noeud = tree$edge[branch,1]
        i=i+1
      } else { i = 10 }
    }
  }
}



dt_per_edge = data.frame(edge.length = tree$edge.length)
for (concatenate in concatenate_list){
  tree_length = read.tree(paste(method,"/dnds/",concatenate,"/raxml.dnd_1",sep=""))
  # for (type in c("dN","dS")){
  if (concatenate %in% concatenate_list[1:round(length(concatenate_list)/2)]){type="dN"}
  if (concatenate %in% concatenate_list[round(length(concatenate_list)/2):length(concatenate_list)]){type="dS"}
  
  for (numden in c("","_norm")){
    tree = read.tree(paste(method,"/dnds/",concatenate,"/counts_",type,numden,".dnd",sep=""))
    if (numden == "_norm"){ tree$edge.length = tree$edge.length / tree_length$edge.length}
    
    if (paste(type,numden,sep="") %in% colnames(dt_per_edge)){
      dt_per_edge[,paste(type,numden,sep="")] = tree$edge.length + dt_per_edge[,paste(type,numden,sep="")]
    } else {
      dt_per_edge[,paste(type,numden,sep="")] = tree$edge.length 
    }
  }
}

nodes <- sapply(tree$tip.label,function(x,y) which(y==x),y=tree$tip.label)

for (type in c("dN","dS")){
  for (gccons in c("")){
    dt_temp = data.frame()
    for (species in tree$tip.label){ 
      noeud = nodes[species]
      i = 0
      value = 0
      while (i < 10){
        branch = sapply(noeud,function(x,y) which(y==x),y=tree$edge[,2])
        if ( length( branch[[1]] ) != 0 ){
          
          value = value + dt_per_edge[branch,paste(type,gccons,sep="")] / dt_per_edge[branch,paste(type,gccons,"_norm",sep="")]
          
          dt_temp = rbind(dt_temp,data.frame(
            type,
            species,
            branch_concat = i,
            type,
            gccons,
            value
          ))
          noeud = tree$edge[branch,1]
          i = i + 1
        } else { i = 10 }
      }
    }
    big_data[,paste(type,gccons,"",sep="")] = dt_temp$value
  }
}

big_data$id = paste(big_data$species,big_data$branch_concat,sep="_")







data_flo = data.frame(
  id=names(tapply(big_data$dN,big_data$id,sum)),
  species = tapply(big_data$species,big_data$id,unique),
  dS = tapply(big_data$dS,big_data$id,sum),
  dN = tapply(big_data$dN,big_data$id,sum)
)
if (any(data_flo$species == "Chloebia_gouldiae")){    data_flo[data_flo$species == "Chloebia_gouldiae",]$species = "Erythrura_gouldiae"}

clade_dt = read.delim("database/list_species.tab")
rownames(clade_dt) = clade_dt$species

data_flo$dNdS = data_flo$dN/data_flo$dS
data_flo$clade = clade_dt[data_flo$species,]$clade
data_flo$clade_group = clade_dt[data_flo$species,]$clade_group

data_flo = data_flo[order(data_flo$id),]
data_flo$dS_added = NA
for (i in c(2:nrow(data_flo))){
  data_flo[i,]$dS_added  =  data_flo[i,]$dS - data_flo[i-1,]$dS
}

write.table(data_flo,paste("data/data_calculation_pooled_root.tab",sep=""),quote=F,row.names = F,sep="\t")



dt_graph = data_flo
dt_graph = dt_graph[dt_graph$dS > 0.1,]
dt_graph = dt_graph[ !duplicated(dt_graph$species) ,]
dt_graph = dt_graph[dt_graph$dS <= 1,]

p = ggplot( dt_graph ,aes(x=dS,y=dN/dS,fill=clade_group,label=species)) +
  geom_vline(xintercept=0.5,linetype="dashed",col="#FB9A99",lwd=1.2) +
  geom_vline(xintercept=1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  geom_vline(xintercept=0.1,linetype="dashed",col="#E31A1C",lwd=1.2) +
  geom_point(pch=21,size=3,alpha=.8) +
  theme_bw() +scale_fill_manual("Clades",values=Clade_color) + theme(
    axis.title.x = element_text(color="black", size=25,family="economica"),
    axis.title.y = element_text(color="black", size=25, family="economica"),
    axis.text.y =  element_text(color="black", size=20, family="economica"),
    axis.text.x =  element_text(color="black", size=20, family="economica"),
    title =  element_text(color="black", size=22, family="economica"),
    legend.text =  element_text(color="black", size=20, family="economica"),
    strip.text = element_text(size = 15)
  ) +ylim(0,0.5) + xlim(0,5)
p
library(plotly)
ggplotly(p)


write.table(dt_graph,paste("data/dnds_0.1_1_dS.tab",sep=""),quote=F,row.names = F,sep="\t")

