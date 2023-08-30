

list_project = list.files("/home/fbenitiere/data/Projet-SplicedVariants/Analyses/Homo_sapiens/bioproject_analysis/",full.names = F)
list_project = list_project[grepl("gene",list_project)]

dt = data.frame()
for (proj in list_project){print(proj)
  dc = read.delim(paste("/home/fbenitiere/data/Projet-SplicedVariants/Analyses/Homo_sapiens/bioproject_analysis/",proj,sep=""))
  dc$group=proj
  dt = rbind(dt,dc)
  
  
}

library(RColorBrewer)
set_color = brewer.pal(8, 'Paired')
set_color = append(set_color,c("#fdfd99","#e2cc1a"))

ggplot(dt , aes(x=group,fill=group,y=log10(dt$median_fpkm+1))) + geom_boxplot()  + scale_fill_manual(values=set_color)
ggplot(dt , aes(x=group,fill=group,y=median_fpkm)) + geom_boxplot()  + scale_fill_manual(values=set_color)
