source("figure/figure_main_generator/library_path.R")

label_color = paste(names(Clade_color)," N=",table(data1$clade_group)[names(Clade_color)],sep='')
names(label_color) = names(Clade_color)

data1$clade_group = factor(data1$clade_group, levels = names(Clade_color))

arbrePhylotips = read.tree( "data/dnds_phylo/per_clade/merged_clades_tree_root.nwk")

# PANNEL A

dt_graph = data1
ylabel = "max_length_cm"
xlabel = "max_weight_kg"
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = log10(dt_graph[,ylabel])
lm_x = log10(dt_graph[,xlabel])

model_to_use = fitted_model(x=lm_x,y=lm_y,label=dt_graph$species,tree=arbrePhylotips,display_other=F)

pA = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) +
  geom_abline(lwd=1,slope = model_to_use$slope, intercept = model_to_use$intercept) + geom_point(pch=21,size=3,alpha=.6)  +
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + guides(fill = guide_legend(override.aes = list(size=5))) +theme(legend.position="none")  +
  scale_x_log10(breaks=c(10^-6,10^-4,10^-2,10^0,10^2,10^4,10^6),labels=label_log(digits = 2),limits = c(0.000001,1000000)) + xlab("Body Weight (kg, log scale)")+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,5000),labels=c(0.01,0.1,1,10,100,1000,5000)) + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=31, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=20, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 5)),
    plot.caption = element_text(hjust = 0.65, face= "italic", size=20, family="economica"),
    plot.caption.position =  "plot"
  )+ ylab("Body length (cm, log scale)")+
  labs(
    caption = substitute(paste(model," :",aic," R"^2,"= ",r2,", p-value = ",pvalue,model_non_opti), model_to_use),
    title = paste("N = ",nrow(dt_graph)," species",sep="")
  ) + annotation_logticks(sides = "lb")
pA

jpeg(paste(path_pannel,"F5pA.jpg",sep=""),width = 5200/resolution, height = 4000/resolution,res=700/resolution)
print(pA)
dev.off()


# PANNEL B

dt_graph = data1
ylabel = "max_length_cm"
xlabel = "max_lifespan_days"
dt_graph = dt_graph[!is.na(dt_graph[,xlabel]) & !is.na(dt_graph[,ylabel]) & dt_graph$species %in% arbrePhylotips$tip.label,]
lm_y = log10(dt_graph[,ylabel])
lm_x = log10(dt_graph[,xlabel])

model_to_use = fitted_model(x=lm_x,y=lm_y,label=dt_graph$species,tree=arbrePhylotips,display_other=F)

pB = ggplot(dt_graph , aes_string(x=xlabel,y=ylabel,fill="clade_group")) + 
  geom_abline(lwd=1,slope = model_to_use$slope, intercept = model_to_use$intercept) +geom_point(pch=21,size=3,alpha=.6)  + geom_point(pch=21,size=3,alpha=.6)  + 
  scale_fill_manual("Clades",values = Clade_color ) + theme_bw() + theme(
    axis.title.x = element_text(color="black", size=31,family="economica"),
    axis.title.y = element_text(color="black", size=31, family="economica"),
    axis.text.y =  element_text(color="black", size=26, family="economica"),
    axis.text.x =  element_text(color="black", size=26, family="economica"),
    title =  element_text(color="black", size=20, family="economica"),
    text =  element_text(color="black", size=31, family="economica"),
    legend.text =  element_text(color="black", size=24, family="economica",vjust = 1.5,margin = margin(t = 5)),
    plot.caption = element_text(hjust = 0.39, face= "italic", size=20, family="economica"),
    legend.title =  element_text(color="black", size=27, family="economica"),
    plot.caption.position =  "plot"
  ) + guides(fill = guide_legend(override.aes = list(size=5))) + 
  scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10,100,1000,10000,100000),labels=c(0.05,0.1,0.5,1,5,10,100,1000,10000,100000)) + xlab("Longevity (days, log scale)")+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,5000),labels=c(0.01,0.1,1,10,100,1000,5000)) +
  labs(
    caption = substitute(paste(model," :",aic," R"^2,"= ",r2,", p-value = ",pvalue,model_non_opti), model_to_use),
    title = paste("N = ",nrow(dt_graph)," species",sep="")
  )  + annotation_logticks(sides = "lb") + ylab("")
pB

jpeg(paste(path_pannel,"F5pB.jpg",sep=""), width = 7000/resolution, height = 4000/resolution,res=700/resolution)
print(pB)
dev.off()


# FIGURE 5

imgA = load.image(paste(path_pannel,"F5pA.jpg",sep=""))
imgB = load.image(paste(path_pannel,"F5pB.jpg",sep=""))


{
  pdf(file= paste(path_figure,"Figure5.pdf",sep=""), width=3*5/2, height=2.5)
  
  m=matrix(rep(NA,2*10), nrow=1)
  
  m[1,]=c(rep(1,9),rep(2,11))
  
  m
  layout(m)
  
  par(mar=c(0, 1, 0, 1))
  plot(imgA, axes=F)
  mtext("A",at=30,adj=0, side=2, line=1, font=2, cex=1.3,las=2)
  par(mar=c(0, 0, 0, 0))
  plot(imgB, axes=F)
  mtext("B",at=20,adj=-0.5, side=2, line=1, font=2, cex=1.3,las=2)
  
  dev.off()
}
