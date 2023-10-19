source("figure/figure_main generator/library_path.R")

#### FIGURE 4

imgA = load.image(paste(path_require,"diagram_pipeline.png",sep=""))

{
  pdf(file=paste(path_figure,"Figure4.pdf",sep=""), width=7, height=4.5)
  
  m=matrix(rep(1,10*17), nrow=17)
  layout(m)
  
  par(mar=c(0, 0, 1, 0))
  plot(imgA, axes = F)
  dev.off()
}
