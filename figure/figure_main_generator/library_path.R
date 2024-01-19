library(ggplot2)
library("gridExtra")
library(ape)
library(stringr)
library(magick)
library(scales)
library(imager)
library(caper)
library(png)
library(ggtree)
library(ggplot2)
library(RColorBrewer)
set_color = brewer.pal(8, 'Paired')
set_color = append(set_color,c("#fdfd99","#e2cc1a"))
resolution=3

path_pannel = "figure/pannels/"
path_figure = "figure/figure_main/"
path_require = "figure/images_library/"

Clade_color = c(Embryophyta="#33A02C",Diptera="red",Lepidoptera="#FB9A99",Coleoptera="#e2cc1a",Hymenoptera="#ba8e18","Other Insects"="#FF7F00",
                Nematoda="#B2DF8A",Teleostei="#1F78B4",Mammalia="#66281A",Aves="#5b5b5b","Other Vertebrates"="#A6CEE3","Other Metazoans"="#f5b48a","branch"="black"
)

data1 = read.delim("data/data1.tab",comment.char = "#")
rownames(data1) = data1$species



fitted_model <- function(x=dt_graph[,xlabel],y=dt_graph[,ylabel],label=dt_graph$species,tree = NA,display_other=T){
  # Function to choose which model between PGLS, LM and Pagel's lambda is best suited to the data.
  dt_fit = data.frame()
  if ( length(tree) != 1){
    shorebird <- comparative.data(tree, 
                                  data.frame(label=label,
                                             x=x,
                                             y=y), label, vcv=TRUE)
    fit = pgls(y~x,shorebird)
    summ_fit = summary(fit)
    dt_fit = rbind(dt_fit,data.frame(
      model="PGLS",
      p_val_slope = summ_fit$coefficients[2,4],
      r.squared = summ_fit$r.squared,
      adj.r.squared = summ_fit$adj.r.squared,
      aic = AIC(fit),
      slope = coef(fit)[2],
      intercept = coef(fit)[1]
    ))
    
    fit <- phylolm(y~x, phy = shorebird$phy, data = shorebird$data, model = "lambda")
    summ_fit = summary(fit)
    dt_fit = rbind(dt_fit,data.frame(
      model="Pagel's lambda",
      p_val_slope = summ_fit$coefficients[2,4],
      r.squared = summ_fit$r.squared,
      adj.r.squared = summ_fit$adj.r.squared,
      aic = AIC(fit),
      slope = coef(fit)[2],
      intercept = coef(fit)[1]
    ))
  } 
  
  fit = lm(y~x)
  summ_fit = summary(fit)
  dt_fit = rbind(dt_fit,data.frame(
    model="LM",
    p_val_slope = summ_fit$coefficients[2,4],
    r.squared = summ_fit$r.squared,
    adj.r.squared = summ_fit$adj.r.squared,
    aic = AIC(fit),
    slope = coef(fit)[2],
    intercept = coef(fit)[1]
  ))
  
  print(dt_fit)
  
  model_sub = dt_fit[dt_fit$aic != min(dt_fit$aic),]
  dt_fit = dt_fit[dt_fit$aic == min(dt_fit$aic),]
  
  model = paste(dt_fit$model,sep="")
  R2 = paste(round(dt_fit$r.squared, 2),sep="")
  pvalue = paste(formatC(dt_fit$p_val_slope, format = "e", digits = 0),sep="")
  model_non_opti = ""
  
  if ( length(tree) != 1 & display_other == T){
    AIC = paste(" AIC = ",round(dt_fit$aic),",",sep="")
    model_non_opti = paste(paste("/ ",model_sub$model,": AIC = ",round(model_sub$aic),sep=""),collapse = " ")
  }
  return(list(model=model,aic=AIC,r2=R2,pvalue=pvalue,model_non_opti=model_non_opti,slope=dt_fit$slope,intercept=dt_fit$intercept))
} 

