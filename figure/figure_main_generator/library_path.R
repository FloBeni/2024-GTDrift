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

Clade_color = c("Other Invertebrates"="#f5b48a","Mecopterida"="red","Other Vertebrates"="#A6CEE3","Other Insecta"="#FF7F00",
                Nematoda="#B2DF8A",Teleostei="#1F78B4",Hymenoptera="#ba8e18",Aves="#5b5b5b",Mammalia="#66281A",Embryophyta="#33A02C","branch"="black"
)



data1 = read.delim("data/data1.tab",comment.char = "#")
rownames(data1) = data1$species



lm_eqn <- function(m=lm(Y ~ X,data)){
  pvalue = summary(m)$coefficients[2,4]
  if (pvalue<10^-100){
  paste(" = ", round(summary(m)$r.squared, 2) , ", p-value = 0",sep="")
  } else {
  paste(" = ", round(summary(m)$r.squared, 2) , ", p-value = ",formatC(pvalue, format = "e", digits = 0),sep="")
    }
}

