library(ggplot2)
library("gridExtra")
library(ape)
library(stringr)
library(imager)
library(ggtree)
library(ggplot2)
library(RColorBrewer)
set_color = brewer.pal(8, 'Paired')
set_color = append(set_color,c("#fdfd99","#e2cc1a"))
resolution=3

path_pannel = "figure/pannels/"
path_figure = "figure/figure_main/"
path_require = "figure/images_library/"

Clade_color = c("Other Invertebrates"="#f5b48a","Lepido Diptera"="red","Other Vertebrates"="#A6CEE3","Other Insecta"="#FF7F00",
                Nematoda="#B2DF8A",Teleostei="#1F78B4",Hymenoptera="#ba8e18",Aves="#5b5b5b",Mammalia="#66281A",Embryophyta="#33A02C","branch"="black"
)



list_species = read.delim("database/list_species.tab")
rownames(list_species) = list_species$species
