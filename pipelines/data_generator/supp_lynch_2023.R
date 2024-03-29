
# wget https://www.embopress.org/doi/suppl/10.15252/embr.202357561/suppl_file/embr202357561-sup-0002-datasetev1.xlsx

library(xlsx)
library(stringr)

embr202357561 = read.xlsx("embr202357561-sup-0002-datasetev1.xlsx", sheetIndex = 1)
embr202357561 = read.xlsx("/home/fbenitiere/Downloads/embr202357561-sup-0002-datasetev1 (1).xlsx", sheetIndex = 1)
embr202357561 = embr202357561[!is.na(embr202357561$Dataset.EV1),]
colnames(embr202357561) = embr202357561[embr202357561$Dataset.EV1 == "Species",]

embr202357561 = embr202357561[,c("Species","Mutation rate","Mutation rate","(pi\\sb s)","Ne","(ug DW)","Mutation Rate","Silent-site Diversity")]


embr202357561 = embr202357561[which(embr202357561$Species=="Invertebrates:" ):199,]
embr202357561 = embr202357561[!grepl(":",embr202357561$Species),]

colnames(embr202357561) = c("Species","Base-substitution Mutation rate","v1","Silent-site diversity","Ne","Dry Mass at Maturity  (µg)",
                            "ref mutation rate","ref silent-site Diversity")

embr202357561 = embr202357561[,-c(3,6)]

embr202357561[nrow(embr202357561)+1,] = c("Caenorhabditis nigoni",
                                                1.3e-9,
                                                0.06,
                                               0.06/(1.3e-9*4),
                                             "Denver et al. 2012",
                                               "Asher Cutter, personal communication"
)

embr202357561$Species = sapply(embr202357561$Species,function(x) paste(str_split_1(x," ")[1:2],collapse = "_"))

write.table(embr202357561 , "data/supp_lynch_2023.tab",quote=F,row.names = F,sep="\t")