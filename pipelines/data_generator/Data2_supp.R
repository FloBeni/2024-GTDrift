library(dplyr)
library(tidyr)

data1 = read.delim("database/list_species.tab")
data1 = data1[data1$expression_data,]


list_species = list.dirs(paste("database/Transcriptomic",sep=""), full.names = F,recursive = F)

data2 = data.frame()
for (species in list_species){
  print(species)
  contig =  list.dirs(paste("database/Transcriptomic/",species,sep=""), full.names = F,recursive = F)
  by_intron = read.delim(paste("database/Transcriptomic/",species,"/",contig,"/by_intron_analysis.tab.gz",sep=""), header=T , sep="\t",comment.char = "#")
  
  by_intron = by_intron[ by_intron$ns != 0 ,]
  
  by_gene_path = paste("database/Transcriptomic",species,"/by_gene_analysis.tab",sep="")
  gene_db = read.delim(paste("database/Transcriptomic/",species,"/",contig,"/by_gene_analysis.tab.gz",sep=""), header=T , sep="\t",comment.char = "#")
  
  busco_to_gene = read.delim(paste("database/BUSCO_annotations/",species,"/",contig,"/busco_to_gene_id_eukaryota",sep=""))
  gene_db = gene_db[gene_db$gene_id %in% busco_to_gene$gene_id,]
  
  # by_intron[(by_intron$ns + by_intron$na_spl3 + by_intron$na_spl5) < 10,]$splice_variant_rate = NA
  # by_intron[(2 *by_intron$ns + by_intron$nu_spl3 + by_intron$nu_spl5) < 10,]$nonsplice_variant_rate = NA
  
  
  major =  sum( by_intron$splice_variant_rate < 0.5 & by_intron$nonsplice_variant_rate < 0.5  ,na.rm = T)
  annotated_major =  sum( by_intron$Annotation & by_intron$splice_variant_rate < 0.5 & by_intron$nonsplice_variant_rate < 0.5  ,na.rm = T)
  
  
  minor = sum( (by_intron$splice_variant_rate >= 0.5 | by_intron$nonsplice_variant_rate >= 0.5) ,na.rm = T)
  annotated_minor = sum( by_intron$Annotation & (by_intron$splice_variant_rate >= 0.5 | by_intron$nonsplice_variant_rate >= 0.5) ,na.rm = T)
  
  
  unclassified = nrow(by_intron) - (major + minor)   
  annotated_unclassified = sum( by_intron$Annotation ) - (annotated_major + annotated_minor)   
  
  
  data2 = rbind(data2,
                data.frame(
                  species,
                  sequencing_depth = median( gene_db$exon_coverage ,na.rm=T ),
                  intron = nrow( by_intron ),
                  annotated = sum( by_intron$Annotation ),
                  annotated_unclassified ,
                  annotated_major,
                  annotated_minor ,
                  prop_annot_unclassified = annotated_unclassified / unclassified,
                  prop_annot_minor = annotated_minor / minor,
                  prop_annot_major = annotated_major /major
                ))
}

write.table(data2 , "data/data2.tab",quote=F,row.names = F,sep="\t")
