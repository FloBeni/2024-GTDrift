
library(dplyr)
library(tidyr)

path = "/home/fbenitiere/data/"
path = "/beegfs/data/fbenitiere/"

list_species = list.dirs(paste(path,"Projet-SplicedVariants/Analyses/",sep=""), full.names = F,recursive = F)

# table = table[c(1:10),]

data2 = data.frame()
for (species in list_species){
  if (file.exists(paste(path,"Projet-SplicedVariants/Analyses/",species,"/by_gene_db.tab.gz",sep=""))){
    print(species)
    
    library_path= paste(path,"Projet-SplicedVariants/Analyses/",species,"/IntronLibrary_inclusive.txt",sep="")
    
    
    IntronLibrary = read.delim(library_path)
    IntronLibrary = IntronLibrary %>%
      mutate(Gene = strsplit(as.character(Gene), ",")) %>%
      unnest(Gene) %>%
      filter(Gene != "")
    
    IntronLibrary = IntronLibrary[grepl("Annotation",IntronLibrary$Source),]
    IntronLibrary$id = paste(IntronLibrary$Chr,IntronLibrary$Strand,IntronLibrary$Splice5,IntronLibrary$Splice3,IntronLibrary$Gene,sep=";")
    
    # by_intron_overlap_path = paste(path,"Projet-SplicedVariants/Analyses/",species,"/by_intron_db.tab.gz",sep="")
    by_intron_overlap_path = paste(path,"Projet-SplicedVariants/Analyses/",species,"/by_intron_analysis.tab",sep="")
    # by_intron = read.table(by_intron_overlap_path, header=T , sep="\t",comment.char = "#")
    by_intron = read.delim(by_intron_overlap_path, header=T , sep="\t",comment.char = "#")
    by_intron$id = paste(by_intron$seqname,by_intron$strand,by_intron$splice5,by_intron$splice3,by_intron$gene_id,sep=";")
    by_intron$isAnnotated = by_intron$id %in% IntronLibrary$id
    
    by_intron = by_intron[ by_intron$n1 != 0 ,]
    
    # by_gene_path = paste(path,"Projet-SplicedVariants/Analyses/",species,"/by_gene_db.tab.gz",sep="")
    by_gene_path = paste(path,"Projet-SplicedVariants/Analyses/",species,"/by_gene_analysis.tab",sep="")
    # gene_db = read.table(by_gene_path, header=T , sep="\t",comment.char = "#")
    gene_db = read.delim(by_gene_path, header=T , sep="\t",comment.char = "#")
    
    busco_to_gene = read.delim(paste(path,"Projet-SplicedVariants/Annotations/",species,"/busco_analysis/busco_to_gene_id_eukaryota",sep=""))
    gene_db = gene_db[gene_db$gene_id %in% busco_to_gene$gene_id,]
    
    by_intron[(by_intron$n1 + by_intron$n2_spl3 + by_intron$n2_spl5) < 10,]$splice_variant_rate = NA
    by_intron[(2 *by_intron$n1 + by_intron$n3_spl3 + by_intron$n3_spl5) < 10,]$nonsplice_variant_rate = NA
    
    
    major =  sum( by_intron$splice_variant_rate < 0.5 & by_intron$nonsplice_variant_rate < 0.5  ,na.rm = T)
    annotated_major =  sum( by_intron$isAnnotated & by_intron$splice_variant_rate < 0.5 & by_intron$nonsplice_variant_rate < 0.5  ,na.rm = T)
    
    
    minor = sum( (by_intron$splice_variant_rate >= 0.5 | by_intron$nonsplice_variant_rate >= 0.5) ,na.rm = T)
    annotated_minor = sum( by_intron$isAnnotated & (by_intron$splice_variant_rate >= 0.5 | by_intron$nonsplice_variant_rate >= 0.5) ,na.rm = T)
    
    
    unclassified = nrow(by_intron) - (major + minor)   
    annotated_unclassified = sum( by_intron$isAnnotated ) - (annotated_major + annotated_minor)   
    
    
    data2 = rbind(data2,
               data.frame(
                 species,
                 sequencing_depth = median( gene_db$exon_coverage ,na.rm=T ),
                 intron = nrow( by_intron ),
                 annotated = sum( by_intron$isAnnotated ),
                 annotated_unclassified ,
                 annotated_major,
                 annotated_minor ,
                 prop_annot_unclassified = annotated_unclassified / unclassified,
                 prop_annot_minor = annotated_minor / minor,
                 prop_annot_major = annotated_major /major
               ))
    print(data2)
  }
}

# write.table(data2 , "data/data2.tab",quote=F,row.names = F,sep="\t")
write.table(data2 , "data2.tab",quote=F,row.names = F,sep="\t")