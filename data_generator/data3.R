
library(stringr)
species = "Drosophila_melanogaster_NCBI.taxid7227"
assembly =  list.dirs(paste("database/BUSCO_annotations/",species,sep=""), full.names = F,recursive = F)

busco_to_gene = read.delim(paste("database/BUSCO_annotations/",species,"/",assembly,"/busco_to_gene_id_eukaryota",sep=""))

busco_to_gene = busco_to_gene[!(duplicated(busco_to_gene$busco_id,fromLast = FALSE) | duplicated(busco_to_gene$busco_id,fromLast = TRUE)) &
                                !(duplicated(busco_to_gene$gene_id,fromLast = FALSE) | duplicated(busco_to_gene$gene_id,fromLast = TRUE)) ,]


intron_db = read.delim(paste("database/Transcriptomic/",species,"/",assembly,"/by_intron_analysis.tab.gz",sep=""), header=T , sep="\t",comment.char = "#")
gene_db = read.delim(paste("database/Transcriptomic/",species,"/",assembly,"/by_gene_analysis.tab.gz",sep=""), header=T , sep="\t",comment.char = "#")

for (file in list.dirs(paste("database/Transcriptomic/",species,"/",assembly,"/Run/",sep=""),recursive = F,full.names = F)){
  intron_db_to_add = read.delim(paste("database/Transcriptomic/",species,"/",assembly,"/Run/",file,"/by_intron_db.tab.gz",sep=""), header=T , sep="\t",comment.char = "#")
  intron_db = cbind(intron_db , intron_db_to_add)
  
  gene_db_to_add = read.delim(paste("database/Transcriptomic/",species,"/",assembly,"/Run/",file,"/by_gene_db.tab.gz",sep=""), header=T , sep="\t",comment.char = "#")
  gene_db = cbind(gene_db , gene_db_to_add)
}

gene_db = gene_db[gene_db$type == "gene",]
intron_db = intron_db[intron_db$gene_id %in% gene_db$gene_id,]
gene_db = gene_db[gene_db$gene_id %in% busco_to_gene$gene_id,]

rna_seq = colnames(intron_db)
list_rna = str_replace_all(rna_seq[grepl("ns_",rna_seq)],"ns_","")


if ( length( list_rna ) >= 20 ){ nb = 20 } else { nb = length( list_rna )}
data3 = data.frame()
list_major_intron = list()

for ( i in 1:nb ){ # nombre de RNA-seqs compilés
  no_replicate = 10
  if ( i == 1 ){ no_replicate = length( list_rna )    }
  
  for ( j in 1:no_replicate ){ # nombre de rééchantillonnages
    if ( i == 1 ){
      intervalle = list_rna[j]
      
      print(intervalle)
      n1_value = intron_db[,paste("ns_",intervalle,sep="")]
      n2_value = intron_db[,paste("nu_",intervalle,sep="")]
      n3_value = intron_db[,paste("na_",intervalle,sep="")]
      cov = gene_db[,paste("exon_coverage_",intervalle,sep="")]
    } else {
      intervalle = sample( list_rna ,i )
      print(intervalle)
      n1_value = apply(intron_db[,paste("ns_",intervalle,sep="")],1,function(x) sum(x,na.rm=T))
      n2_value = apply(intron_db[,paste("nu_",intervalle,sep="")],1,function(x) sum(x,na.rm=T))
      n3_value = apply(intron_db[,paste("na_",intervalle,sep="")],1,function(x) sum(x,na.rm=T))
      cov = apply(gene_db[,paste("exon_coverage_",intervalle,sep="")],1,function(x) sum(x,na.rm=T))
    }
    
    SVR = n2_value / (n1_value + n2_value)
    SVR[(n1_value + n2_value) < 10] = NA
    
    IR = n3_value / (2 * n1_value + n3_value)
    IR[ (2 * n1_value + n3_value) < 10] = NA
    
    major =  sum( n1_value != 0 & SVR < 0.5 & IR < 0.5  ,na.rm = T)  
    
    minor = sum(n1_value != 0 & (SVR >= 0.5 | IR >= 0.5) ,na.rm = T)
    
    unclassified = sum( n1_value != 0 ) - (minor + major) 
    
    
    da = data.frame(
      major ,
      minor,
      unclassified,
      sequencing_depth = median(cov,na.rm = T),
      no_compiled_rna_seq = i,
      replicate = j,
      echantillon = "all introns",
      selection = 0
    )
    da$samples = list(intervalle)
    data3 = rbind(data3,da)
  }
}
data3$samples = unlist(lapply(data3$samples,function(x) paste(x,collapse=",")))


write.table(data3 , "data/data3.tab",quote=F,row.names = F,sep="\t")
