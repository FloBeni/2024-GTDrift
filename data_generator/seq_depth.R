
library(stringr)

busco_to_gene = read.delim("/home/fbenitiere/data/Projet-SplicedVariants/Annotations/Loxodonta_africana/busco_analysis/busco_to_gene_id_eukaryota")

busco_to_gene = busco_to_gene[!(duplicated(busco_to_gene$busco_id,fromLast = FALSE) | duplicated(busco_to_gene$busco_id,fromLast = TRUE)) &
                        !(duplicated(busco_to_gene$gene_id,fromLast = FALSE) | duplicated(busco_to_gene$gene_id,fromLast = TRUE)) ,]


intron_db = read.table("/home/fbenitiere/data/Projet-SplicedVariants/Analyses/Loxodonta_africana/by_intron_db.tab.gz", header=T , sep="\t",comment.char = "#")
gene_db = read.table("/home/fbenitiere/data/Projet-SplicedVariants/Analyses/Loxodonta_africana/by_gene_db.tab.gz", header=T , sep="\t",quote="")
gene_db = gene_db[gene_db$type == "gene",]
intron_db = intron_db[intron_db$gene_id %in% gene_db$gene_id,]
gene_db = gene_db[gene_db$gene_id %in% busco_to_gene$gene_id,]

rna_seq = colnames(intron_db)
list_rna = str_replace_all(rna_seq[grepl("n1_",rna_seq)],"n1_","")


if (length( list_rna ) >= 20 ){ nb = 20 } else { nb = length( list_rna )}
df = data.frame()
list_major_intron = list()

for ( i in 1:nb ){ # nombre de RNA-seqs compilés
  no_replicate = 2
  if ( i == 1 ){ no_replicate = length( list_rna )    }
  
  for ( j in 1:no_replicate ){ # nombre de rééchantillonnages
    if ( i == 1 ){
      intervalle = list_rna[j]
      
      print(intervalle)
      n1_value = intron_db[,paste("n1_",intervalle,sep="")]
      n2_value = intron_db[,paste("n2_",intervalle,sep="")]
      n3_value = intron_db[,paste("n3_",intervalle,sep="")]
      cov = gene_db[,paste("exon_coverage_",intervalle,sep="")]
    } else {
      intervalle = sample( list_rna ,i )
      print(intervalle)
      n1_value = apply(intron_db[,paste("n1_",intervalle,sep="")],1,function(x) sum(x,na.rm=T))
      n2_value = apply(intron_db[,paste("n2_",intervalle,sep="")],1,function(x) sum(x,na.rm=T))
      n3_value = apply(intron_db[,paste("n3_",intervalle,sep="")],1,function(x) sum(x,na.rm=T))
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
    df = rbind(df,da)
  }
}


library(RColorBrewer)
library(ggplot2)
set_color = brewer.pal(8, 'Paired')
set_color = append(set_color , c("#fdfd99","#e2cc1a"))


p = ggplot(df[df$echantillon == "all introns",],aes(x=sequencing_depth,y = N1_sup0)) + 
  # geom_point(aes(fill="N1_sup10"),size=3,pch=21)  + 
  geom_point(aes(y = major,fill="Major"),size=3,pch=21,alpha=.7)+
  geom_point(aes(y = minor,fill="Minor"),size=3,pch=21,alpha=.7)+
  geom_point(aes(y = unclassified ,fill="Unclassified"),size=3,pch=21,alpha=.7)+
  theme_bw() + theme(
    axis.title.x = element_text(color="black", size=25,family="serif"),
    axis.title.y = element_text(color="black", size=25, family="serif"),
    axis.text.y =  element_text(color="black", size=20, family="serif"),
    axis.text.x =  element_text(color="black", size=20, family="serif"),
    title =  element_text(color="black", size=25, family="serif"),
    text =  element_text(color="black", size=25, family="serif"),
    legend.text =  element_text(color="black", size=20, family="serif",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4,  size=20, family="serif"),
    plot.caption.position =  "plot"
  ) + scale_fill_manual("Intron classes",values = set_color[c(4,6,2,8)] ) + ylab("Number of introns") + xlab("Sequencing depth")
# +
#   labs(
#     caption = "(median per-base read coverage BUSCO eukaryota genes)"
#   )
ggplotly(p)
p
resolution=2
jpeg("/home/fbenitiere/LBBE-Projects/Projet SplicedVariants/article/database/seq_depth.jpg",width = 7000/resolution, height = 4000/resolution,res=700/resolution)
print(p)
dev.off()

