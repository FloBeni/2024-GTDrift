
.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.3"))

options(stringsAsFactors = F, scipen = 999)
library(stringr)
library(taxize)
library(dplyr)
library(tidyr)


pathData="/beegfs/data/fbenitiere/Projet-SplicedVariants/"
# pathData = "/home/fbenitiere/data/Projet-SplicedVariants/"



list_species = list.dirs(paste(pathData,"Annotations/",sep=""),recursive = F,full.names = F)
list_done = list.dirs(paste(pathData,"per_species/",sep=""),recursive=F,full.names=F)
print(list_done)
species = "Acanthocheilonema_viteae"
for (species in list_species[sapply(list_species,function(x) !any(grepl(x,list_done)))] ){
  print(species)
  if (file.exists(paste(pathData,"Analyses/",species,"/by_gene_analysis.tab",sep=""))){
    Sys.sleep(20)
    txid = get_uid_(species)[[1]]["uid"] # Get TaxID
    con <- file(paste(pathData , "Annotations/",species,"/data_source/annotation.gff",sep=""),"r")
    first_line <- readLines(con,n=10)
    close(con)
    print(first_line[5])
    genome_assembly = first_line[5]
    genome_assembly = str_replace(genome_assembly,"#!genome-build-accession NCBI_Assembly:","")
    
    bash_command <- paste("mkdir -p ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,sep="")
    system(bash_command)
    
    bash_command <- paste("cp ",pathData,"Analyses/",species,"/by_gene_analysis.tab ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,sep="")
    system(bash_command)
    
    bash_command <- paste("gzip ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/by_gene_analysis.tab",sep="")
    system(bash_command)
    
    bash_command <- paste("cp ",pathData,"Annotations/",species,"/SRAruninfo.tab ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,sep="")
    system(bash_command)
    
    
    sra_tab = read.delim(paste(pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/SRAruninfo.tab",sep=""))
    
    ## BY INTRON
    by_intron = read.delim(file=paste(pathData,"Analyses/",species,"/by_intron_cds.tab",sep=""), header=T , sep="\t",comment.char = "#")
    by_intron$id = paste(by_intron$seqname,by_intron$gene_id,by_intron$splice5,by_intron$splice3,by_intron$strand,sep=";")
    
    IntronLibrary = read.delim(paste(pathData,"Analyses/",species,"/IntronLibrary_inclusive.txt",sep=""))
    IntronLibrary = IntronLibrary %>%
      mutate(Gene = strsplit(as.character(Gene), ",")) %>%
      unnest(Gene) %>%
      filter(Gene != "")
    IntronLibrary$id = paste(IntronLibrary$Chr,IntronLibrary$Gene,IntronLibrary$Splice5,IntronLibrary$Splice3,IntronLibrary$Strand,sep=";")
    IntronLibrary$Annotation = grepl("Annotation",IntronLibrary$Source)
    rownames(IntronLibrary) = IntronLibrary$id
    by_intron$splicesite = paste(IntronLibrary[by_intron$id,]$SpliceSignal5,IntronLibrary[by_intron$id,]$SpliceSignal3)
    
    IntronCoord_original = read.delim(paste(pathData,"Annotations/",species,"/formatted_data/IntronCoords.tab",sep=""))
    IntronCoord_original$Splice5 = NA
    IntronCoord_original$Splice3 = NA
    IntronCoord_original[IntronCoord_original$Strand == 1,]$Splice5 = IntronCoord_original[IntronCoord_original$Strand == 1,]$Start
    IntronCoord_original[IntronCoord_original$Strand == 1,]$Splice3 = IntronCoord_original[IntronCoord_original$Strand == 1,]$End
    IntronCoord_original[IntronCoord_original$Strand == -1,]$Splice3 = IntronCoord_original[IntronCoord_original$Strand == -1,]$Start
    IntronCoord_original[IntronCoord_original$Strand == -1,]$Splice5 = IntronCoord_original[IntronCoord_original$Strand == -1,]$End
    
    IntronCoord = IntronCoord_original %>%
      mutate(Genes = strsplit(as.character(Genes), ",")) %>%
      unnest(Genes) %>%
      filter(Genes != "")
    IntronCoord$id = paste(IntronCoord$Chr,IntronCoord$Genes,IntronCoord$Splice5,IntronCoord$Splice3,IntronCoord$Strand,sep=";")
    by_intron$Annotation = by_intron$id %in% IntronCoord$id 
    colnames(by_intron) = str_replace_all(colnames(by_intron),"n1","ns")
    colnames(by_intron) = str_replace_all(colnames(by_intron),"n2","na")
    colnames(by_intron) = str_replace_all(colnames(by_intron),"n3","nu")
    
    
    bash_command <- paste("grep '^#' ",pathData,"Analyses/",species,"/by_intron_cds.tab | sed 's/n1/ns/g; s/n2/na/g; s/n3/nu/g' > ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/by_intron_analysis.tab",sep="")
    system(bash_command)
    
    write.table(by_intron,paste(pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/by_intron_analysis.tab",sep=""), row.names=F, col.names=T, sep="\t", quote=F, append=TRUE)
    
    bash_command <- paste("gzip ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/by_intron_analysis.tab",sep="")
    system(bash_command)
    ###
    
    con <- file(paste(pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/by_gene_analysis.tab.gz",sep=""),"r")
    first_line <- readLines(con,n=10)
    close(con)
    print(first_line)
    sra_list = first_line[grep("RNAseq",first_line)]
    sra_list = str_split(str_split(sra_list,": ")[[1]][2]," ")[[1]]
    print(sra_list == sra_tab$Run)
    
    for (sra in sra_list){
      
      bash_command <- paste("mkdir -p ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/Run/",sra,sep="")
      system(bash_command)
  }    
         
      bash_command <- paste("python3 transcriptomic_import.py ",pathData,"Analyses/",species,"/by_gene_db.tab.gz ",paste(sra_list,collapse=',')," ", pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/Run/"," /by_gene_db.tab.gz",sep="")
print(bash_command)
      system(bash_command)
      
      
      
      bash_command <- paste("python3 transcriptomic_import.py ", pathData,"Analyses/",species,"/by_intron_db.tab.gz ", paste(sra_list,collapse=',')," ",pathData,"per_species/",species,"_NCBI.txid",txid,"/",genome_assembly,"/Run/"," /by_intron_db.tab.gz",sep="")
print(bash_command)
      system(bash_command)
    }
}











