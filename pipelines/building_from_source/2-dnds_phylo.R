# Extract information and metadata for reproducibility from each dN/dS and phylogenetic tree analysis.
library(stringr)
library(ape)

list_species = data.frame(sp_taxid = list.dirs("database/BUSCO_annotations/",recursive = F,full.names = F)) # Get species.
list_species$species = sapply(list_species$sp_taxid,function(x) str_split(x,"_NCBI.taxid")[[1]][1]) # Extract scientific name.
list_species$NCBI.taxid = sapply(list_species$sp_taxid,function(x) str_split(x,"_NCBI.taxid")[[1]][2]) # Extract NCBI taxID.
rownames(list_species) = list_species$species


compute_files <- function(name,busco_set,path,raxml,dnds){ # Function to systematically download and reformat metadata from the original directories.
  
  dir.create( paste("data/dnds_phylo/",name,"/",sep=""),recursive = T)
  
  dir.create( paste("data/dnds_phylo/",name,"/phylogeny",sep=""),recursive = T)
  file.copy(list.files(paste(path,raxml,sep=""),pattern=".support",full.names = T), paste("data/dnds_phylo/",name,"/phylogeny/raxml.support.nwk",sep=""), overwrite = TRUE )
  file.copy(list.files(paste(path,raxml,sep=""),pattern=".root",full.names = T), paste("data/dnds_phylo/",name,"/phylogeny/raxml.root.nwk",sep=""), overwrite = TRUE )
  file.copy(list.files(paste(path,raxml,sep=""),pattern=".log",full.names = T), paste("data/dnds_phylo/",name,"/phylogeny/raxml.log",sep=""), overwrite = TRUE )
  file.copy(list.files(paste(path,raxml,sep=""),pattern="info",full.names = T)[1], paste("data/dnds_phylo/",name,"/phylogeny/gene.info",sep=""), overwrite = TRUE )
  
  list_species_sample = read.delim(paste(path,"gene_No_aas_cds",sep=""))
  list_species_sample$NCBI.taxid = list_species[list_species_sample$species,]$NCBI.taxid
  if ("nb_gene_85" %in% colnames(list_species_sample)){
    list_species_sample = list_species_sample[,c("species","NCBI.taxid","No_AAS_total","No_CDS_total","No_AAS_Busco","No_CDS_Busco","No_AAS_CDS_corresponding","nb_gene_85" )]
  } else {
    file.copy(list.files(paste(path,raxml,sep=""),pattern="constrain",full.names = T)[1], paste("data/dnds_phylo/",name,"/phylogeny/constrain_tree.aln",sep=""), overwrite = TRUE )
    list_species_sample = list_species_sample[,c("species","NCBI.taxid","No_AAS_total","No_CDS_total","No_AAS_Busco","No_CDS_Busco","No_AAS_CDS_corresponding","nb_gene_to_study" )]
    candidate_species = read.delim(paste(path,"candidate_species",sep=""))
    rownames(candidate_species) = candidate_species$species
    candidate_species = candidate_species[candidate_species$clade_group == name,]
    tree = read.tree(paste("data/dnds_phylo/",name,"/phylogeny/raxml.root.nwk",sep=""))
    list_species_sample = list_species_sample[list_species_sample$species %in% candidate_species$species | list_species_sample$species %in% tree$tip.label , ]
  }
  
  write.table(list_species_sample , paste("data/dnds_phylo/",name,"/species_list.tab",sep=""),quote=F,row.names = F,sep="\t")
  
  
  con <- file(paste("data/dnds_phylo/",name,"/phylogeny/raxml.log",sep=""),"r")
  first_line <- readLines(con,n=20)
  close(con)
  first_line = str_replace_all(first_line,"/beegfs/data/fbenitiere/","/home/fbenitiere/data/")
  command_line = first_line[grep("raxml-ng ",first_line)]
  
  file = str_split_1(str_split_1(command_line,"--msa ")[2]," ")[1]
  file.copy(file, paste("data/dnds_phylo/",name,"/phylogeny/",sep=""), overwrite = TRUE )
  
  file = str_split_1(str_split_1(command_line,"--model ")[2]," ")[1]
  file.copy(file, paste("data/dnds_phylo/",name,"/phylogeny/",sep=""), overwrite = TRUE )
  old_text = paste(str_split_1(file,"/")[c(1:length(str_split_1(file,"/"))-1)],collapse = "/")
  old_text = str_replace_all(old_text,"/home/fbenitiere/data/","/beegfs/data/fbenitiere/")
  
  bash_command <- paste("gzip ","data/dnds_phylo/",name,"/phylogeny/*.aln", sep="")
  system(bash_command)
  
  bash_command <- paste("sed -i 's#",old_text,"/##g' ","data/dnds_phylo/",name,"/phylogeny/raxml.log",sep="")
  system(bash_command)
  
  gene_info_tab = read.delim(paste("data/dnds_phylo/",name,"/phylogeny/gene.info",sep=""))
  file = list.files(paste("data/dnds_phylo/",name,"/phylogeny/",sep=""),pattern=".aln")[1]
  if (length(str_split_1(file,"_cons")) == 1){
    nb_species_tokeepsites = 1
  } else {
    nb_species_tokeepsites = as.numeric(str_split_1(str_split_1(file,"_cons")[2],".aln")[1])
  }
  
  
  if ("nb_gene_85" %in% colnames(list_species_sample)){
    summary = t(data.frame(
      busco_set,
      nb_genes=length(list.files(paste(path,"CDS",sep=""))),
      nb_genes_at_least_85percent_species = length(list.files(paste(path,"PRANK_CDS_at_least_85percent",sep=""))),
      nb_species_initially = nrow(read.delim(paste("data/dnds_phylo/",name,"/species_list.tab",sep=""))),
      nb_species_analyzed = length(read.tree(paste("data/dnds_phylo/",name,"/phylogeny/raxml.root.nwk",sep=""))$tip.label),
      nb_genes_raxml = str_split(gene_info_tab[nrow(gene_info_tab),]," ")[[1]][3],
      nb_species_tokeepsites ,
      length_raxml_concat = str_split(gene_info_tab[nrow(gene_info_tab),]," ")[[1]][8]
    ))
  } else {
    summary = t(data.frame(
      busco_set,
      nb_genes=length(list.files(paste(path,"CDS",sep=""))),
      nb_genes_studied = length(list.files(paste(path,"PRANK_CDS_clade",sep=""))),
      nb_species_initially = nrow(read.delim(paste("data/dnds_phylo/",name,"/species_list.tab",sep=""))),
      nb_species_analyzed = length(read.tree(paste("data/dnds_phylo/",name,"/phylogeny/raxml.root.nwk",sep=""))$tip.label),
      nb_genes_raxml = str_split(gene_info_tab[nrow(gene_info_tab),]," ")[[1]][3],
      nb_species_tokeepsites ,
      length_raxml_concat = str_split(gene_info_tab[nrow(gene_info_tab),]," ")[[1]][8]
    ))
  }
  
  write.table(summary , paste("data/dnds_phylo/",name,"/readme",sep=""),quote=F,row.names = T,col.names = F,sep="\t")
  
  
  
  ## reproducibility dnds
  dir.create( paste("data/dnds_phylo/",name,"/dnds",sep=""),recursive = T)
  
  bash_command <- paste("cp ",path, dnds,"/mapnh_dNdS_subsets.bpp"," data/dnds_phylo/",name,"/dnds/mapnh_dNdS_subsets.bpp", sep="")
  system(bash_command)
  bash_command <- paste("cp ",path, dnds,"/bppml_subsets.bpp"," data/dnds_phylo/",name,"/dnds/bppml_subsets.bpp", sep="")
  system(bash_command)
  
  for (i in list.dirs(paste(path, dnds, sep=""), recursive = F, full.names = F)){
    bash_command <- paste("mkdir"," data/dnds_phylo/",name,"/dnds/",i, sep="")
    system(bash_command)
    
    bash_command <- paste("cp ",path, dnds,"/",i,"/dNdS/*"," data/dnds_phylo/",name,"/dnds/",i, sep="")
    system(bash_command)
    bash_command <- paste("cp ",path, dnds,"/",i,"/concatenatCDS.aln"," data/dnds_phylo/",name,"/dnds/",i,"/concatenatCDS.aln", sep="")
    system(bash_command)
    bash_command <- paste("gzip ","data/dnds_phylo/",name,"/dnds/",i,"/concatenatCDS.aln", sep="")
    system(bash_command)
    bash_command <- paste("cp ",path, dnds,"/",i,"/info"," data/dnds_phylo/",name,"/dnds/",i,"/info", sep="")
    system(bash_command)
    bash_command <- paste("cp ",path, dnds,"/",i,"/*.dnd_1"," data/dnds_phylo/",name,"/dnds/",i,"/raxml.dnd_1", sep="")
    system(bash_command)
    bash_command <- paste("cp ",path, dnds,"/",i,"/model_ml.params"," data/dnds_phylo/",name,"/dnds/",i,"/model_ml.params", sep="")
    system(bash_command)
  }
}

# Execute the function for each analysis.
compute_files(name = "Eukaryota",busco_set="eukaryota_odb9",path = "/home/fbenitiere/data/Projet-SplicedVariants/DnDs/Eukaryota_v8/",raxml="RAxML/",dnds="subset_200_ksites_GC3_root")
compute_files(name = "Embryophyta",busco_set="embryophyta_odb9",path = "/home/fbenitiere/data/Projet-SplicedVariants/DnDs/Embryophyta_v2/",raxml="RAxML/",dnds="subset_200_ksites_GC3_root")
compute_files(name = "Metazoa",busco_set="metazoa_odb9",path = "/home/fbenitiere/data/Projet-SplicedVariants/DnDs/Metazoa_v11/",raxml="RAxML/",dnds="subset_200_ksites_GC3_root")

for (clade in c("Aves","Hymenoptera","Mecopterida","Mammalia","Nematoda","Other_Insecta","Other_Invertebrates","Other_Vertebrates","Teleostei")){
  print(clade)
  compute_files(
    name = paste("per_clade/",clade,sep=""),
    busco_set="metazoa_odb9",
    path = "/home/fbenitiere/data/Projet-SplicedVariants/DnDs/Metazoa_clades_v2/",
    raxml=paste("RAxML_clade/",clade,"/",sep=""),
    dnds=paste("dNdS_clade/",clade,"/subset_200_ksites_GC3_root",sep="")
  )
}
