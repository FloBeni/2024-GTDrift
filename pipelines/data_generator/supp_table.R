# Generate Supplementary Tables 1 and 2
options(stringsAsFactors = F, scipen = 999)
library(stringi)
library(stringr)
library(ape)

data1 = read.delim("database/list_species.tab")
rownames(data1) = data1$species

supp_data1 = data1[,c("species","NCBI.taxid","assembly_accession","clade", "nb_rnaseq")]

write.table(supp_data1 , "data/supp_table1.tab",quote=F,row.names = F,sep="\t")


runinfo_list = list.files("database/Transcriptomic/",pattern = "SRAruninfo.tab",recursive = T,full.names = T)

supp_data2 = data.frame()
for (file in runinfo_list){
  dt = read.delim(file)
  dt$species = str_split_1(str_split_1(file,"_NCBI.t")[1],"Transcriptomic//")[2]
  supp_data2 = rbind(supp_data2,dt)
}

supp_data2 = supp_data2[,c("species","Run","Experiment","SRAStudy","BioProject","Sample","BioSample","download_path")]
write.table(supp_data2 , "data/supp_table2.tab",quote=F,row.names = F,sep="\t")
