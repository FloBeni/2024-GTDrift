
#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))

options(stringsAsFactors = F, scipen = 999)
library(seqinr)
library(stringr)

std <- function(x) sd(x)/sqrt(length(x))

args = (commandArgs(TRUE))
cds_dir = args[1]
cds_clade_dir = args[2]
clade = args[3]
prank_clade_dir = args[4]
concatenatePathAAS_raxmlcons = args[5]
output_path = args[6]

data_sequence = data.frame()

list_busco <- str_replace(list.files(paste(cds_dir,sep="")),".fa","")

# GC3 CONTENT
for ( busco_gene in list_busco ){ print(busco_gene)# through fasta file
  fasta_file <- read.fasta(paste(cds_clade_dir,busco_gene,'/',clade,".fa",sep=""))
  prank_file <- read.fasta(paste(prank_clade_dir,busco_gene,'/',clade,".fa-prank.aln",sep=""))
  length_prank = length(prank_file[[1]])

  for (seq_id in names(fasta_file) ){ # through sequences
    species = str_split(seq_id,"_buscoid")[[1]][1]
    sequence = fasta_file[[seq_id]]
    seq_pos3 = sequence[seq(3,length(sequence),3)] # Position 3 de la sequence
    GC3 = sum(seq_pos3 %in% c("c","g")) # Count GC

    data_sequence = rbind(data_sequence,data.frame(
      species ,
      clade ,
      busco_gene ,
      AA_length = length(seq_pos3) ,
      length_prank,
      GC3_count = GC3,
      GC3_ratio = GC3 / (length(seq_pos3))
    ))
  }
}

# Species filtered
concatenate_aas = read.fasta(concatenatePathAAS_raxmlcons)
data_sequence$species_filtered = data_sequence$species %in% names(concatenate_aas)


write.table(data_sequence,paste(output_path,sep=""), row.names=F, col.names=T, sep="\t", quote=F)
