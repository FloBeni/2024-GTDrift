################################################################################
#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.3" , .libPaths() ))
#.libPaths()
options(stringsAsFactors = F, scipen = 999)
library(taxize)


args = (commandArgs(TRUE))
species_name = args[1]
taxonomy_path = args[2]

taxID =  get_uid_(species_name)[[1]]["uid"] # Get TaxID
table_ncbi = classification(taxID[[1]], db = 'ncbi')[[1]] # Get taxonomy

write.table(table_ncbi, file=taxonomy_path, row.names=F, col.names=T, sep="\t", quote=F)

