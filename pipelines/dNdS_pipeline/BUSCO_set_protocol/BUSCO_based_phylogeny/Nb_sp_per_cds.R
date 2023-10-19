.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))                   
.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))

library(seqinr)
library(ggplot2)


args = (commandArgs(TRUE))
pathCDS = args[1]
pathCDS_85percent = args[2]
species_list = args[3]
path_Folder = args[4]

gene_quantity = data.frame()

list_species = read.table(species_list,header=T)$species

for (file in list.files(pathCDS)){
  print(file)
  r = read.fasta(paste(pathCDS, file, sep = ""))

  gene_quantity=rbind(gene_quantity,data.frame(
    gene=file,
    nb_species=length(r)
  ))
}

total_genes_number = nrow(gene_quantity)


p = ggplot(gene_quantity,aes(x = nb_species)) +
  geom_vline(xintercept = length(list_species), linetype="dashed",color = "white", size=0.5) +
  geom_histogram(col="black",fill="#A6CEE3")+
  xlab("Number of species per genes") + ylab("Number of genes") +
  geom_vline(xintercept = floor(.85 * length(list_species)), linetype="dashed",color = "#FDBF6F", size=2) +
  geom_text(label = floor(.85 * length(list_species)),y=length(list.files(pathCDS)) * 0.1,x = floor(.85 * length(list_species)), color = "#E31A1C", family="serif", size=10) +
  ggtitle(paste("Total number of genes :",total_genes_number))+ theme_bw() +
  theme(
    axis.title.x = element_text(color="black", size=31,family="serif"),
    axis.title.y = element_text(color="black", size=31, family="serif"),
    axis.text.y =  element_text(color="black", size=26, family="serif"),
    axis.text.x =  element_text(color="black", size=26, family="serif"),
    title =  element_text(color="black", size=31, family="serif"),
    text =  element_text(color="black", size=31, family="serif"),
    legend.text =  element_text(color="black", size=26, family="serif",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="serif"),
plot.caption.position="plot"  )


jpeg(paste(path_Folder,"distribution_genes.jpg",sep=""), width = 5000/2, height = 3000/2,res=500/3)
print(p)
dev.off()


gene_quantity = gene_quantity[gene_quantity$nb_species >= floor( .85 * length(list_species) ),]


gene_per_species = data.frame(species=list_species)
rownames(gene_per_species) = gene_per_species$species
gene_per_species$nb_gene = 0

####
for(gene in gene_quantity$gene){print(gene)
print(gene)
  file.copy(paste(pathCDS,gene,sep="")
            , paste(pathCDS_85percent,gene,sep=""))
  r = read.fasta(paste(pathCDS_85percent,gene,sep=""))
  gene_per_species[sapply(names(r),function(x) strsplit(x,"_busco")[[1]][1]),"nb_gene"] = gene_per_species[sapply(names(r),function(x) strsplit(x,"_busco")[[1]][1]),"nb_gene"] + 1

} #pour charger en local les fichiers nécessaires

p = ggplot(gene_per_species,aes(x = nb_gene)) +
  geom_vline(xintercept =length(list.files(pathCDS_85percent)), linetype="dashed",color = "white", size=0.5) +
  geom_histogram(col="black",fill="#A6CEE3")+
  xlab("Number of genes per species") + ylab("Number of species")  +
  geom_vline(xintercept = floor(.8 * length(list.files(pathCDS_85percent))), linetype="dashed", color = "#FDBF6F", size=2) +
  geom_text(label =  floor(.8 * length(list.files(pathCDS_85percent))),y=length(list_species) * 0.1,x = floor(.8 * length(list.files(pathCDS_85percent))), color = "#E31A1C", family="serif", size=10) +
  ggtitle(paste("Total number of species :",length(list_species)))+ theme_bw() +
  theme(
    axis.title.x = element_text(color="black", size=31,family="serif"),
    axis.title.y = element_text(color="black", size=31, family="serif"),
    axis.text.y =  element_text(color="black", size=26, family="serif"),
    axis.text.x =  element_text(color="black", size=26, family="serif"),
    title =  element_text(color="black", size=31, family="serif"),
    text =  element_text(color="black", size=31, family="serif"),
    legend.text =  element_text(color="black", size=26, family="serif",vjust = 1.5,margin = margin(t = 10)),
    plot.caption = element_text(hjust = 0.4, face= "italic", size=23, family="serif"),
plot.caption.position="plot")

jpeg(paste(path_Folder,"distribution_species.jpg",sep=""), width = 5000/2, height = 3000/2,res=500/3)
print(p)
dev.off()
