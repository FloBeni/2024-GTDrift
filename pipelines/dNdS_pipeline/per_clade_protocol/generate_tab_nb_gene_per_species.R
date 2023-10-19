#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))
#.libPaths()
library(seqinr)
library(stringr)


args = (commandArgs(TRUE))
species = args[1]
pathData = args[2]
fileName = args[3]
buscoSample = args[4]
list_busco_to_study_path = args[5]
out_tab_path = args[6]

# Collecte des IDbusco des arthropodes
IDBusco = read.delim(paste(pathData, "Busco/", buscoSample, "/info/ogs.id.info", sep = ""))# Collecte des IDbusco
IDBusco = IDBusco[duplicated(IDBusco[, 2]) == F, 2]

allSequence = data.frame(matrix(ncol = 6, nrow = 0))
colnames(allSequence) = c('name', 'sequenceAAS', 'sequenceCDS', 'busco_id', 'protein', "species")

information_table = data.frame()

print(species)
pathAnalyses = paste(pathData, "Analyses/", species, sep = "")
pathAnnotations = paste(pathData, "Annotations/", species, sep = "")
buscoRef = read.delim(paste(pathAnnotations,"/busco_analysis/", fileName, sep = "/"))
buscoRef = buscoRef[!(duplicated(buscoRef$busco_id,fromLast = FALSE) | duplicated(buscoRef$busco_id,fromLast = TRUE)) &
                      !(duplicated(buscoRef$gene_id,fromLast = FALSE) | duplicated(buscoRef$gene_id,fromLast = TRUE)) ,]
rownames(buscoRef) = buscoRef[,"sequence"]

AAsequence = read.fasta(paste(pathAnnotations, "/data_source/protein.faa", sep = ""))
aas = sapply(AAsequence, function(x) attr(x, 'name')) #recupere le code de la proteine
aas = data.frame(protein = aas)
aas$sequence = AAsequence #recupere la sequence de la proteine


CDSsequence = read.fasta(paste(pathAnnotations, "/data_source/cds_from_genomic.fna", sep = ""))
cds = sapply(CDSsequence, function(x) attr(x, 'Annot'))
cds = data.frame(protein = sub(".*protein_id=*(.*?) *].*", "\\1", cds)) #recupere le code de la proteine
cds$sequence = CDSsequence #recupere la sequence de la proteine
cds$longueur = lapply(cds$sequence, function(x) length(x)) #recupere la longueur de la cds
cds = cds[order(unlist(cds$longueur), decreasing = T),]
cds = cds[duplicated(cds[, 1]) == F,] #elimine les proteines ayant plus d'une cds en gardant la plus grande
rownames(cds) = cds$protein

aas$busco_id = buscoRef[aas$protein,"busco_id"]
cds$busco_id = buscoRef[cds$protein,"busco_id"]

rownames(buscoRef) = buscoRef$busco_id

information_table = rbind(information_table,data.frame(
  species,
  No_AAS_total = nrow(aas),
  No_CDS_total = nrow(cds),
  No_AAS_Busco = nrow(aas[!is.na(aas$busco_id),]),
  No_CDS_Busco = nrow(cds[!is.na(cds$busco_id),])
))


print(information_table[information_table$species == species,])

for (busco_id in IDBusco) { #pour chaque gene busco recupere la cds et aas de sa proteine la plus longue
  if (busco_id %in% buscoRef$busco_id) {
    allSequence[paste(species, "_buscoid:", busco_id, "_longestProt:", buscoRef[busco_id, "sequence"], sep = ""),] =
      list(paste(species, "_buscoid:", busco_id, "_longestProt:", buscoRef[busco_id, "sequence"], sep = ""), aas[buscoRef[busco_id, "sequence"], 2], cds[buscoRef[busco_id, "sequence"], 2], busco_id, buscoRef[busco_id, "sequence"], species)
  }
}

allSequence$sequenceCDS = lapply(allSequence$sequenceCDS, function(x) if (translate(x[(length(x) - 2) : length(x)]) == '*')
{toupper(x[1 : (length(x) - 3)])} else {toupper(x[1 : (length(x))])}) #supprime le dernier codon s'il est STOP=*



############################################################################


for (i in rownames(allSequence)) { #verifie que la CDS correspond a la AAS
    this.translation = translate(allSequence[i, 'sequenceCDS'][[1]])
    if (length(allSequence[i, 'sequenceCDS'][[1]])/3 != length(toupper(allSequence[i, 'sequenceAAS'][[1]])) |
        ! all(this.translation == toupper(allSequence[i, 'sequenceAAS'][[1]])))
        { allSequence = allSequence[- which(rownames(allSequence) == i),]
    }
}

# Write per species the number of genes for which we got the cds and aas, corresponding
information_table$No_AAS_CDS_corresponding = table(allSequence$species)[information_table$species]




list_busco_to_study = list.files(list_busco_to_study_path)
list_busco_to_study = as.character(sapply(list_busco_to_study,function(x) str_split(x,".fa")[[1]][1]))



rownames(information_table) = information_table$species
information_table$nb_gene_to_study = 0

for (busco_id in list_busco_to_study) {print(busco_id)
    if (busco_id %in% allSequence$busco_id ){
        information_table[sapply(allSequence[allSequence$busco_id == busco_id, 'name'],function(x) strsplit(x,"_busco")[[1]][1]),"nb_gene_to_study"] =
        information_table[sapply(allSequence[allSequence$busco_id == busco_id, 'name'],function(x) strsplit(x,"_busco")[[1]][1]),"nb_gene_to_study"] + 1
    }
}

write.table(information_table,out_tab_path, row.names=F, col.names=T, sep="\t", quote=F)

