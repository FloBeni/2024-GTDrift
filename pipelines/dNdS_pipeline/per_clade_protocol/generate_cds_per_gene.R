
#.libPaths(c( "/beegfs/data/soft/R-3.5.2/lib/R/library" , .libPaths() ))
#.libPaths(c( "/beegfs/home/fbenitiere/R/x86_64-pc-linux-gnu-library/4.2" , .libPaths() ))

library(seqinr)
library(stringr)


args = (commandArgs(TRUE))
candidate_species_path = args[1]
IDBusco = args[2]
pathData = args[3]
fileName = args[4]
gene_fasta_path = args[5]

candidate_species = read.delim(candidate_species_path,header=T)
candidate_species = candidate_species$species

# Collecte des IDbusco des arthropodes

allSequence = data.frame(matrix(ncol = 6, nrow = 0))
colnames(allSequence) = c('name', 'sequenceAAS', 'sequenceCDS', 'busco_id', 'protein', "species")

for (species in candidate_species) {
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

  for (busco_id in IDBusco) { #pour chaque gene busco recupere la cds et aas de sa proteine la plus longue
    if (busco_id %in% buscoRef$busco_id) {
      allSequence[paste(species, "_buscoid:", busco_id, "_longestProt:", buscoRef[busco_id, "sequence"], sep = ""),] =
        list(paste(species, "_buscoid:", busco_id, "_longestProt:", buscoRef[busco_id, "sequence"], sep = ""), aas[buscoRef[busco_id, "sequence"], 2], cds[buscoRef[busco_id, "sequence"], 2], busco_id, buscoRef[busco_id, "sequence"], species)
    }
  }
}

allSequence$sequenceCDS = lapply(allSequence$sequenceCDS, function(x) if (translate(x[(length(x) - 2) : length(x)]) == '*')
{toupper(x[1 : (length(x) - 3)])} else {toupper(x[1 : (length(x))])}) #supprime le dernier codon s'il est STOP=*



############################################################################


for (i in rownames(allSequence)) { #verifie que la CDS correspond a la AAS
  this.translation = translate(allSequence[i, 'sequenceCDS'][[1]])
  if (length(allSequence[i, 'sequenceCDS'][[1]])/3 != length(toupper(allSequence[i, 'sequenceAAS'][[1]])) |
    ! all(this.translation == toupper(allSequence[i, 'sequenceAAS'][[1]])))
  {allSequence = allSequence[- which(rownames(allSequence) == i),]
  }
}



write.fasta(c(allSequence[allSequence$busco_id == IDBusco, 'sequenceCDS']),
            c(allSequence[allSequence$busco_id == IDBusco, 'name']),
            file.out = gene_fasta_path)

