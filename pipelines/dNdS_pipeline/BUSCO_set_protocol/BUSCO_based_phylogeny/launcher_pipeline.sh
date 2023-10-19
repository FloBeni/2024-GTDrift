#!/bin/bash
echo '    #################################'
echo "############ Phylogenie DnDs ############"
echo "######  Credit: Florian Bénitière  ######"
echo '    #################################'

# This script generate phylogenetic tree
export pathData=/beegfs/data/XXXXXXXX/Projet-SplicedVariants/ # (1) Path to change
export pathHome=/beegfs/home/XXXXXXXX/  # (2) Path to change
export pathdNdS=${pathData}DnDs/ # (3) Path to change
export pathEch=Embryophyta_v2/ # (4) Path to change
export buscoSample=embryophyta_odb9 # (5) Path to change
export fileName=busco_to_gene_id_embryophyta # (6) Path to change

# Paths
export pathFolder=${pathdNdS}${pathEch}/
export pathScripts=${pathHome}Scripts/Projet-SplicedVariants/BUSCO_based_phylogeny/

# Creation and clear the different folder
mkdir -p  ${pathFolder}AAS/
mkdir -p  ${pathFolder}CDS/
mkdir -p  ${pathFolder}log/
mkdir -p  ${pathFolder}PRANK_CDS/
mkdir -p  ${pathFolder}PRANK_CDS_at_least_85percent/
mkdir -p  ${pathFolder}RAxML/
mkdir -p  ${pathFolder}subsets/


# Supprimer la précédente analyse
read -p "Are you sure to remove all files in ${pathFolder} ? (y/n)" -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]
then
    find ${pathFolder} -type f -delete
    ## Generation des AAS et CDS communes aux especes analysées
    Rscript --vanilla ${pathScripts}/generate_AAS_and_CDS.R ${pathFolder} ${pathData} ${pathScripts} ${buscoSample} ${fileName}
fi
