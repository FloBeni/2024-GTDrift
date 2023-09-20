
list_species = $(ls /home/fbenitiere/data/Projet-SplicedVariants/Annotations/)

for species in $list_species
do
echo $species
sp=$(echo $species | sed 's/_/%20/g')
txid=$(curl -X GET --header 'Accept: application/json' 'https://www.ebi.ac.uk/ena/taxonomy/rest/scientific-name/'${sp}'?binomialOnly=false' | jq -r '.[0].taxId')
AssemblyAccession=$(grep "NCBI_Assembly:" /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/data_source/annotation.gff | cut -d ':' -f2)

mkdir -p database/BUSCO_annotations/${species}_NCBI.txid${txid}/${AssemblyAccession}
cp /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/busco_analysis/busco_to_gene_id_embryophyta database/BUSCO_annotations/${species}_NCBI.txid${txid}/${AssemblyAccession}/busco_to_gene_id_embryophyta
cp /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/busco_analysis/busco_to_gene_id_metazoa database/BUSCO_annotations/${species}_NCBI.txid${txid}/${AssemblyAccession}/busco_to_gene_id_metazoa
cp /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/busco_analysis/busco_to_gene_id_eukaryota database/BUSCO_annotations/${species}_NCBI.txid${txid}/${AssemblyAccession}/busco_to_gene_id_eukaryota

done
