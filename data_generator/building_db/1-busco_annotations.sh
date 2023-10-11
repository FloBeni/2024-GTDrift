
list_species=$(ls /home/fbenitiere/data/Projet-SplicedVariants/Annotations/)

for species in $list_species
do
echo $species
sp=$(echo $species | sed 's/_/%20/g')
#taxid=$(curl -X GET --header 'Accept: application/json' 'https://www.ebi.ac.uk/ena/taxonomy/rest/scientific-name/'${sp}'?binomialOnly=false' | jq -r '.[0].taxId')
AssemblyAccession=$(head /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/data_source/annotation.gff | grep "NCBI_Assembly:" | cut -d ':' -f2)

taxid=$(head /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/data_source/annotation.gff | grep "www.ncbi.nlm.nih.gov"  | cut -d '=' -f2)

taxid_species=$(efetch -db taxonomy -id $taxid -format xml | xtract -pattern Taxon -if Rank -equals "species" -element TaxId | head -n 1)
if [ -z ${taxid_species} ]; then
echo $taxid_species
    taxid_species=$(efetch -db taxonomy -id $taxid -format xml | xmlstarlet sel -t -c "//LineageEx" | xtract -pattern Taxon -if 'Rank' -equals 'species' -element TaxId | head -n 1)
    taxid_species=$(echo ${taxid_species} | cut -d " " -f 1)
fi
echo $taxid_species

mkdir -p database/BUSCO_annotations/${species}_NCBI.taxid${taxid_species}/${AssemblyAccession}
cp /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/busco_analysis/busco_to_gene_id_embryophyta database/BUSCO_annotations/${species}_NCBI.taxid${taxid_species}/${AssemblyAccession}/busco_to_gene_id_embryophyta
cp /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/busco_analysis/busco_to_gene_id_metazoa database/BUSCO_annotations/${species}_NCBI.taxid${taxid_species}/${AssemblyAccession}/busco_to_gene_id_metazoa
cp /home/fbenitiere/data/Projet-SplicedVariants/Annotations/${species}/busco_analysis/busco_to_gene_id_eukaryota database/BUSCO_annotations/${species}_NCBI.taxid${taxid_species}/${AssemblyAccession}/busco_to_gene_id_eukaryota

done

