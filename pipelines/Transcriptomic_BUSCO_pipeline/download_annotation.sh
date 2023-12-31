#!/bin/bash
# Download from NCBI genomes, annotations and protein sequences

# Paths
export nameSpecies=$1
export pathData=$2
export pathHome=$3
export gffOutput=$4
export gtfOutput=$5
export sif_directory=$6

# Extract species code names
echo "------------ ${nameSpecies} ------------"

# Creation of the differents directories
export pathAnnotations=${pathData}/Annotations

source ${pathHome}.bashrc

mkdir -p ${pathAnnotations}/${nameSpecies}/data_source
mkdir -p ${pathAnnotations}/${nameSpecies}/formatted_data


PathLink=$(esearch -db assembly -query ${nameSpecies}[Organism] | efetch -format docsum | xtract -pattern DocumentSummary -if RefSeq_category -equals "reference genome" -or  RefSeq_category -equals "representative genome" -element FtpPath_RefSeq)
PathLink=$(echo ${PathLink} | cut -d " " -f 1)

if [ -z ${PathLink} ]; then
    PathLink=$(esearch -db assembly -query ${nameSpecies}[Organism] | efetch -format docsum | xtract -pattern DocumentSummary -if RefSeq_category -equals "reference genome" -or  RefSeq_category -equals "representative genome" -element FtpPath_GenBank)
    PathLink=$(echo ${PathLink} | cut -d " " -f 1)
fi


BASENAME=`basename ${PathLink}`

Path=https${PathLink:3}/${BASENAME}'_genomic.gff.gz'


wget ${Path} -O ${gffOutput}.gz

gzip -d ${gffOutput}.gz

#singularity exec --bind ${pathData}:${pathData} ${sif_directory}/cufflinks_2.2.1.sif gffread ${gffOutput} -T -o ${gtfOutput}
gffread ${gffOutput} -T -o ${gtfOutput}
