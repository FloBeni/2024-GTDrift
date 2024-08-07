[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10022493.svg)](https://doi.org/10.5281/zenodo.10022493)

This archive includes all processed data that have been generated and used in the database paper 'GTDrift: A resource for exploring the interplay between genetic drift, genomic and transcriptomic characteristics in eukaryotes', as well as the scripts used to analyze the data and to generate the figures.

Developed by Florian Bénitière, Laurent Duret and Anamaria Necsulea. Laboratoire de Biométrie et Biologie Évolutive, Université Lyon 1, UMR CNRS 5558, Villeurbanne, France.

### Directories content

-   The 'database' directory is deposited on the zenodo archive [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10017653.svg)](https://doi.org/10.5281/zenodo.10017653) that can be downloaded with the script 'upload_database.sh'. A description of the files that it contains is available on the respective archive.

-   The folder 'figure' contains the necessary materials to produce the figures of the manuscript:

    -   Intermediate pannels generated by R scripts to produce figures are stored in the directory 'pannels'.

    -   Stable images used in pannels are stored in 'images_library'.

    -   Directories labeled with the extension '\_generator' contain R scripts used to produce figures.

    -   The remaining directories are where the figures are stored.

-   The 'data' directory contains processed data in tab-separated text format, which is used for generating figures and conducting analyses.

    -   The directory labeled 'dnds_phylo' houses curated data pertaining to the molecular evolutionary rate, specifically, the dN/dS ratio. This ratio signifies the rate at which non-synonymous substitutions occur in comparison to synonymous substitutions. It encompasses four distinct methods for estimation, each associated with specific taxonomic categories: 'Embryophyta', 'Metazoa', 'Eukaryota'. In an effort to encompass a broader spectrum of our species, a fourth methodology was employed named 'per clade', involving the division of clades within the metazoa taxon (see paper for methods).

        -   The 'readme' files contain characteristics about the analysis.

        -   A 'species_list.tab' table contains the set of species on which was done the analysis.

        -   In the subdirectories 'phylogeny' are stored the outputs of the pipeline, the alignments used to infer the phylogeny, the RAxML-NG log and the phylogenetic trees.

        -   In the subdirectories 'dnds' are stored the output of the pipeline, the alignments on which was estimated the dN/dS *per* concatenate.

    -   The directory labeled 'life_history_traits' contains all analyses pertaining to the collect of life history traits body length, body mass and longevity of each species.

        -   Two excel files, (metazoa.xls and embyophyta.xls) contain the life history traits extracted manually. The 'embryophyta.xls' file, while devoid of data pertaining to life history traits, serves a critical purpose in another script, where it is utilized to compile species and clade lists.

        -   A pdf file 'lht_references.pdf' contains sources regarding the life history trait values collected from articles.

        -   The subdirectory 'ADW_ML' contains all the script to reproduce and collect the life history traits by a Machine Learning approach.

        -   The subdirectory 'screen_db' contains all the script to reproduce and collect the life history traits by a screening approach.

    -   'data1.tab' encompasses per species data on life history traits, number of identified genes among the eukaryota BUSCO gene set, and the number of genes that have been associated to one annotated protein-coding gene only (Fig. 1,2,3,5,6,7,8).

    -   'data2.tab' contains for each species the sequencing depth and the proportion of annotated introns regarding their category (Fig. 9).

    -   'data3.tab' analysis of the coverage impacting the proportion of analyzable annotated introns in *Drosophila melanogaster* (Fig. 9).
    
    -   'supp_lynch_2023.tab' represents the data from the paper of Lynch et al. (2023) (DOI embr.202357561) pertaining to polymorphism-derived Ne.
    
    -   'supp_table1.tab' supplementary data table shared along with the article presenting the list of species.
    
    -   'supp_table2.tab' supplementary data table shared along with the article presenting the RNA-seq samples used in the study.
    
    -   'taxonomy.tab' is a compilation of all the species' taxonomy collected with the R package taxize using the taxID from the NCBI.

-   The 'pipelines' folder contains the bionformatics pipelines for different purposes.

    -   To calculate the dN/dS ratio ('dNdS_pipeline').

    -   To annotate BUSCO gene and analyze various alternative splicing characteristics as part of our study ('Transcriptomic_BUSCO_pipeline').

    -   To generate data table located in the 'data' directory ('data_generator').

    -   To generate data table located in the 'data' directory or to produce the 'database' located at [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10017653.svg)](https://doi.org/10.5281/zenodo.10017653) ('building_from_source' and 'building_db');

### 

Some packages that you may encounter in these R scripts should be install: - stringr - kableExtra - cowplot - ggplot2 - imager - RColorBrewer *etc.*

### 

The script 'values_in_text_paper_generator.R' retrieves the values found in the main text of the paper from the data.
