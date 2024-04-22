# Download the archive containing the database
wget -O database.zip https://zenodo.org/api/records/10908656/files-archive

# Uncompress the archive
unzip database.zip -d database
tar -xvzf database/Transcriptomic.tar.gz
tar -xvzf database/dNdS.tar.gz
tar -xvzf database/BUSCO_annotations.tar.gz

# Remove the archive
rm database/*.tar.gz
