# Create an archive of the entire database for zenodo deposit
tar czvf database_GTDrift.tar.gz database

# Create a reduced archive of the database for zenodo deposit (used by ShinyApp)
tar czvf database_ShyniApp.tar.gz --exclude='Run' database
