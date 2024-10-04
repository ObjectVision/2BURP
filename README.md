# 2BURP
To Build Urban and Rural Projections

For more information on how to install GeoDMS and load a configuration, look here: https://github.com/ObjectVision/GeoDMS_Academy/wiki/Module-0%2C-Install-GeoDMS-GUI-and-setup-a-configuration

This project's configuration can be downloaded from here: https://github.com/ObjectVision/2BURP/archive/refs/heads/main.zip

This model can be used to disaggregate Built-up area and population projections. It is set up so that projections per functional area (created in the NetworkModel_World repo: https://github.com/ObjectVision/NetworkModel_World) are read in from a CSV, which can be found in /SourceData/Claims/Read_CSV.

Several settings are available in the '/ModelParameters', such as the study area. The model is usually run per continent, which can be chosen in the 'StudyArea_manually' parameter.  

How to run the model
1. First, choose your study area in the ModelParameters. If changed, reload the configuration (alt + r)
2. Generate pre-processed data in /SourceData/MakeUnlinkedData, first run b1 and b2, then reload, generate b3, reload, genrate b4 and b5, and reload. 
3. Now you have all the necessary files. And the actual model can be run. Make sure the ModelParameter/Use_TempTifFiles is set to FALSE.
4. To generate all model outcome files and write them to tiff files, double-click on /Analysis/Future/Indicators/Prep/Generate_All. This will generate population, built-up, and degree of urbanisation grid files for all decades.
5. When finished, if you want to analyse the different indicators, it would be efficient to set ModelParameter/Use_TempTifFiles to TRUE and reload. Now, the generated TIFF files are used instead of running the model again.
6. Now, a range of indicators can be assessed in the /Analysis/Future/Indicators container.
