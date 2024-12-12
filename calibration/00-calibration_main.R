############################## HPC HEADER ###############################
.libPaths("/scistor/guest/twn650/R/library") # Set the library path for R packages
options(repos = c(CRAN = "https://cloud.r-project.org")) # set CRAN mirror for package installation
rv <- getRversion() # get R version
print(paste0("R version ", rv))

#########################################################################

############################## Set-up ###################################
#' Loads required packages and sets up data folder
#' input CSVs should be stored in a 'raw' folder within the data folder

# set path to data folder
folder <- "2BURP/Calibration_MW_20241120"

# storage options
del_int <- TRUE # Set to TRUE to delete intermediate files after processing
marker <- "CALI_025" # marker for variable selection sets. Set to "" to overwrite cleaned files

# define continents
continents <- c(
  "Africa",
  "Asia",
  "Australia_Oceania",
  "Europe",
  "North_America",
  "South_America"
)

# set seed for reproducibility
set.seed(1)

# Load required packages using pacman
library(pacman)
p_load(
  devtools,
  autoGLM,
  data.table,
  caret,
  ranger,
  tidyverse,
  e1071,
  vroom,
  dplyr,
  hablar,
  corrplot,
  stargazer
)

# Define paths for subfolders
folder_raw <- paste0(folder, "/raw/") # input data
folder_clean <- paste0(folder, "/processed/") # intermediate products
folder_out <- paste0(folder, "/output/") # output folder

# Create folders if they do not exist
if (!dir.exists(folder)) dir.create(folder)
if (!dir.exists(folder_raw)) dir.create(folder_raw)
if (!dir.exists(folder_clean)) dir.create(folder_clean)
if (!dir.exists(folder_out)) dir.create(folder_out)

for (continent in continents) {
  if (!dir.exists(paste0(folder_clean, continent))) dir.create(paste0(folder_clean, continent))
  if (!dir.exists(paste0(folder_out, continent))) dir.create(paste0(folder_out, continent))
  if (!dir.exists(paste0(folder_out, continent, "/results"))) dir.create(paste0(folder_out, continent, "/results"))
}

print("Finished loading packages and setting up folders")
#########################################################################

######################### Sourcing scripts ##############################
#' Sources all scripts in the correct order

source('2BURP/scripts/R/01-data_processing.R')

# crit_urb_025 <- 0.025 # overwrite default cut-off value for urban area
source('2BURP/scripts/R/02-variable_selection.R')

source('2BURP/scripts/R/03-caret_predicitive_models.R')

source('2BURP/scripts/R/04-results_table.R') # formats results, not critical for the model

#########################################################################

if(del_int == TRUE){
  unlink(folder_clean, recursive = TRUE)
  print("Intermediate files deleted")
}