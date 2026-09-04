# Last changed 03.12.2024

print("Starting variable selection")

##################### Define variable selections ########################
# Define variables to be excluded for each continent
#' Including "latitude_factor" ensures that there is always at least one variable to exclude.
#' This is necessary because an empty exclusion list can cause errors during the variable selection process.

exclude_vars <- list()

exclude_vars$Africa <- list(
  "Distance2MajorRoads", #RED #CALI
  "Grid_costs_atleast_to_towns", #RED #CALI
  "slope", #RED #CALI
  "LandSlideProne_ARUP", #CALI
  "latitude_factor"
)

exclude_vars$Asia <- list(
  "Distance2MajorRoads", #RED #CALI
  "Grid_costs_atleast_to_towns", #RED #CALI
  # "IsFloodProneArea_RP100",
  "latitude_factor"
)

exclude_vars$Australia_Oceania <- list(
  "Distance2MajorRoads", #RED #CALI
  "Grid_costs_atleast_to_towns", #RED #CALI
  "IsProtectedArea", #CALI
  # "IsFloodProneArea_RP100",
  "latitude_factor"
)

exclude_vars$Europe <- list(
  "Distance2MajorRoads", #RED #CALI
  "Grid_costs_atleast_to_towns", #RED #CALI
  # "IsProtectedArea",
  "latitude_factor"
)

exclude_vars$North_America <- list(
  "Distance2MajorRoads", #RED #CALI
  "Grid_costs_atleast_to_towns", #RED #CALI
  "latitude_factor"
)

exclude_vars$South_America <- list(
  "Distance2MajorRoads", #RED #CALI
  "Grid_costs_atleast_to_towns", #RED #CALI
  "Distance2LargeInlandWater", #CALI
  # "IsFloodProneArea_RP100",
  "LandSlideProne_ARUP", #CALI
  "latitude_factor"
)

# Full variable list
#' List should contain all variables to be fed to the model
#' Also include "latitude_factor" as a guaranteed drop variable to avoid empty lists
all_vars <- c(
  "Urban_2020_025", # dependent variable
  "ln_PopulationDensity_2000_8dir", # ln neighbourhood population density 1990
  "Distance2Coastline", # distance to coastline
  "Distance2LargeInlandWater", # distance to freshwater
  "Distance2MajorRoads", # distance to main roads
  "Distance2SecundaryRoads", # distance to secondary roads
  "Grid_costs_atleast_to_cities", # grid cost to at least nearest city
  "Grid_costs_atleast_to_towns", # grid cost to at least nearest town
  "Grid_costs_atleast_to_villages", # grid cost to at least nearest village
  "Elevation", # elevation
  "slope", # slope
  "TRI_mean", # terrain roughness index
  "IsProtectedArea", # protected area
  "IsFloodProneArea_RP100", # flood prone area
  "Earthquakes_MMI_Index", # earthquake intensity
  "LandSlideProne_ARUP", # landslide prone
  "latitude_factor" # GUARANTEED DROP
)

#########################################################################

###################### Execute variable selection #######################

for (continent in continents) {
  print(paste0("Selecting variables for ", continent))

  df <- fread(
    paste0(folder_clean, continent, "/calibset_urban.csv"),
    na.strings = c("NA", "N/A", "null")
  )
  df <- df[complete.cases(df), ]

  df <- df %>%
    dplyr::select(all_of(all_vars)) %>%
    dplyr::select(!all_of(unlist(exclude_vars[[continent]])))

  fwrite(df, file = paste0(folder_clean, continent, "/calibset_urban_", marker, ".csv"))

  rm(df)
  gc()
}

# you can restart your session here
#########################################################################

print("Finished variable selection")