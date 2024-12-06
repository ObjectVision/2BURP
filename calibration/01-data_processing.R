# Last changed 26.11.2024

print("Starting data processing")

########################### Loading data ###############################
for (continent in continents) {
  df <- vroom(paste0(folder_raw, continent, "/calibset.csv"), na = c("NA", "N/A", "null", "-9999"))
  df_dropna <- df[complete.cases(df), ] # remove rows w/ NA

  # export cleaned dataset, shift explanatory var to first column
  fwrite(
    cbind(df_dropna),
    file = paste0(folder_clean, continent, "/calibset_noNA.csv")
  )

  print(paste0("Finished cleaning ", continent))
  rm(df_dropna, df)
}

# you can restart your session here
########################################################################

####################### Calculate variables ############################
print("Starting to calculate variables")

# cut-off for urban area
if (!exists("crit_urb_025")) {
  cat("no cut-off for urban area defined. Defaulting to 0.025")
  crit_urb_025 <- 0.025 # suggested in meeting 15/4/2024
}

for (continent in continents) {
  df <- fread(
    paste0(folder_clean, continent, "/calibset_noNA.csv"),
    na.strings = c("NA", "N/A", "null")
  )
  df <- df[complete.cases(df), ]
  stargazer(df, type = "html", out = paste0(folder_out, continent, "/desc_calibset_noNA.html"))

  df <- df %>%
    mutate(
      # create urban dummy variables
      Urban_2020_025 = ifelse(BUPERCTOT_2020 > crit_urb_025, 1, 0),
      Urban_2000_025 = ifelse(BUPERCTOT_2000 > crit_urb_025, 1, 0),

      # ln transform population density (0 -> 0.1, consistent w/ CEUS-paper)
      ln_PopulationDensity_2020_8dir = log(PopulationDensity_2020_8dir + .1),
      ln_PopulationDensity_2000_8dir = log(PopulationDensity_2000_8dir + .1),

      # combine distance to road variables
      Distance2AnyRoads = ifelse(
        Distance2MajorRoads < Distance2SecundaryRoads, Distance2MajorRoads, Distance2SecundaryRoads
      ),

      # dummify LandSlide_risk (4 = high, 3 = moderate, 2 = low, 1 = very low/none)
      LandSlideProne_ARUP = ifelse(LandSlide_risk >= 3, 1, 0) # high and moderate risk
    )

  # store data, shift dependent variable to the front
  colnames(df)
  fwrite(
    cbind(df),
    file = paste0(folder_clean, continent, "/calibset_urban.csv")
  )
  stargazer(df, type = "html", out = paste0(folder_out, continent, "/desc_calibset_urban.html"))

  print(paste0("Finished calculating variables for ", continent))
  rm(df)
}

# you can restart your session here
########################################################################

print("Finished data processing")