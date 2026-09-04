library(ggplot2)
library(tidyr)
library(scales)

print("Libraries loaded")

#### Custom colour set for degree of urbanisation level 1
custom_colour_lvl1<-c(
  "Urban centre" = "red",
  "Urban cluster" = "#fdf96f", 
  "Rural grid cells" = "#33a02c"
)
#### Custom colour set for degree of urbanisation level 2
custom_colour_lvl2<-c(
  "Urban centre" = "red",
  "Dense urban cluster" = "#884100",
  "Semi-dense urban cluster" = "#ff7f00", 
  "Suburban grid cell" = "#fdf96f", 
  "Rural cluster" = "#33a02c", 
  "Low density rural grid cell" = "#6cf32d", 
  "Very low density grid cell" = "#d1f3b1"
)
custom_colour_settlements<-c(
  "city"="red",
  "town"="#fdf96f",
  "village"="#33a02c"
)
custom_indicator_settlements<-c(
  "n"="solid",
  "size"="dashed"
)
##### Set ordering of degrees of urbanisation for graphs
ordered_lvl1<-c( 
  "Urban centre",
  "Urban cluster",
  "Rural grid cells"
)
reversed_lvl1<-c(
  "Rural grid cells",
  "Urban cluster",
  "Urban centre"
)
ordered_lvl2<-c(
  "Urban centre",
  "Dense urban cluster",
  "Semi-dense urban cluster",
  "Suburban grid cell", 
  "Rural cluster",
  "Low density rural grid cell",
  "Very low density grid cell"
)
reversed_lvl2<-c(
  "Very low density grid cell",
  "Low density rural grid cell",
  "Rural cluster",  
  "Suburban grid cell", 
  "Semi-dense urban cluster", 
  "Dense urban cluster",
  "Urban centre"
)

################## degurba characteristics
if(toupper(classification)==toupper("DegUrba_lvl1")) {
  custom_colour<-custom_colour_lvl1 
  order<-ordered_lvl1
  rev_order<-reversed_lvl1
  print("Classification schemes for degrees of urbanisation level 1")
} else if(toupper(classification)==toupper("DegUrba_lvl2")) {
  custom_colour<-custom_colour_lvl2
  order<-ordered_lvl2
  rev_order<-reversed_lvl2
  print("Classification schemes for degrees of urbanisation level 2")
} else {
  print("No classification scheme selected. Please verify the classification setting.")
}

