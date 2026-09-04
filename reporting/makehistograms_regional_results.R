########## Create world-region specific graphs at degree of urbanisation level 2 ########
########## Chris Jacobs-Crisioni, November 2024

######### Index


library(ggplot2)
library(tidyr)
library(scales)

##### Location of automatically generated output csvs (used as input)
setwd("E:/LocalData/2BURP/Indicators")

##### File suffix of run of interest
#runset<-"_v8_fixed_bucap_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_calib_20241205_54009"
runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_calib_20241205_54009" # new calibration, preferred spec?
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_updated_ETs_bu-truncation_54009" # old calibration files
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_updated_ETs_54009" # preferred option 3/12/2024
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_updated_ETs_trunc_bu_landdens_limits_54009" # run with truncation of built-up, new land-based densities. Results not promising.
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_ETs-1_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_Lewis_20241126_54009"

##### Place where graphs need to be stored
outputdir<-"C:/Users/jacochr/Documents/global_model/generate_graphs/continentresults/"

##### Regions for which results are available and graphs need to be generated
regionslist<-c("Africa",  "Europe", "North_America", "South_America", "Asia", "Australia_Oceania") #
#regionslist<-c("Africa")

#### Loop through all regions in the regions list
for (region in regionslist) {
  
  buhistdata<-read.csv(paste(region,"/Builtup_Histogram_",region,runset,".csv", sep="")) #, stringsAsFactors=FALSE)
  buhist<-subset(buhistdata, select=c(Label, Y1975, Y2000, Y2020, Y2050, Y2070, Y2100))
  
  bu<-buhist %>% pivot_longer(
    cols = !Label, 
    names_to = "Year", 
    values_to = "bu"
  )
  #bu$Year<-as.numeric(substr(bu$Year, 2, 5))
  
  ##### fig0 built-up histogram
  fig0<-ggplot(bu, aes(x=as.numeric(Label),y=log(bu), color=Year))
  fig0+
    coord_cartesian(xlim=c(0,70)) + #, ylim=c(0,0.01)) +
    geom_line(size=1) +
    xlab("Built-up fraction (%)") + ylab("Amount of observations (log scale)") + labs(color=paste("Results", region), caption=gsub("_", " ", paste(region, runset))) +
    #scale_colour_manual(values=custom_colour)+
    #expand_limits(y = 0) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "buhisto_",region,runset,".png",sep=""), width=16, height=10)
  
  
  pophistdata<-read.csv(paste(region,"/Population_Histogram_",region,runset,".csv", sep=""))
  pophist<-subset(pophistdata, select=c(Label, Y1975, Y2000, Y2020, Y2050, Y2070, Y2100))
  pop<-pophist %>% pivot_longer(
    cols = !Label, 
    names_to = "Year", 
    values_to = "Number"
  )
  
  pop$pbreak<-pop$Label
  
  
  # Create Figure 1: population histogram
  fig1<-ggplot(pop, aes(x=pbreak,y=log(Number), color=Year))
  fig1+
    #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
    # geom_bar(stat="identity") +
    geom_line(size=1) + geom_point(size=1.5) +
    xlab("Middle of population class") + ylab("Average amount of observations in class (log scale)") + labs(color=paste("Results", region), caption=gsub("_", " ", paste(region, runset))) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "pophisto_",region,runset,".png",sep=""), width=16, height=10)

  # Create Figure 2: population histogram zoomed in at lowest classes
  fig2<-ggplot(pop, aes(x=pbreak,y=log(Number), color=Year))
  fig2+
    coord_cartesian(xlim=c(0,50)) +
    # geom_bar(stat="identity") +
    geom_line(size=1) + geom_point(size=1.5) +
    xlab("Middle of population class") + ylab("Average amount of observations in class (log scale)") + labs(color=paste("Results", region), caption=gsub("_", " ", paste(region, runset))) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "pophisto_zoomed_",region,runset,".png",sep=""), width=16, height=10)
  
}
