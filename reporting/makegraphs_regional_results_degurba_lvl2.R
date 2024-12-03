########## Create world-region specific graphs at degree of urbanisation level 2 ########
########## Chris Jacobs-Crisioni, November 2024

######### Index
# fig0: total population graph
# fig1: population density graph
# fig2: loglinear area graph
# fig3: sum abandoned since prior population grid cells graph
# fig4: stacked population bar graph
# fig5: areas as indices line graph

library(ggplot2)
library(tidyr)
library(scales)

##### Location of automatically generated output csvs (used as input)
setwd("E:/LocalData/2BURP/Indicators")

##### Place where graphs need to be stored
outputdir<-"C:/Users/jacochr/Documents/global_model/generate_graphs/continentresults/"

##### File suffix of run of interest
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_updated_ETs_trunc_bu_landdens_limits_54009" # run with truncation of built-up, new land-based densities. Results not promising.
runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_updated_ETs_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_ETs-1_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_Lewis_20241126_54009"

##### Regions for which results are available and graphs need to be generated
regionslist<-c("Africa",  "Europe", "North_America", "South_America", "Asia", "Australia_Oceania") #
#regionslist<-c("Africa")

#### Custom colour set for degree of urbanisation level 2
custom_colour<-c(
  "Urban centre" = "red",
  "Dense urban cluster" = "#884100",
  "Semi-dense urban cluster" = "#ff7f00", 
  "Suburban grid cell" = "#fdf96f", 
  "Rural cluster" = "#33a02c", 
  "Low density rural grid cell" = "#6cf32d", 
  "Very low density grid cell" = "#d1f3b1"
)

counter<-0

#### Loop through all regions in the regions list
for (region in regionslist) {
  
  ##### Read population data and put it in a long form required by ggplot
  popdata<-read.csv(paste(region,"/Population_perDoU_",region,runset,".csv", sep="")) #, stringsAsFactors=FALSE)
  pop<-popdata %>% pivot_longer(
    cols = !Label, 
    names_to = "Year", 
    values_to = "Pop"
  )
  
  ##### Read area data and put it in a long form required by ggplot
  areadata<-read.csv(paste(region,"/Area_perDoU_",region,runset,".csv", sep="")) #, stringsAsFactors=FALSE)
  area<-areadata %>% pivot_longer(
    cols = !Label, 
    names_to = "Year", 
    values_to = "Area"
  )
  
  ##### Combine population and area data, discard redundant columns
  plotdata<-cbind(pop, area)
  plotdata<-subset(plotdata,select=-c(Year, Label))
  plotdata$Year<-as.numeric(substr(plotdata$Year, 2, 5))
  plotdata$Region<-region
  
  if(counter==0) {
    all_data<-plotdata
  } else {
    all_data<-rbind(all_data, plotdata)    
  }
  
  ##### Set ordering of degrees of urbanisation for graphs
  plotdata$degurb_ordered<-factor(plotdata$Label, levels=c(
      "Urban centre","Dense urban cluster","Semi-dense urban cluster", "Suburban grid cell", 
      "Rural cluster", "Low density rural grid cell", "Very low density grid cell"
      ))
 
  # Create Figure 0: total population graph
  fig0<-ggplot(plotdata, aes(x=Year,y=Pop / (1000 * 1000), color=degurb_ordered))
  fig0+
    #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
    geom_line(size=1) +
    xlab("Year") + ylab("Population (M)") + labs(color=paste("Results", region), caption=gsub("_", " ", paste(region, runset))) +
    scale_colour_manual(values=custom_colour)+
    expand_limits(y = 0) +
    geom_vline(xintercept=2020, linetype="dashed")+
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "pop_",region,runset,".png",sep=""), width=16, height=10)
  
  # Create Figure 1: population density graph
  fig1<-ggplot(plotdata, aes(x=Year,y=Pop / Area, color=degurb_ordered))
  fig1+
    #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
    geom_line(size=1) +
    xlab("Year") + ylab("Average population density") + labs(color=paste("Results", region), caption=gsub("_", " ", paste(region, runset))) +
    scale_colour_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    #scale_x_continuous(labels = scales::percent) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "popdens_",region,runset,".png",sep=""), width=16, height=10)
  
  # Create figure 2: loglinear area graph
  fig2<-ggplot(plotdata, aes(x=Year,y=log(Area), color=degurb_ordered))
  fig2+
    #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
    geom_line(size=1) +
    xlab("Year") + ylab("Area (ln)") + labs(color=paste("Results", gsub("_", " ", region)), caption=gsub("_", " ", paste(region, runset))) +
    scale_colour_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    #scale_x_continuous(labels = scales::percent) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "ln_area_",region,runset,".png",sep=""), width=16, height=10)
  
  ####### Explore abandonment
  
  # read abandonment
  abandata<-read.csv(paste(region,"/Abandoned_since_prev_decade_perDoU_",region,runset,".csv", sep="")) 
  abandata$c<-1
  aban<-aggregate(abandata[, -1], by=list(abandata$c), FUN="sum")
  aban<-subset(aban,select=-c(Group.1, c))
  aban$country<-c(region)
  
  # transform to long format
  aban<-aban %>% pivot_longer(
    cols= !country,
    names_to = "Year", 
    values_to = "Abandoned"
  )
  aban$Year<-as.numeric(substr(aban$Year, 2, 5))
  
  # Create figure 3: sum abandoned since prior population grid cells graph
  fig3<-ggplot(aban, aes(x=Year,y=Abandoned / 1000))
  fig3+
    #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
    geom_line(size=1) +
    expand_limits(y = 0) +
    xlab("Year") + ylab("Abandoned since prior (k)") + labs(caption=gsub("_", " ", paste(region, runset))) +
    geom_vline(xintercept=2020, linetype="dashed")+
    #scale_x_continuous(labels = scales::percent) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "abandoned_",region,runset,".png",sep=""), width=16, height=10)
  
  # Create stacked population graph (percentages per degurba)
  popsharesdata<-popdata
  
  # throw out 1975, 1985, ... 2015 to make sure that all bars represent equal width areas
  popsharesdata<-subset(popsharesdata,select=-c(Y1975, Y1985, Y1995, Y2005, Y2015))
  
  # compute percentages
  for (i in colnames(popsharesdata)) {
    if(is.numeric(popsharesdata[[i]])) {
      popsharesdata[[i]]<-      popsharesdata[[i]] / sum(popsharesdata[[i]])
    }
  }
  
  # transform to long form
  popshares<-popsharesdata %>% pivot_longer(
    cols = !Label, 
    names_to = "Year", 
    values_to = "Popshare"
  )
  popshares$Year<-as.numeric(substr(popshares$Year, 2, 5))
  popshares$degurb_ordered<-factor(popshares$Label, levels=c(
    "Very low density grid cell",
    "Low density rural grid cell",
    "Rural cluster",  
    "Suburban grid cell", 
    "Semi-dense urban cluster", 
    "Dense urban cluster",
    "Urban centre"
  ))
  
  # Create figure 4: stacked population bar graph
  fig4<-ggplot(popshares, aes(x=Year-5, y=Popshare,  fill=degurb_ordered))
  fig4+
    geom_bar(stat="identity", width=10) +
    xlab("Year") + ylab("Share of total population") + labs(fill=paste("Results", gsub("_", " ", region)), caption=gsub("_", " ", paste(region, runset))) +
    scale_colour_manual(values=custom_colour)+
    scale_fill_manual(values=custom_colour)+
    expand_limits(y = 0) +
    geom_vline(xintercept=2020, linetype="dashed")+
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "popshares_",region,runset,".png",sep=""), width=16, height=10)
  
  # compute percentages
  tad<-areadata
  for (i in colnames(areadata)) {
    if(is.numeric(areadata[[i]])) {
      areadata[[i]]<-      (tad[[i]] / tad$Y2020)*100
    }
  }
  
  ###### indexed area per degurba
  
  indexed_area<-areadata %>% pivot_longer(
    cols = !Label, 
    names_to = "Year", 
    values_to = "Area"
  )
  
  indexed_area$Year<-as.numeric(substr(indexed_area$Year, 2, 5))
  indexed_area$degurb_ordered<-factor(indexed_area$Label, levels=c(
    "Very low density grid cell",
    "Low density rural grid cell",
    "Rural cluster",  
    "Suburban grid cell", 
    "Semi-dense urban cluster", 
    "Dense urban cluster",
    "Urban centre"
  ))
  
  fig5<-ggplot(indexed_area, aes(x=Year,y=Area, color=degurb_ordered))
  fig5+
    #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
    geom_line(size=1) +
    xlab("Year") + ylab("Area change (2020 = 100)") + labs(color=paste("Results", gsub("_", " ", region)), caption=gsub("_", " ", paste(region, runset))) +
    scale_colour_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    #scale_x_continuous(labels = scales::percent) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
    theme_light()
  
  ggsave(paste(outputdir, "index_area_",region,runset,".png",sep=""), width=16, height=10)
  
  counter=counter+1
  
}
write.csv(all_data, paste("pop_area_perDoU_",runset,".csv", sep=""))
