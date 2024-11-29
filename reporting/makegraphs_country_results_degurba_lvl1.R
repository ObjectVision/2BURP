########## Create country specific graphs at degree of urbanisation level 1 ########
########## Chris Jacobs-Crisioni, November 2024

######### Index
# fig0: total population graph
# fig1: population density graph
# fig4: stacked population bar graph

library(ggplot2)
library(tidyr)
library(scales)

##### Location of automatically generated output csvs (used as input)
setwd("E:/LocalData/2BURP/Indicators")

##### Place where graphs need to be stored
outputdir<-"C:/Users/jacochr/Documents/global_model/generate_graphs/countryresults/"

##### File suffix of run of interest
runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_updated_ETs_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_Lewis_20241126_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_ETs-1_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_54009"

##### Regions for which results are available and graphs need to be generated
regionslist<-c("Africa", "Asia", "Australia_Oceania", "Europe", "North_America", "South_America")
#regionslist<-c("Europe")

#### Custom colour set for degree of urbanisation level 1
custom_colour<-c(
  "Urban centre" = "red",
  "Urban cluster" = "#fdf96f", 
  "Rural grid cells" = "#33a02c"
)

counter<-0

#### Loop through all regions in the regions list
for (region in regionslist) {
  
  # load results that are in the form of a cartesian product of countries in continent x degurba lvl 1 classes
  in_popdata<-read.csv(paste(region,"/Pop_per_country_per_DoU_lvl1_",region,runset,".csv", sep="")) #, stringsAsFactors=FALSE)
  in_areadata<-read.csv(paste(region,"/Area_per_country_per_DoU_lvl1_",region,runset,".csv", sep="")) #, stringsAsFactors=FALSE)
  
  # list all unique modelled country names from in_popdata results
  countrieslist<-unique(in_popdata$country)
  
  # Loop through all countries in the countries list
  for (selcountry in countrieslist) {
    
    # internal output to track progress
    print(paste(region, selcountry))
    
    #### prep total pop and popdensity graphs by putting them into long form (required by ggplot)
    popdata<-subset(in_popdata, country==selcountry)
    areadata<-subset(in_areadata, country==selcountry)
    
    pop<-popdata %>% pivot_longer(
      cols = !Degurba & !country, 
      names_to = "Year", 
      values_to = "Pop"
    )
    
    area<-areadata %>% pivot_longer(
      cols = !Degurba & !country, 
      names_to = "Year", 
      values_to = "Area"
    )
    
    ##### Combine population and area data, discard redundant columns
    plotdata<-cbind(pop, area)
    plotdata<-subset(plotdata,select=-c(Year, Degurba))
    plotdata$Year<-as.numeric(substr(plotdata$Year, 2, 5))
    
    if(counter==0) {
      all_data<-plotdata
    } else {
      all_data<-rbind(all_data, plotdata)    
    }
    
    ##### Set ordering of degrees of urbanisation for graphs
    plotdata$degurb_ordered<-factor(plotdata$Degurba, levels=c(
      "Rural grid cells",
      "Urban cluster",
      "Urban centre"
    ))
    
    # Create Figure 0: total population graph
    fig0<-ggplot(plotdata, aes(x=Year,y=Pop / (1000 * 1000), color=degurb_ordered))
    fig0+
      #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
      geom_line(size=1) +
      xlab("Year") + ylab("Population (M)") + labs(color=paste("Results", selcountry), caption=gsub("_", " ", paste(region, runset))) +
      scale_colour_manual(values=custom_colour)+
      geom_vline(xintercept=2020, linetype="dashed")+
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
      theme_light()
    
    ggsave(paste(outputdir, "pop_",selcountry,runset,".png",sep=""), width=16, height=10)
    
    # Create Figure 1: population density graph
    fig1<-ggplot(plotdata, aes(x=Year,y=Pop / Area, color=degurb_ordered))
    fig1+
      #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
      geom_line(size=1) +
      xlab("Year") + ylab("Average population density") + labs(color=paste("Results", selcountry), caption=gsub("_", " ", paste(region, runset))) +
      scale_colour_manual(values=custom_colour)+
      geom_vline(xintercept=2020, linetype="dashed")+
      #scale_x_continuous(labels = scales::percent) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
      theme_light()
    
    ggsave(paste(outputdir, "popdens_",selcountry,runset,".png",sep=""), width=16, height=10)
    
    # Create stacked population graph (percentages per degurba)
    popsharesdata<-popdata
    
    # throw out 1975, 1985, ... 2015 to make sure that all bars represent equal width areas
    popsharesdata<-subset(popsharesdata,select=-c(country, Y1975, Y1985, Y1995, Y2005, Y2015))
    
    # compute percentages
    for (i in colnames(popsharesdata)) {
      if(is.numeric(popsharesdata[[i]])) {
        popsharesdata[[i]]<-      popsharesdata[[i]] / sum(popsharesdata[[i]])
      }
    }
    
    # transform to long form
    popshares<-popsharesdata %>% pivot_longer(
      cols = !Degurba, 
      names_to = "Year", 
      values_to = "Popshare"
    )
    popshares$Year<-as.numeric(substr(popshares$Year, 2, 5))
    
    ##### Set ordering of degrees of urbanisation for graphs
    popshares$degurb_ordered<-factor(popshares$Degurba, levels=c(
      "Rural grid cells",
      "Urban cluster",
      "Urban centre"
    ))
    
    # Create figure 4: stacked population bar graph
    fig4<-ggplot(popshares, aes(x=Year-5, y=Popshare,  fill=degurb_ordered))
    fig4+
      geom_bar(stat="identity", width=10) +
      xlab("Year") + ylab("Share of total population") + labs(fill=paste("Results", gsub("_", " ", selcountry)), caption=gsub("_", " ", paste(region, runset))) +
      scale_colour_manual(values=custom_colour)+
      scale_fill_manual(values=custom_colour)+
      geom_vline(xintercept=2020, linetype="dashed")+
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
      theme_light()
     
    ggsave(paste(outputdir, "popshares_",selcountry,runset,".png",sep=""), width=16, height=10)
    
    counter=counter+1
 
  }
}
write.csv(all_data, paste("pop_area_countries_perDoU_",runset,".csv", sep=""))