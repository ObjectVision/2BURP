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
runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_calib_20241205_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_updated_ETs_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_Lewis_20241126_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_stylised_ETs-1_54009"
#runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_popdraw_54009"

##### Regions for which results are available and graphs need to be generated
regionslist<-c("Africa", "Asia", "Australia_Oceania", "Europe", "North_America", "South_America")
#regionslist<-c("Europe", "Asia")

#### Custom colour set for degree of urbanisation level 1
custom_colour<-c(
  "Urban centre" = "red",
  "Urban cluster" = "#fdf96f", 
  "Rural grid cells" = "#33a02c"
)

names_data<-read.csv("D:/ProjDir/2BURP/reporting/UNSD - Methodology.csv", sep=";")
names_data$M49<-as.character(names_data$M49.Code)
names_data$M49[names_data$M49.Code<100]<-paste0("0",names_data$M49[names_data$M49.Code<100])
names_data$M49[names_data$M49.Code<10]<-paste0("0",names_data$M49[names_data$M49.Code<10])

#prep backcast database to fit the reporting dataframe (column names, values)
backcast_data<-read.csv("D:/ProjDir/2BURP/reporting/df_smod_est_v7.csv")
backcast_data$country<-backcast_data$iso3c
backcast_data$Year<-backcast_data$year
backcast_data$Degurba<-backcast_data$smod
backcast_data$Pop<-backcast_data$pop_WPP2022 * (1000*1000) # multiplied with 1M because the original values were in millions
backcast_data<-subset(backcast_data,select=-c(pop_WPP2022, smod, iso3c, year))

counter<-0
ruscounter<-0

#### Loop through all regions in the regions list
for (region in regionslist) {
  
  # load results that are in the form of a cartesian product of countries in continent x degurba lvl 1 classes
  in_popdata<-read.csv(paste(region,"/Pop_per_country_per_DoU_lvl1_",region,runset,".csv", sep="")) #, stringsAsFactors=FALSE)
 
  # list all unique modelled country names from in_popdata results
  countrieslist<-unique(in_popdata$country)
  
  # Loop through all countries in the countries list
  for (selcountry in countrieslist) {
    
    # internal output to track progress
    print(paste(region, selcountry))
    
    #### prep total pop and popdensity graphs by putting them into long form (required by ggplot)
    popdata<-subset(in_popdata, country==selcountry)
    
    if(selcountry=="ANR") {selcountry<-"AND"}
    if(selcountry=="KOS") {selcountry<-"XKO"}
    
    if(selcountry=="ERUS" | selcountry=="ARUS") {
      ruscounter<-ruscounter+1
      if(ruscounter<2) {
        rusdata<-popdata
      } else {
        popdata<-cbind(popdata[c(1,2)], popdata[-c(1,2)] + rusdata[-c(1,2)])
        selcountry="RUS"
        popdata$country=selcountry
      }
    }
    
    pop<-popdata %>% pivot_longer(
      cols = !Degurba & !country, 
      names_to = "Year", 
      values_to = "Pop"
    )
    pop$Year<-as.numeric(substr(pop$Year, 2, 5))
    
    # subset the backcasting data to add
    bc_data<-subset(backcast_data, country==selcountry & Year < 1975)
    
    ##### Combine population and area data, discard redundant columns
    plotdata<-rbind(pop, bc_data)
    
    cname<-names_data$Country.or.Area[names_data$ISO.alpha3.Code==selcountry]
    if(length(cname) < 1) {cname<-selcountry}
    cm49<-names_data$M49[names_data$ISO.alpha3.Code==selcountry]
    if(length(cm49) < 1) {cm49<-"000"}
    
    plotdata$Country_Name<-cname
    plotdata$M49<-cm49
    plotdata$degurba_code<-1
    plotdata$degurba_code[plotdata$Degurba=="Urban cluster"]<-2
    plotdata$degurba_code[plotdata$Degurba=="Urban centre"]<-3
    
    
    
    if(counter==0) {
      all_data<-plotdata
    } else {
      all_data<-rbind(all_data, plotdata)    
    }

    ##### Set ordering of degrees of urbanisation for graphs
    plotdata$degurb_ordered<-factor(plotdata$Degurba, levels=c(
      "Urban centre",
      "Urban cluster",
      "Rural grid cells"
    ))
    
    # Create Figure 0: total population graph
    fig0<-ggplot(plotdata, aes(x=Year,y=Pop / (1000 * 1000), color=degurb_ordered))
    fig0+
      #coord_cartesian(xlim=c(0,0.045), ylim=c(-500, 2000)) +
      geom_line(size=1) +
      xlab("Year") + ylab("Population (M)") + labs(color=paste("Results", selcountry), title=(paste("Total population", cname)), caption=gsub("_", " ", paste(cname, region, runset, sep=", "))) +
      scale_colour_manual(values=custom_colour)+
      expand_limits(y = 0) +
      geom_vline(xintercept=2020, linetype="dashed")+
      geom_vline(xintercept=1975, linetype="dashed")+
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
      theme_light()
    
    ggsave(paste(outputdir, "pop_",selcountry,runset,".png",sep=""), width=16, height=10)

    # throw out 1975, 1985, ... 2015 to make sure that all bars represent equal width areas
    popshares<-subset(plotdata, Year %% 10==0)
    
    ##### Set ordering of degrees of urbanisation for graphs
    popshares$degurb_ordered<-factor(popshares$Degurba, levels=c(
      "Rural grid cells",
      "Urban cluster",
      "Urban centre"
    ))
    
    # Create figure 4: stacked population bar graph
    fig4<-ggplot(popshares, aes(x=Year-5, y=Pop,  fill=degurb_ordered))
    fig4+
      geom_bar(position="fill", stat="identity", width=10) +
      xlab("Year") + ylab("Share of total population") + labs(fill=paste("Results", gsub("_", " ", selcountry)), title=(paste("Total population", cname)), caption=gsub("_", " ", paste(cname, region, runset, sep=", "))) +
      scale_colour_manual(values=custom_colour)+
      scale_fill_manual(values=custom_colour)+
      expand_limits(y = 0) +
      geom_vline(xintercept=2020, linetype="dashed")+
      geom_vline(xintercept=1970, linetype="dashed")+
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
      theme_light()
     
    ggsave(paste(outputdir, "popshares_",selcountry,runset,".png",sep=""), width=16, height=10)
    
    counter=counter+1
 
  }
}
write.csv(all_data, paste("pop_area_countries_perDoU_",runset,".csv", sep=""))