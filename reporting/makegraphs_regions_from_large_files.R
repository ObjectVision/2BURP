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
setwd("E:/LocalData/2BURP/export")

##### Place where graphs need to be stored
outputdir<-"C:/Users/jacochr/Documents/global_model/generate_graphs/region_results/"

#### Custom colour set for degree of urbanisation level 1
custom_colour<-c(
  "Urban centre" = "red",
  "Urban cluster" = "#fdf96f", 
  "Rural grid cells" = "#33a02c"
)

# in_filename<-"2burp_regions_result_new_calib_20241205"
in_filename<-"un_regions_result_new_calib_20241205"

in_popdata<-read.csv(paste(in_filename, ".csv", sep="")) #, stringsAsFactors=FALSE)

  # list all unique modelled country names from in_popdata results
  regionslist<-unique(in_popdata$Region)
  
  # Loop through all regionslist in the countries list
  for (selregion in regionslist) {
    
    # internal output to track progress
    print(paste(selregion))
    
    #### prep total pop and popdensity graphs by putting them into long form (required by ggplot)
    popdata<-subset(in_popdata, Region==selregion)
   
    pop<-popdata %>% pivot_longer(
      cols = !degurba_code & !Region, 
      names_to = "Year", 
      values_to = "Pop"
    )
    pop$Year<-as.numeric(substr(pop$Year, 2, 5))
    
    pop$Degurba[pop$degurba_code==1]<-"Rural grid cells"
    pop$Degurba[pop$degurba_code==2]<-"Urban cluster"
    pop$Degurba[pop$degurba_code==3]<-"Urban centre"
    
    plotdata<-pop
    
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
      xlab("Year") + ylab("Population (M)") + labs(color="Results", title=paste("Total population ", gsub("_", " ", selregion),sep=""), caption=gsub("_", " ", paste(selregion, in_filename, sep=", "))) +
      scale_colour_manual(values=custom_colour)+
      expand_limits(y = 0) +
      geom_vline(xintercept=2020, linetype="dashed")+
      geom_vline(xintercept=1975, linetype="dashed")+
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
      theme_light()
    
    ggsave(paste(outputdir, "pop_",selregion,"_",in_filename,".png",sep=""), width=16, height=10)

    # throw out 1975, 1985, ... 2015 to make sure that all bars represent equal width areas
    popshares<-subset(pop, Year %% 10==0)
    
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
      xlab("Year") + ylab("Share of total population") + labs(color="Results", title=paste("Population share ", gsub("_", " ", selregion),sep=""), caption=gsub("_", " ", paste(selregion, in_filename, sep=", "))) +
      scale_colour_manual(values=custom_colour)+
      scale_fill_manual(values=custom_colour)+
      expand_limits(y = 0) +
      geom_vline(xintercept=2020, linetype="dashed")+
      geom_vline(xintercept=1970, linetype="dashed")+
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.title=element_text(size=14)) +
      theme_light()
     
    ggsave(paste(outputdir, "popshares_",selregion,"_",in_filename,".png",sep=""), width=16, height=10)

  }
