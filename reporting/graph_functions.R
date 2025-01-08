print("Loading project-specific graph templates")

font_size<-14

#---------------- function to set fonts definition
fonts_def<-function(infontsize=20)  {
  out_def<- theme(
    plot.title=element_text(size=infontsize*1.11), 
    plot.caption=element_text(size=infontsize*0.5), 
    axis.text=element_text(size=infontsize*0.9), 
    axis.title=element_text(size=infontsize), 
    legend.title=element_text(size=infontsize),
    legend.text=element_text(size=infontsize*0.8))
  return(out_def)
}
  
#----------------- graph to show absolute population over time
#
# Chris Jacobs-Crisioni, 3/1/2025
#
# presupposes a long form dataset
# also: 
#   a custom_colour element aligned with the cvar values
#
# -------------------------------------------------------------
absolute_pop_graph<-function(
    plotdata,               # dataframe (long format)
    printname,              # reporting unit name to be printed in the graph
    storename=printname,    # reporting unit name to be put in the filename (defaults to printname)
    xvar=plotdata$year,     # override for x variable
    yvar=plotdata$pop,      # override for y variable
    cvar=plotdata$degurb_ordered,    # override for colour variable
    soutputdir=if(exists("outputdir")) {outputdir} else {(getwd())} #extended definition
    ) 
  {
  if(max(yvar)>1000*1000) {
    div_fac<-1000*1000
    y_label<-"Population (M)"
  } else {
    div_fac<-1000
    y_label<-"Population (k)"
  }
  fig0<-ggplot(plotdata, aes(x=xvar,y=yvar / div_fac, color=cvar))
  fig0+
    geom_line(size=1) +
    xlab("Year") + ylab(y_label) + 
    labs(title= paste("Population", gsub("_", " ", printname)),color="Results", caption=gsub("_", " ", paste(printname, runset))) +
    scale_x_continuous(expand = c(0, 0))+
    scale_colour_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    geom_vline(xintercept=1975, linetype="dashed")+
    theme_light()+fonts_def(font_size)
  
  ggsave(paste(soutputdir, "pop_",storename,".png",sep=""), width=16, height=10)
}

#----------------- graph to show indexed area over time
#
# Chris Jacobs-Crisioni, 3/1/2025
#
# presupposes a long form dataset
# also: 
#   a custom_colour element aligned with the cvar values
#
# -------------------------------------------------------------
indexed_area_graph<-function(
    plotdata,               # dataframe (long format)
    printname,              # reporting unit name to be printed in the graph
    storename=printname,    # reporting unit name to be put in the filename (defaults to printname)
    xvar=plotdata$year,     # override for x variable
    yvar=plotdata$area,      # override for y variable
    cvar=plotdata$degurb_ordered,    # override for colour variable
    soutputdir=if(exists("outputdir")) {outputdir} else {(getwd())} #extended definition
) 
{
  fig0<-ggplot(plotdata, aes(x=xvar,y=yvar * 100, color=cvar))
  fig0+
    geom_line(size=1) +
    xlab("Year") + ylab("Area index (2020 = 100)") + 
    labs(title= paste("Area index", gsub("_", " ", printname)),color="Results", caption=gsub("_", " ", paste(printname, runset))) +
    scale_x_continuous(expand = c(0, 0))+
    scale_colour_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    geom_vline(xintercept=1975, linetype="dashed")+
    theme_light()+fonts_def(font_size)
    
  ggsave(paste(soutputdir, "area_",storename,".png",sep=""), width=16, height=10)
}

#----------------- graph to show population density over time
#
# Chris Jacobs-Crisioni, 3/1/2025
#
# presupposes a long form dataset
# also: 
#   a custom_colour element aligned with the cvar values
#
# -------------------------------------------------------------
pop_density_graph<-function(
  plotdata,               # dataframe (long format)
  printname,              # reporting unit name to be printed in the graph
  storename=printname,    # reporting unit name to be put in the filename (defaults to printname)
  xvar=plotdata$year,     # override for x variable
  yvar=plotdata$pop / plotdata$area,      # override for y variable
  cvar=plotdata$degurb_ordered,    # override for colour variable
  soutputdir=if(exists("outputdir")) {outputdir} else {(getwd())} #extended definition
  ) 
{
  fig1<-ggplot(plotdata, aes(x=xvar,y=yvar, color=cvar))
  fig1+
    geom_line(size=1) +
    xlab("Year") + ylab("Average population density") +
    labs(title= paste("Population density", gsub("_", " ", printname)),color="Results", caption=gsub("_", " ", paste(printname, runset))) +
    scale_colour_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    geom_vline(xintercept=1975, linetype="dashed")+
    scale_x_continuous(expand = c(0, 0))+
    theme_light()+fonts_def(font_size)
  
  ggsave(paste(soutputdir, "popdens_",storename,".png",sep=""), width=16, height=10)
}

#----------------- graph to show population shares over time
#
# Chris Jacobs-Crisioni, 3/1/2025
#
# presupposes a long form dataset
# best results when the xvar values have equal interval, and moved
# left with half their interval value
# also: 
#   a custom_colour element aligned with the cvar values
#
# -------------------------------------------------------------
stacked_pop_graph<-function(
    plotdata,               # dataframe (long format)
    printname,              # reporting unit name to be printed in the graph
    storename=printname,    # reporting unit name to be put in the filename (defaults to printname)
    xvar=plotdata$year,     # override for x variable
    yvar=plotdata$pop,      # override for y variable
    cvar=plotdata$degurb_revordered,    # override for colour variable (reverse order for prettiest results)
    soutputdir=if(exists("outputdir")) {outputdir} else {(getwd())} #extended definition
) {
  fig4<-ggplot(plotdata, aes(x=xvar, y=yvar,  fill=cvar))
  fig4+
    geom_bar(position="fill", stat="identity", width=10) +
    xlab("Year") + ylab("Share of total population") + 
    labs(title= paste("Population shares ", gsub("_", " ", printname)),fill="Results", caption=gsub("_", " ", paste(printname, runset))) +
    scale_colour_manual(values=custom_colour)+
    scale_fill_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    geom_vline(xintercept=1975, linetype="dashed")+
    scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    scale_x_continuous(expand = c(0, 0))+
    theme_light()+fonts_def(font_size)

  ggsave(paste(soutputdir, "popshares_",storename,".png",sep=""), width=16, height=10)
}
#----------------- graph to show population density over time
#
# Chris Jacobs-Crisioni, 3/1/2025
#
# presupposes a long form dataset
# also: 
#   a custom_colour element aligned with the cvar values
#
# -------------------------------------------------------------
pop_density_graph<-function(
    plotdata,               # dataframe (long format)
    printname,              # reporting unit name to be printed in the graph
    storename=printname,    # reporting unit name to be put in the filename (defaults to printname)
    xvar=plotdata$year,     # override for x variable
    yvar=plotdata$pop / plotdata$area,      # override for y variable
    cvar=plotdata$degurb_ordered,    # override for colour variable
    soutputdir=if(exists("outputdir")) {outputdir} else {(getwd())} #extended definition
) 
{
  fig1<-ggplot(plotdata, aes(x=xvar,y=yvar, color=cvar))
  fig1+
    geom_line(size=1) +
    xlab("Year") + ylab("Average population density") +
    labs(title= paste("Population density", gsub("_", " ", printname)),color="Results", caption=gsub("_", " ", paste(printname, runset))) +
    scale_colour_manual(values=custom_colour)+
    geom_vline(xintercept=2020, linetype="dashed")+
    geom_vline(xintercept=1975, linetype="dashed")+
    scale_x_continuous(expand = c(0, 0))+
    theme_light()+fonts_def(font_size)
  
  ggsave(paste(soutputdir, "popdens_",storename,".png",sep=""), width=16, height=10)
}

#----------------- graph to show settlements and size over time
#
# Chris Jacobs-Crisioni, 3/1/2025
#
# presupposes a long form dataset
# also: 
#   a custom_colour element aligned with the cvar values
#
# -------------------------------------------------------------
settlements_graph<-function(
    plotdata,               # dataframe (long format)
    printname,              # reporting unit name to be printed in the graph
    storename=printname,    # reporting unit name to be put in the filename (defaults to printname)
    xvar=plotdata$year,     # override for x variable
    yvar=plotdata$Values,      # override for y variable
    cvar=plotdata$Settlement,    # override for colour variable
    tvar=plotdata$Indicator, #override for linetype
    soutputdir=if(exists("outputdir")) {outputdir} else {(getwd())} #extended definition
) 
{
  fig0<-ggplot(plotdata, aes(x=xvar,y=yvar * 100, color=cvar, linetype=tvar))
  fig0+
    geom_line(size=1) +
    xlab("Year") + ylab("Indexed number (n) and size (km2) (2020 = 100)") + 
    labs(title= paste("Settlements indices", gsub("_", " ", printname)),color="Type", linetype = "Indicator", caption=gsub("_", " ", paste(printname, runset))) +
    scale_x_continuous(expand = c(0, 0))+
    scale_colour_manual(values=custom_colour_settlements)+
    scale_linetype_manual(values=custom_indicator_settlements)+
    geom_vline(xintercept=2020, linetype="dashed")+
    theme_light()+fonts_def(font_size)
  
  ggsave(paste(soutputdir, "settlements_",storename,".png",sep=""), width=16, height=10)
}