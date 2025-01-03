################### specifics of run
inputdir<-"E:/LocalData/2BURP/Indicators/World"
reportingdir<-"D:/ProjDir/2BURP/reporting/"
boundaryset<-"UN_intermediate_regions" #or Countries, UN_countries, UN_intermediate_regions, Continents, World
classification<-"DegUrba_lvl2"
runset<-"_v8_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_calib_20241205" # new calibration, preferred spec?

################### set outputfolder (stepwise!!!)
rootoutputdir<-"E:/LocalData/2BURP/graphs"
outputdir<-rootoutputdir
#### make folder structure

if (!dir.exists(outputdir)) dir.create(outputdir)
outputdir<-paste0(outputdir,"/",runset)
if (!dir.exists(outputdir)) dir.create(outputdir)
outputdir<-paste0(outputdir,"/",boundaryset)
if (!dir.exists(outputdir)) dir.create(outputdir)
outputdir<-paste0(outputdir,"/",classification,"/")
if (!dir.exists(outputdir)) dir.create(outputdir)

source(paste0(reportingdir, "supporting_scripts.r"))
source(paste0(reportingdir, "graph_functions.r"))
source(paste0(reportingdir, "treat_backcasting.r"))

################### load datafiles
indata<-read.csv(paste0(inputdir,"/","Indicators_",boundaryset,"_",classification,runset,".csv"))
poponlydata<-rbind(indata, out_columns)

# loop over all unique geographies
codeslist<-unique(indata$ru_code)
for (incode in codeslist) {
  
  # internal output to track progress
  print(incode)
  
  #### prep total pop and popdensity graphs by putting them into long form (required by ggplot)
  plotdata<-subset(indata, ru_code==incode)
  name<-plotdata$ru_label[[1]]
  
  # in intermediate regions, 
  # set name so that it matches the original UN names (assumes that numerics < 10 are padded with one zero)
  if (boundaryset=="UN_intermediate_regions") {name=substring(name, 4)}
  
  popplotdata<-subset(poponlydata, ru_label==name | ru_code==incode)
    
  pop_density_graph(plotdata, printname = name, storename = incode, cvar = factor(plotdata$cls_label, level=order))
  
  absolute_pop_graph(popplotdata, printname = name, storename = incode, cvar = factor(popplotdata$cls_label, level=order))

  stackdata<-subset(popplotdata, year %% 10 == 0)
  stacked_pop_graph(stackdata, printname = name, storename = incode, xvar = stackdata$year - 5, cvar = factor(stackdata$cls_label, level=rev_order))
  
}









