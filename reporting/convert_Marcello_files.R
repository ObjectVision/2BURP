# #####################################
# code to convert Marcello's output to the format expected by custom graphing scripts
# Chris Jacobs-Crisioni, 24/2/2025
# #############################################

#############################################################
#####
##### User inputs!!!
#####
########################## the inputs below are subject to user inputs

library(readxl)
setwd("~/Werk/2023_global_lu_model/results/20250331_Marcello")
indata<-read_xlsx("Region_stats_SMOD_V1s7_O7noBU_PopTrend_Summary_L1_SHARE_LD_VALUES.xlsx")
degurba_level<-"DegUrba_lvl1"

##############################################################
# intended structure:
# ru_label	ru_code	cls_label	cls_code	year	area	pop	Builtup
countries<-read.csv("countries_20250206.csv", sep=";", fileEncoding="UTF-8-BOM")

# optional: drop extra columns
indata$UNLocationID<-NULL
indata$SDGIntermediate<-NULL
indata$SDGRegion<-NULL

# squish into right format
indata$year<-indata$Year
indata$Year<-NULL
indata$pop<-indata$POP
indata$POP<-NULL
indata$area<-indata$AREA_km2
indata$AREA_km2<-NULL
indata$Builtup<-indata$BU_km2_total
indata$BU_km2_total<-NULL
indata$BU_km2_used<-NULL

# recode degrees of urbanisation
indata$cls_label<-"Rural grid cells"
indata$cls_code<-10
indata$cls_label[indata$DEGURBA=="UC"]<-"Urban centre"
indata$cls_code [indata$DEGURBA=="UC"]<-30
indata$cls_label[indata$DEGURBA=="DUC"]<-"Dense urban cluster"
indata$cls_code [indata$DEGURBA=="DUC"]<-23
indata$cls_label[indata$DEGURBA=="SDUC"]<-"Semi-dense urban cluster"
indata$cls_code [indata$DEGURBA=="SDUC"]<-22
indata$cls_label[indata$DEGURBA=="SBRB"]<-"Suburban grid cell"
indata$cls_code [indata$DEGURBA=="SBRB"]<-21
indata$cls_label[indata$DEGURBA=="UCL"]<-"Urban cluster"
indata$cls_code [indata$DEGURBA=="UCL"]<-20
indata$cls_label[indata$DEGURBA=="RC"]<-"Rural cluster"
indata$cls_code [indata$DEGURBA=="RC"]<-13
indata$cls_label[indata$DEGURBA=="LDR"]<-"Low density rural grid cell"
indata$cls_code [indata$DEGURBA=="LDR"]<-12
indata$cls_label[indata$DEGURBA=="VLDR"]<-"Very low density grid cell"
indata$cls_code [indata$DEGURBA=="VLDR"]<-11
indata$cls_label[indata$DEGURBA=="WATER"]<-"Water"
indata$cls_code [indata$DEGURBA=="WATER"]<-00

indata<-subset(indata, cls_code>0)

indata$DEGURBA<-NULL

########### add info from countries file
indata<-merge(indata, countries, by="CountryID", all.x = TRUE)

# make data frame to output to countries
out_countries<-aggregate(cbind(pop, area, Builtup) ~ iso3cd + Name + year + cls_code + cls_label, data=indata, FUN=sum)
out_countries$ru_code<-out_countries$iso3cd
out_countries$ru_label<-out_countries$Name
out_countries$iso3cd<-NULL
out_countries$Name<-NULL
write.csv(out_countries, row.names=FALSE, file=paste0("UN_countries_",degurba_level,".csv"))

# make data frame to output to continents
out_continents<-aggregate(cbind(pop, area, Builtup) ~ Continent + year + cls_code + cls_label, data=indata, FUN=sum)
out_continents<-subset(out_continents, Continent!="none")
out_continents$ru_code<-out_continents$Continent
out_continents$ru_label<-out_continents$Continent
out_continents$Continent<-NULL
write.csv(out_continents, row.names=FALSE, file=paste0("Continents_1RUS_uint32_",degurba_level,".csv"))

# make data frame to output to intermediate regions
out_itm_regions<-aggregate(cbind(pop, area, Builtup) ~ UN.intermediate.region + year + cls_code + cls_label, data=indata, FUN=sum)
out_itm_regions$ru_code<-out_itm_regions$UN.intermediate.region
out_itm_regions$ru_label<-out_itm_regions$UN.intermediate.region
out_itm_regions$UN.intermediate.region<-NULL
write.csv(out_itm_regions, row.names=FALSE, file=paste0("UN_intermediate_regions_",degurba_level,".csv"))

