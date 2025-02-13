# ------------------- backcast data aggregator
# 
# Chris Jacobs-Crisioni, 3/1/2025
#
# code to take the backcast data, and turn it in a dataset consistent with the 
# loaded boundaryset
# 
# presupposes: 
#      reportingdir<-"D:/ProjDir/2BURP/reporting/"
#      boundaryset<-"UN_intermediate_regions" #or Countries, UN_countries, UN_intermediate_regions, Continents, Continents_1RUS_uint32, World
# ------------------------------------------------

# create an empty dataframe to which rows can be added if available
out_columns <- subset(indata, ru_label==-9999)

# fill out_columns only if the boundaryset can be aggregated from the countries encoded in 
if(toupper(classification)==toupper("DegUrba_lvl1") & (boundaryset=="Continents_1RUS_uint32" | boundaryset=="UN_intermediate_regions" | boundaryset=="UN_countries" | boundaryset=="World")) {

  rawdata<-read.csv(paste0(reportingdir, "/", "df_smod_est_v7.csv"), stringsAsFactors = FALSE)
  rawdata<-subset(rawdata, year<1975)
  rawdata$pop_WPP2022<-rawdata$pop_WPP2022 * (1000*1000)
  un_regions<-read.csv(paste0(reportingdir, "/","GADM4104GHSL_CNTRY_map_v4 2.csv"), stringsAsFactors = FALSE)
  
  merged<-merge(x=rawdata, y=un_regions, by.x="iso3c", by.y="GID_0GHSL", all.x=TRUE, all.y=FALSE)
  
  # set specifics of merging field
  if(boundaryset=="UN_intermediate_regions") {
    merged$agg_field<-merged$UN.intermediate.group
    merged$name_field<-merged$UN.intermediate.group
  } else if(boundaryset=="UN_countries") {
    merged$agg_field<-merged$iso3c
    merged$name_field<-merged$country
  } else if(boundaryset=="World") {
    merged$agg_field<-"World"
    merged$name_field<-"World"
  } else if(boundaryset=="Continents_1RUS_uint32") {
    merged$agg_field<-merged$Continent
    merged$name_field<-merged$Continent
  }

    
  temp<-aggregate(merged$pop_WPP2022, by=list(merged$agg_field, merged$name_field, merged$smod, merged$year), FUN="sum")
  
  temp$ru_code<-temp$Group.1
  temp$ru_label<-temp$Group.2
  temp$cls_label<-temp$Group.3
  temp$cls_code<-NA
  temp$year<-temp$Group.4
  temp$area<-NA
  temp$pop<-temp$x
  temp$Builtup<-NA
  #temp$abandoned<-NA
  #temp$emerged<-NA
  
  temp$Group.1<-NULL
  temp$Group.2<-NULL
  temp$Group.3<-NULL
  temp$Group.4<-NULL
  temp$x<-NULL

  out_columns<-rbind(out_columns, temp)
  
}