library(rgeos)                      
library(rgdal)               #install.package("rgdal",type = "source",repos = NULL)
library(maptools)
library(ggplot2)
library(plyr)
library(RColorBrewer)

setwd("downloads/real-estate")

#read shp file
taiwanmap<-readShapeSpatial("twmapdata/TOWN_MOI_1090324.shp")               #readshpfile
taiwanmap.county<-fortify(taiwanmap,region = "COUNTYID")                    #transform shpdf to df
taiwanmap.df<-fortify(taiwanmap,region = "TOWNID")  #transform shpdfto df

#remove the Spratly Islands and Pratas Island
x<-which(taiwanmap.county$lat<=20)
taiwanmap.county<-taiwanmap.county[-x,]
x<-which(taiwanmap.df$lat<=20)
taiwanmap.df<-taiwanmap.df[-x,]

#transform the encoding
taiwanmap$TOWNNAME<- iconv(taiwanmap$TOWNNAME,from = "UTF-8", to ="CP950")  
taiwanmap$COUNTYNAME<- iconv(taiwanmap$COUNTYNAME,from = "UTF-8", to ="CP950")  

#create a df include townname & TownID
NameIdTable <- data.frame(id = taiwanmap$TOWNID,name = taiwanmap$TOWNNAME)  
NameIdTable<-cbind(NameIdTable,
                   idlabel=substr(NameIdTable$id,start = 1,stop= 1))



