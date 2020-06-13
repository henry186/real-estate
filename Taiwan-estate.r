library(rgeos)                      
library(rgdal)               #install.package("rgdal",type = "source",repos = NULL)
library(maptools)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(gridExtra)
library(dplyr)

setwd("~/cmder/real-estate/real-estate")

#read shp file and use the county region
taiwanmap<-readShapeSpatial("twmapdata/TOWN_MOI_1090324.shp")        #readshpfile
taiwanmap.county<-fortify(taiwanmap,region = "COUNTYID")             #transform shpdf to df
#remove the Spratly Islands and Pratas Island
x<-which(taiwanmap.county$lat<=20)
taiwanmap.county<-taiwanmap.county[-x,]


#taiwanmap.df<-fortify(taiwanmap,region = "TOWNID")  #transform shpdfto df
#remove the Spratly Islands and Pratas Island
#x<-which(taiwanmap.df$lat<=20)
#taiwanmap.df<-taiwanmap.df[-x,]

#transform the encoding
taiwanmap$TOWNNAME<- iconv(taiwanmap$TOWNNAME,from = "UTF-8", to ="CP950")  
taiwanmap$COUNTYNAME<- iconv(taiwanmap$COUNTYNAME,from = "UTF-8", to ="CP950")  

#create a df include townname & TownID
NameIdTable <- data.frame(id = taiwanmap$TOWNID,name = taiwanmap$TOWNNAME)  
NameIdTable<-cbind(NameIdTable,
                   idlabel=substr(NameIdTable$id,start = 1,stop= 1))



setwd("104-4")

ReadFile<-function(i)
{
  filename<-paste(LETTERS[i],"_lvr_land_A.csv",sep="")
  if(file.exists(filename))
  {
    
    x<-read.table(filename,header = T,sep=",",
                  quote = "",
                  row.names = NULL,
                  fill= T,fileEncoding="UTF-8-BOM",
                  stringsAsFactors = F)
    x<-x[-1,]
    cat(sprintf("read file %s\n",filename))
    return(cbind(x,ID=LETTERS[i]))
  }
  
  else{
    cat(sprintf("file %s doesn't exist.\n",filename))
  }
}

ReadAllFile <- function(i,year)
{
  path<-paste("../",year,"-",i,sep = "")
  setwd(path)
  cat("file1",year,"-",i,"\n")
  return(adply(1:26,.margins = 1,.fun = ReadFile))
}

#read estate data and clean up the data
taiwan<-adply(1,.margins = 1,.fun = ReadAllFile,year=103)
taiwan<-taiwan%>%filter(交易標的!="土地"&交易標的!="車位")
taiwan <-taiwan[c(30,2,23,24,3,9,16,10,11,12,13,14,17,18,19,20,21,22,25,26,27)]

colnames(taiwan)[c(1,2,4)]<-c("id","name","PricePerSqrtm")

taiwan[3]<- as.numeric(taiwan[[3]])
taiwan[4]<- as.numeric((taiwan[[4]]))
taiwan[6]<- as.numeric(taiwan[[6]])+19110000
taiwan[7]<- as.numeric(taiwan[[7]])+19110000
taiwan<-taiwan%>%filter(交易標的!="土地"&交易標的!="車位")

#transfirm to class date
a<-taiwan$交易年月日
taiwan$交易年月日<-paste(substr(a,1,4),substr(a,5,6),substr(a,7,8),sep="-")
taiwan[6]<-as.Date(taiwan[[6]])

#calculate the avg of price/squaremeters
priceMean<-ddply(taiwan,.(id),summarize,mean = round(mean(PricePerSqrtm,na.rm = T)))

#calculate the max value of price/square meters
priceMax<-ddply(taiwan,.(id),summarise,max=max(PricePerSqrtm,na.rm = T))

#calculate the min value of price/square meters
priceMin<-ddply(taiwan,.(id),summarise,min=min(PricePerSqrtm,na.rm = T))

#merge the map data with estate data                              
p<-data.frame(name=taiwanmap$COUNTYNAME,                                                 
              id=taiwanmap$COUNTYID,
              stringsAsFactors = F)

#meanplot
mean_plot<-merge(p,priceMean,by= "id")
meanPlot<-merge(taiwanmap.county,mean_plot,by="id",all.mean_plot=T)
priceMeanMap<-ggplot()+geom_polygon(data = meanPlot,
                                    aes(x=long,
                                        y=lat,
                                        group=group,
                                        fill=mean),
                                    color="black",
                                    size=0.25)+
  scale_fill_gradientn(
    colours = brewer.pal(9,"Reds"))+
  theme_void()+
  coord_map()+
  labs(title = "mean price of the real estate")

#maxplot
max_plot<-merge(p,priceMax,by= "id")
maxPlot<-merge(taiwanmap.county,max_plot,by="id",all.max_plot=T)
priceMaxMap<-ggplot()+geom_polygon(data = maxPlot,
                                    aes(x=long,
                                        y=lat,
                                        group=group,
                                        fill=max),
                                    color="black",
                                    size=0.25)+
  scale_fill_gradientn(
    colours = brewer.pal(9,"Reds"))+
  theme_void()+
  coord_map()+
  labs(title = "max price of the real estate")
#minplot
min_plot<-merge(p,priceMin,by= "id")
minPlot<-merge(taiwanmap.county,min_plot,by="id",all.min_plot=T)
priceMinMap<-ggplot()+geom_polygon(data = minPlot,
                                   aes(x=long,
                                       y=lat,
                                       group=group,
                                       fill=min),
                                   color="black",
                                   size=0.25)+
  scale_fill_gradientn(
    colours = brewer.pal(9,"Reds"))+
  theme_void()+
  coord_map()+
  labs(title = "min price of the real estate")

#plot the map of the max,min,mean of the  estate price
#grid.arrange(priceMaxMap,priceMinMap,priceMeanMap,nrow = 2)
