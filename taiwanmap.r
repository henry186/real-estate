library(rgeos)                      
library(rgdal)               #install.package("rgdal",type = "source",repos = NULL)
library(maptools)
library(ggplot2)

plotmap <-function()
{
taiwanmap<-readShapeSpatial("twmapdata/TOWN_MOI_1090324.shp")               #readshpfile
taiwanmap.df<-fortify(taiwanmap,region = "TOWNID")                          #transform shpdf to df

taiwanmap$TOWNNAME<- iconv(taiwanmap$TOWNNAME,from = "UTF-8", to ="CP950")  #transform the encoding

NameIdTable <- data.frame(id = taiwanmap$TOWNID,name = taiwanmap$TOWNNAME)  #create a df include Townname & TownID
NameIdTable<-cbind(NameIdTable,
                   idlabel=substr(NameIdTable$ID,start = 1,stop= 1))

####
y<-rnorm(length(taiwanmap$TOWNID))                                        #create a dataframe with random
x<-data.frame(name=taiwanmap$TOWNNAME,                                    #norm              
              id=taiwanmap$TOWNID,                  
              y,
              stringsAsFactors = F)
####

finalplot<-merge(taiwanmap.df,x,by="id",all.x=T)

head(finalplot)

twcmap<-ggplot()+geom_polygon(data=finalplot,
                               aes(x=long,y=lat,
                                     group=group,
                                     fill=y),
                             color="black",
                             size=0.25
                             )
                     
#twcmap+coord_map()
}
