library(rgeos)                      
library(rgdal)               #install.package("rgdal",type = "source",repos = NULL)
library(maptools)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(readxl)

readPopulation<- function(year)
{
  path<-paste("population/",year,".xls",sep = "")
  population<-read_xls(path,sheet = 1)[-2:-1,]
  colnames(population)<-c("name","?ˆ¶?•¸","population","male","female")
  population[3]<-as.numeric(population$population)
  a<-substr(population$name[2],4,4)
  population[1]<-gsub(a,"",population$name)
  return(population)
}
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
    return(cbind(id=LETTERS[i],x))
  }
  
}

ReadAllFile <- function(i,year)
{
  path<-paste("../",year,"-",i,sep = "")
  if(file.exists(path))
  {
  setwd(path)
  cat("file1",year,"-",i,"\n")
  return(adply(1:26,.margins = 1,.fun = ReadFile))
  }
  else
    cat(year,"-",i,"doesn't exsist\n")
    
}

#read estate data and clean up the data
readyear<-function(year)
{
taiwan<-adply(1:4,.margins = 1,.fun = ReadAllFile,year=year)

taiwan <-taiwan[c(2 , 3 , 24 , 25 , 4 , 10 , 17 , 11 , 12 , 13 , 14 ,
                  15 , 18 , 19 , 20 , 21 , 22 , 23 , 26 , 27 , 28)]

colnames(taiwan)[c(2,4)]<-c("name","PricePerSqrtm")

taiwan[3]<- as.numeric(taiwan[[3]])
taiwan[4]<- as.numeric((taiwan[[4]]))
taiwan[6]<- as.numeric(taiwan[[6]])+19110000
taiwan[7]<- as.numeric(taiwan[[7]])+19110000
taiwan[13]<-as.numeric(taiwan[[13]])


taiwan<-taiwan%>%filter(¥æ©ö¼Ðªº=="©Ð¦a(¤g¦a+«Øª«)"
                            |¥æ©ö¼Ðªº=="©Ð¦a(¤g¦a+«Øª«)+¨®¦ì"
                            |¥æ©ö¼Ðªº=="«Øª«")
taiwan<-taiwan%>%filter(¥æ©ö¦~¤ë¤é>=20140101)
taiwan<-taiwan%>%filter(¥D­n¥Î³~=="¦í¦v"
                            |¥D­n¥Î³~=="¦í¦v¥Î"
                            |¥D­n¥Î³~=="¦í©Ð"
                            |¥D­n¥Î³~=="¦í®a¥Î"
                            |¥D­n¥Î³~=="¦í°Ó¥Î"
                            |¥D­n¥Î³~=="¦í¤u¥Î"
                            |¥D­n¥Î³~=="¨£¨ä¥Lµn°O¨Æ¶µ"
                            |¥D­n¥Î³~=="¨£¨ä¥¦µn°O¨Æ¶µ"
                            |¥D­n¥Î³~=="©±çE¡B¦í¦v"
                            |¥D­n¥Î³~=="°ê¥Á¦í¦v"
                            |¥D­n¥Î³~=="¶°¦X¦í¦v")
taiwan<-taiwan%>%filter(taiwan$«Øª«²{ªp®æ§½.©Ð!="0")
taiwan<-taiwan%>%filter(taiwan$PricePerSqrtm!=0)
taiwan<-taiwan%>%filter(taiwan$Á`¼Ó¼h¼Æ!=""
                        &taiwan$Á`¼Ó¼h¼Æ!="(ªÅ¥Õ)"
                        &taiwan$Á`¼Ó¼h¼Æ!="000"
                        &taiwan$Á`¼Ó¼h¼Æ!="043"
                        &taiwan$Á`¼Ó¼h¼Æ!="¨£¨Ï¥Î°õ·Ó"
                        &taiwan$Á`¼Ó¼h¼Æ!="¨£¨ä¥Lµn°O¨Æ¶µ")
                       

#transfirm to class date
a<-taiwan$¥æ©ö¦~¤ë¤é
taiwan$¥æ©ö¦~¤ë¤é<-paste(substr(a,1,4),substr(a,5,6)
                    ,substr(a,7,8),sep="-")
taiwan[6]<-as.Date(taiwan[[6]])

return(na.omit(taiwan))
}

plotmap<-function(taiwan=taiwan,taiwanmap=taiwanmap,choosemap,year=106)
{ 
  
  taiwanmap.county<-fortify(taiwanmap,region = "COUNTYID")  #transform shpdf to df
  #remove the Spratly Islands and Pratas Island
  NameIdTable <- data.frame(id = taiwanmap$COUNTYID,
                            name = taiwanmap$COUNTYNAME,
                            stringsAsFactors = F)  
  NameIdTable<-unique(NameIdTable)  #delete 
  x<-which(taiwanmap.county$lat<=20)
  taiwanmap.county<-taiwanmap.county[-x,]
  
  if(choosemap=="mean")
  {
  #meanplot
  #calculate the avg of price/squaremeters
  priceMean<-ddply(taiwan,.(id),summarize,mean = round(mean(PricePerSqrtm,na.rm = T)))
  mean_plot<-right_join(priceMean,NameIdTable,by= "id")
  meanPlot<-inner_join(taiwanmap.county,mean_plot,by="id")
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
  return(priceMeanMap)
  }
  else if(choosemap=="max")
  {
  #maxplot
  #calculate the max value of price/square meters
  
  priceMax<-ddply(taiwan,.(id),summarise,max=max(PricePerSqrtm,na.rm = T))
  max_plot<-left_join(NameIdTable,priceMax,by= "id")
  
  maxPlot<-inner_join(taiwanmap.county,max_plot,by="id")
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
  return(priceMaxMap)
  }
  else if(choosemap=="min")
  {
  #minplot
  #calculate the min value of price/square meters
  priceMin<-ddply(taiwan,.(id),summarise,min=min(PricePerSqrtm,na.rm = T))
  
  min_plot<-left_join(NameIdTable,priceMin,by= "id")
  minPlot<-inner_join(taiwanmap.county,min_plot,by="id")
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
  return(priceMinMap)
  }
  #population map
  else if (choosemap=="population")
  {
  population<-readPopulation(107)
  population_plot<-right_join(population,NameIdTable,by="name")
  populationPlot<-right_join(population_plot,taiwanmap.county,by="id")
  options(scipen = 999) 
    populationMap<-ggplot()+geom_polygon(data = populationPlot,
                                     aes(x=long,
                                         y=lat,
                                         group=group,
                                         fill=population),
                                     color="black",
                                     size=0.25)+
    scale_fill_gradientn(
      colours = brewer.pal(9,"Reds"))+
    theme_void()+
    coord_map()+
    labs(title = "population of taiwan county")
  return(populationMap)
  }
  
  
}

drawan <- function(){

priceMean<-ddply(taiwan,.(id),summarize,mean = round(mean(PricePerSqrtm,na.rm = T)))
priceMax<-ddply(taiwan,.(id),summarise,max=max(PricePerSqrtm,na.rm = T))
priceMin<-ddply(taiwan,.(id),summarise,min=min(PricePerSqrtm,na.rm = T))
tradesum<-ddply(taiwan,.(id),summarise,sum=sum(PricePerSqrtm!=0,na.rm = T))
¿¤¥« <-c("»O¥_¥«","»O¤¤¥«","°ò¶©¥«","»O«n¥«","°ª¶¯¥«","·s¥_¥«","©yÄõ¿¤","®ç¶é¥«","¹Å¸q¥«","·s¦Ë¿¤","­]®ß¿¤","«n§ë¿¤","¹ü¤Æ¿¤","·s¦Ë¥«","¶³ªL¿¤","¹Å¸q¿¤","«ÌªF¿¤","ªá½¬¿¤","»OªF¿¤","ª÷ªù¿¤","¼ê´ò¿¤","³s¦¿¿¤") 
priceMean <- data.frame("¿¤¥«"=¿¤¥«,priceMean)
priceMax <- data.frame("¿¤¥«"=¿¤¥«,priceMax)
priceMin <- data.frame("¿¤¥«"=¿¤¥«,priceMin)
tradesum <- data.frame("¿¤¥«"=¿¤¥«,tradesum)
houseuse <- data.frame("price"=round(taiwan$PricePerSqrtm,-4),"¥æ©ö¼Ðªº"=taiwan$¥æ©ö¼Ðªº)
sqrtm <- round(taiwan$Á`»ù¤¸ / taiwan$PricePerSqrtm, -1)
sqpr <- data.frame("id"=taiwan$id, "price"=taiwan$PricePerSqrtm ,"©W¼Æ"=sqrtm)
sqpr <- sqpr%>%filter(©W¼Æ<8000)
sqpr <- sqpr%>%filter(price<400000)
anprice<- c(priceMean$mean,priceMax$max,priceMin$min)
pricedf<- data.frame("¿¤¥«"=rep(¿¤¥«,times=3),"©Ð»ù"=anprice,"ann"=c(rep("Mean",times=22),rep("Max",times=22),rep("Min",times=22)))

ggplot(priceMean,aes(mean,¿¤¥«))+geom_col(position = "dodge")+labs(x="¨C¥­¤è¤½¤Ø¥­§¡»ù®æ")
ggplot(houseuse,aes(price,fill=¥æ©ö¼Ðªº))+geom_histogram(stat = "count",position = "fill")+labs(x="¨C¥­¤è¤½¤Ø»ù®æ",y="¥æ©ö¼Ðªº¤ñ¨Ò")+xlim(0,400000)+scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),labels=c("0%","25%","50%","75%","100%"))
ggplot(sqpr,aes(©W¼Æ,price,fill=id))+geom_point(shape=23)+scale_fill_discrete(name="¿¤¥«",labels=¿¤¥«)
ggplot(taiwan,aes(id,fill=Á`¼Ó¼h¼Æ))+geom_histogram(stat = "count",position = "fill")+labs(x="¿¤¥«",y="¼Ó¼h¼Æ¤ñ¨Ò")+scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),labels=c("0%","25%","50%","75%","100%"))
ggplot(pricedf,aes(¿¤¥«,©Ð»ù,fill=ann))+geom_col(position = "dodge")
ggplot(tradesum,aes(¿¤¥«,sum))+geom_col()+labs(y="¥ó¼Æ")

}

setwd("C:/Users/user/Documents/real-estate/108-2")

taiwan <- adply(105:109,.margins = 1,.fun=readyear)[-1]

setwd("..")
taiwanmap<-readShapeSpatial("twmapdata/TOWN_MOI_1090324.shp")        #readshpfile
#transform the encoding
# taiwanmap$TOWNNAME<- iconv(taiwanmap$TOWNNAME,from = "UTF-8", to ="CP950")  
# taiwanmap$COUNTYNAME<- iconv(taiwanmap$COUNTYNAME,from = "UTF-8", to ="CP950")  
#create a df include countyid & county name

#table(substr(taiwan$¥æ©ö¦~¤ë¤é,1,4))
#plot the three maps
#grid.arrange(plotmap(taiwan,"mean"),
#             plotmap(taiwan,"min"),
#             plotmap(taiwan,"max"),
#             nrow = 2)
