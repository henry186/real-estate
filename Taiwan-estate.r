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
  colnames(population)<-c("name","household","population","male","female")
  population[3]<-as.numeric(population$population)
  a<-substr(population$name[2],4,4)
  population[1]<-gsub(a,"",population$name)
  return(population)
}

readIncome <-function()
{
  income<-read_excel("105-107income.xlsx",sheet=1)
  income<- cbind(year = rep(105:107,each=21),income)
  return(income)
}

#read one csv file
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

#read all csvfile in a directorty
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

#read a year of estate data and clean up the data
readyear<-function(year)
{
  taiwan<-adply(1:4,.margins = 1,.fun = ReadAllFile,year=year)
  taiwan <-taiwan[c(2, 3 ,24 ,25 ,18, 4, 10, 17 ,11 , 12, 13 ,14 ,
                  15, 19 ,20 ,21 ,22 ,23)]

  colnames(taiwan)[c(2,4)]<-c("townname","PricePerSqrtm")
  taiwan[3]<- as.numeric(taiwan[[3]])
  taiwan[4]<- as.numeric((taiwan[[4]]))
  taiwan[7]<- as.numeric(taiwan[[7]])+19110000
  taiwan[8]<- as.numeric(taiwan[[8]])+19110000

  taiwan<-taiwan%>%filter(����~���>=20140101)
  taiwan<-taiwan%>%filter(taiwan$PricePerSqrtm!=0)
  taiwan<-taiwan%>%filter(����Ъ�=="�Цa(�g�a+�ت�)"
                              |����Ъ�=="�Цa(�g�a+�ت�)+����"
                              |����Ъ�=="�ت�")
  taiwan<-taiwan%>%filter(�D�n�γ~=="��v"
                              |�D�n�γ~=="��v��"
                              |�D�n�γ~=="���"
                              |�D�n�γ~=="��a��"
                              |�D�n�γ~=="��ӥ�"
                              |�D�n�γ~=="��u��"
                              |�D�n�γ~=="����L�n�O�ƶ�"
                              |�D�n�γ~=="���䥦�n�O�ƶ�"
                              |�D�n�γ~=="���E�B��v"
                              |�D�n�γ~=="�����v"
                              |�D�n�γ~=="���X��v")
  taiwan<-taiwan%>%filter(taiwan$�ت��{�p�槽.��!="0")
  taiwan<-taiwan%>%filter(taiwan$�`�Ӽh��!=""
                          &taiwan$�`�Ӽh��!="(�ť�)"
                          &taiwan$�`�Ӽh��!="000"
                          &taiwan$�`�Ӽh��!="043"
                          &taiwan$�`�Ӽh��!="���ϥΰ���"
                          &taiwan$�`�Ӽh��!="����L�n�O�ƶ�")

#transfirm to class date
#  a<-taiwan$����~���
#  taiwan$����~���<-paste(substr(a,1,4),substr(a,5,6)
#                    ,substr(a,7,8),sep="-")
#  taiwan$����~���<-as.Date(taiwan$����~���)
#
 # a<-taiwan$�ؿv�����~��
  #taiwan$�ؿv�����~��<-paste(substr(a,1,4),substr(a,5,6)
   #                  ,substr(a,7,8),sep="-")
  #taiwan$�ؿv�����~��<-as.Date(taiwan$�ؿv�����~��)

  return(na.omit(taiwan))
}

plotmap<-function(taiwan=taiwan,choosemap,year=109)
{ 
  
  taiwanmap<-readShapeSpatial("twmapdata/TOWN_MOI_1090324.shp")        #readshpfile
  
  taiwanmap.county<-fortify(taiwanmap,region = "COUNTYID")  #transform shpdf to df
  
  
  #transform the encoding
  taiwanmap$COUNTYNAME<- iconv(taiwanmap$COUNTYNAME,from = "UTF-8", to ="CP950")  
  
  #remove the Spratly Islands and Pratas Island
  NameIdTable <- data.frame(id = taiwanmap$COUNTYID,
                            name = taiwanmap$COUNTYNAME,
                            stringsAsFactors = F)  
  NameIdTable<-unique(NameIdTable)  #delete repeat rows
  x<-which(taiwanmap.county$lat<=20)
  taiwanmap.county<-taiwanmap.county[-x,]
  options(scipen = 999)
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
  population<-readPopulation(year)
  population_plot<-right_join(population,NameIdTable,by="name")
  populationPlot<-right_join(population_plot,taiwanmap.county,by="id")
   
    populationMap<-ggplot()+geom_polygon(data = populationPlot,
                                     aes(x=long,
                                         y=lat,
                                         group=group,
                                         fill=population),
                                     color="black",
                                     size=0.25)+
    theme_void()+
    coord_map()+
    labs(title = "county name")
  return(populationMap)
  }
  
}

drawn <- function(y){

priceMean<-ddply(taiwan,.(id),summarize,mean = round(mean(PricePerSqrtm,na.rm = T)))
priceMax<-ddply(taiwan,.(id),summarise,max=max(PricePerSqrtm,na.rm = T))
priceMin<-ddply(taiwan,.(id),summarise,min=min(PricePerSqrtm,na.rm = T))
tradesum<-ddply(taiwan,.(id),summarise,sum=sum(PricePerSqrtm!=0,na.rm = T))
���� <-c("�O�_��","�O����","�򶩥�","�O�n��","������","�s�_��","�y����","��饫","�Ÿq��","�s�˿�","�]�߿�","�n�뿤","���ƿ�","�s�˥�","���L��","�Ÿq��","�̪F��","�Ὤ��","�O�F��","������","���","�s����") 
priceMean <- data.frame("����"=����,priceMean)
priceMax <- data.frame("����"=����,priceMax)
priceMin <- data.frame("����"=����,priceMin)
tradesum <- data.frame("����"=����,tradesum)
houseuse <- data.frame("price"=round(taiwan$PricePerSqrtm,-4),"����Ъ�"=taiwan$����Ъ�)
sqrtm <- round(taiwan$�`���� / taiwan$PricePerSqrtm, -1)
sqpr <- data.frame("id"=taiwan$id, "price"=taiwan$PricePerSqrtm ,"�W��"=sqrtm)
sqpr <- sqpr%>%filter(�W��<8000)
sqpr <- sqpr%>%filter(price<400000)
anprice<- c(priceMean$mean,priceMax$max,priceMin$min)
pricedf<- data.frame("����"=rep(����,times=3),"�л�"=anprice,"ann"=c(rep("Mean",times=22),rep("Max",times=22),rep("Min",times=22)))

meanPricePlot<-ggplot(priceMean,aes(mean,����))+geom_col(position = "dodge")+labs(x="�C���褽�إ�������")
tradeThingPlot<-ggplot(houseuse,aes(price,fill=����Ъ�))+geom_histogram(stat = "count",position = "fill")+labs(x="�C���褽�ػ���",y="����Ъ����")+xlim(0,400000)+scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),labels=c("0%","25%","50%","75%","100%"))
areaPlot<-ggplot(sqpr,aes(�W��,price,fill=id))+geom_point(shape=23)+scale_fill_discrete(name="����",labels=����)
floorPlot<-ggplot(taiwan,aes(id,fill=�`�Ӽh��))+geom_histogram(stat = "count",position = "fill")+labs(x="����",y="�Ӽh�Ƥ��")+scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),labels=c("0%","25%","50%","75%","100%"))
pircePlot<-ggplot(pricedf,aes(����,�л�,fill=ann))+geom_col(position = "dodge")
numbersPlot<-ggplot(tradesum,aes(����,sum))+geom_col()+labs(y="���")

switch(y,
       return(meanPricePlot),
       return(tradeThingPlot),
       return(areaPlot),
       return(floorPlot),
       return(pircePlot),
       return(numbersPlot),
)
}

plotAllMap<-function()
{
  grid.arrange(plotmap(taiwan,"mean"),
               plotmap(taiwan,"max"),
               plotmap(taiwan,"population"),
               nrow = 2)
}

setwd("~/real-estate/108-2")

taiwan <- adply(105:109,.margins = 1,.fun=readyear)[-1]

setwd("..")


