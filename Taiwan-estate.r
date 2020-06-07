setwd("104-4")

ReadFile<-function(i)
{
  filename<-paste(LETTERS[i],"_lvr_land_A.csv",sep="")
  if(file.exists(filename))
  {
    
    x<-read.table(filename,header = T,sep=",",
                  quote = "",
                  row.names = NULL,
                  fill= T,fileEncoding="UTF-8-BOM")
    x<-x[-1,]
    cat(sprintf("read file %s\n",filename))
    return(cbind(x,ID=rep(LETTERS[i],nrow(x))))
  }
  
  else{
    cat(sprintf("file %s doesn't exist.\n",filename))
  }
}

ReadAllFile <- function()
{
  
}

#read estate data and clean up the data
taiwan<-adply(1:26,.margins = 1,.fun = ReadFile)
taiwan <-taiwan[c(30,2,23,24,3,9,16,10,11,12,13,14,17,18,19,20,21,22,25,26,27)]
colnames(taiwan)[c(1,2,4)]<-c("id","name","PricePerSqrtm")
taiwan$PricePerSqrtm<-as.numeric(taiwan$PricePerSqrtm)

#calculate the avg of price/squaremeters
countyplot<-ddply(taiwan,.(id),summarise,y = round(mean(PricePerSqrtm,na.rm = T))/10000)

#1090607 on myPC                               
p<-data.frame(name=taiwanmap$COUNTYNAME,                                                 
              id=taiwanmap$COUNTYID,
              stringsAsFactors = F)

p<-merge(p,countyplot,by= "id")
finalplot2<-merge(taiwanmap.county,p,by="id",all.p=T)

twcmap<-ggplot()+geom_polygon(data = finalplot2,
                              aes(x=long,
                                  y=lat,
                                  group=group,
                                  fill=y),
                              color="black",
                              size=0.25)

      #tablea<-NameIdTable[NameIdTable$idlabel==LETTERS[1],]
      #a<-merge(taiwana,tablea,by = "name")