library(plyr)

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
myfun<-function()
{
  
}
taiwan<-adply(1:26,.margins = 1,.fun = ReadFile)
taiwan <-taiwan[c(30,2,23,24,3,9,16,10,11,12,13,14,17,18,19,20,21,22,25,26,27)]
colnames(taiwan)<-c(colnames(taiwan[1]),"name",colnames(taiwan[-2:-1]))
taiwana<-taiwan[taiwan$ID==LETTERS[1],]
tablea<-NameIdTable[NameIdTable$idlabel==LETTERS[1],]
a<-merge(taiwana,tablea,by = "name")