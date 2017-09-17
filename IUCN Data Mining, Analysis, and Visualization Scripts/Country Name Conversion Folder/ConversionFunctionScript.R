#### Country Conversion Script ####

#To alleviate issues with country name differences between various datasets to concatenate, the following script can be used. Enter a dataset and identify the country name column. Output will be the original dataset with preferred country IDs.

#ISO3	
#ISO2	
#ISO_Numeric	
#FAO_Country	
#All_Species_Table	
#Reference_column	
#FAO_Region	
#IUCN_Region	
#FAO_Waste_Region_Former_Republics_Excluded	
#Continent_Former_Republics_Excluded	
#JMP_Map_Name	
#CIA_Name	
#FAO_Symboless_Name	
#World_Bank_Country	
#Yield_Groupings

setwd("~/Country Name Conversion Folder")

CountryConversionFunction <- read.csv(paste(getwd(),"/CountryConversionFunction.csv",sep=""))

#NOTE DO NOT USE FILES WITH "Conversion" IN THE FILE NAME.

#filename is the name of the file (has to be in the same directory).
#inputconv has to match the name of a column in the country conversion table.
#keep is TRUE or FALSE whether the original column is wanted
convertfunction=function(inputconv,outputconv,keep=TRUE){
  require(plyr)
  if(any(length(which(!grepl("Conversion",dir())))>1,length(which(!grepl("Conversion",dir())))==0)){stop("Incorrect number of unconverted .csv files in Country Conversion Folder. Requires one unconverted .csv file.")}
  convtable=read.csv(paste(getwd(),dir()[which(!grepl("Conversion",dir()))],sep="/"))
  colnames(convtable)[which(colnames(convtable)==inputconv)]="tempxxname"
  joinconversion=data.frame(CountryConversionFunction[[inputconv]],CountryConversionFunction[[outputconv]])
  names(joinconversion)=c("tempxxname",outputconv)
  joined=join(convtable,joinconversion)
  colnames(joined)[which(colnames(joined)=="tempxxname")]=inputconv
  newjoined=subset(joined, select=c(1:which(colnames(joined)==inputconv),which(colnames(joined)==outputconv),(1+which(colnames(joined)==inputconv)):(ncol(joined)-1)))
  if(keep==FALSE){newjoined[[inputconv]]=NULL}
  write.csv(newjoined,file=paste(dir()[which(!grepl("Conversion",dir()))],"_Conversion.csv",sep=""),row.names=FALSE)
} #this function inserts the newly converted country names next to the old column.

names(CountryConversionFunction) #Menu of conversions (use indices for function)

#convertfunction(inputconv="ISO3",outputconv="IUCN_Region",keep=TRUE)
