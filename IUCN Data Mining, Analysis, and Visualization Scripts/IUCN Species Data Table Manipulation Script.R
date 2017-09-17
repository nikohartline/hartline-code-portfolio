setwd("~/Desktop/Tilman Project/") #Change the working directory to be the All Species Combined Folder.

# ##### These pieces of code are for generating the latest aggregates
# for(i in 312:318){
#   currentSS=All_Species_Table[All_Species_Table[[names(All_Species_Table)[i]]]==1,]
#   TableOutput=EndangeredSpeciesCountFunction(input=currentSS,inputname=names(All_Species_Table)[i])
#   setwd("~/Desktop/Tilman Project/Outputs of Database Script/2016Outputs")
#   write.csv(TableOutput,file=paste(names(All_Species_Table)[i],"Country_Aggregates.csv",sep="_"))
#   setwd("~/Desktop/Tilman Project/")
# }

# singleSS=All_Species_Table[All_Species_Table[["Is_Singleton"]]==0,]
# TableOutput=EndangeredSpeciesCountFunction(input=singleSS,inputname="NonSingleton_Diversity")
# setwd("~/Desktop/Tilman Project/Outputs of Database Script/2016Outputs")
# write.csv(TableOutput,file="NonSingleton_Diversity_Country_Aggregates.csv")
# setwd("~/Desktop/Tilman Project/")
# ######

library(dplyr)

All_Species_Table=read.csv(paste(getwd(),"/All_Species_Table.csv",sep=""),na.strings="NA")

EndangeredSpeciesCountFunction=function(input=All_Species_Table,inputname="Diversity"){
  
  CountryConversionFunction=read.csv(paste(getwd(),"/Country Conversion Folder/CountryConversionFunction.csv",sep=""),na.strings="") 
  #Make sure the directory is correctly set to the data mined csv location.
  
  CI1=which(colnames(input)=="Afghanistan")
  CI2=which(colnames(input)=="landIslands")
  #Column indices of the first and last country to prime for loop.

  input[CI1:ncol(input)][is.na(input[CI1:ncol(input)])]=0
  input=input[(input[["Global_Status"]]!="EX")
              &(input[["Global_Status"]]!="EW")
              &(input[["System"]]!="Marine")
              &(input[["System"]]!="Freshwater")
              &(input[["System"]]!="Freshwater; Marine")
              &(input[["System"]]!="Terrestrial; Marine"),] 
  #Removing all instances of extinct, extinct in the wild, freshwater, freshwater marine, and terrestrial marine species.
  
  input[["Global_Status"]][input[["Global_Status"]]=="LR/cd"|input[["Global_Status"]]=="LR/lc"]="LC"
  input[["Global_Status"]][input[["Global_Status"]]=="LR/nt"]="NT"
  #Extraneous classifications aggregated (e.g. all instances of Low Risk/Near Threatened converted to Near Threatened)
  
  input[["Global_Status"]]=factor(input[["Global_Status"]])
  input[["System"]]=factor(input[["System"]])
  extant=input
  #Turns the dataset into dummy variables that indicate whether a species is extant inside of a country
  extant[CI1:CI2][extant[CI1:CI2]==1|extant[CI1:CI2]==2|extant[CI1:CI2]==6]=1
  extant[CI1:CI2][extant[CI1:CI2]==3|extant[CI1:CI2]==4|extant[CI1:CI2]==5|extant[CI1:CI2]==7]=0
  
  Status=c("CR","EN","VU","CR_EN_VU","NT","LC","DD","Total")
  Taxonomy=c("AMPHIBIA","AVES","MAMMALIA","REPTILIA","CHORDATA","PLANTAE","TOTAL")

  output=data.frame(names(All_Species_Table)[CI1:CI2])
  colnames(output)="All_Species_Table"
  for(i in 1:length(Status)){
    for(j in 1:length(Taxonomy)){
      indxS=grepl(gsub("_","|",Status[i]),extant[["Global_Status"]])
      if(Status[i]=="Total"){indxS=grepl(paste(Status,collapse="|"),extant[["Global_Status"]])}
      indxT=grepl(Taxonomy[j],extant[["ClassificationForAggregation"]])
      if(Taxonomy[j]=="CHORDATA"){indxT=grepl(Taxonomy[j],extant[["Phylum"]])}
      if(Taxonomy[j]=="TOTAL"){indxT=grepl(paste(Taxonomy,collapse="|"),extant[["ClassificationForAggregation"]])}
      SS=extant[which(indxS&indxT),]
      output[[paste(Status[i],Taxonomy[j],inputname,sep="_")]]=colSums(SS[CI1:CI2],na.rm=TRUE)
    }
  }
  
  Original=names(output)[2:ncol(output)]
  for(i in 1:length(Original)){
    output[[paste(Original[i],"_+1",sep="")]]=output[[Original[i]]]+1
  }
  
  CountryJoin=CountryConversionFunction[which(CountryConversionFunction[["All_Species_Table"]] %in% output[["All_Species_Table"]]),][c(which(colnames(CountryConversionFunction)=="ISO3"),which(colnames(CountryConversionFunction)=="All_Species_Table"), which(colnames(CountryConversionFunction)=="FAO_Region"),which(colnames(CountryConversionFunction)=="FAO_Country"))]
  
  FinalOutput=left_join(CountryJoin,output)
  
  return(FinalOutput)
  
}
