#Dummy Variable Script

#This script takes an input table that contains species names and creates a dummy variable for the original data table as to whether that species was found in the input table. (This is useful for threats or habitat that a species might be found in). The dummy variable is then appended to the master data file.

setwd("~/Desktop/Tilman Project")
All_Species_Table=read.csv(paste(getwd(),"/All_Species_Table.csv",sep=""),na.strings="")

setwd("~/Desktop/Tilman Project/DV Folder (Threat,Habitat,etc.)") #change the working directory to wherever the file is located

DVmake=function(input){
  DVtable=read.csv(paste(getwd(),input,sep="/"))
  DVname=gsub(".csv","",input)
  All_Species_Table[[DVname]]=as.numeric(All_Species_Table[["IUCN_Species_ID"]]%in%DVtable[["Species.ID"]])
  which(!DVtable%in%All_Species_Table[["IUCN_Species_ID"]])
  return(All_Species_Table)
}

for(i in 1:length(dir())){
  All_Species_Table=DVmake(dir()[i])
}

#use the following code with caution. It will rewrite the old species table.
setwd("~/Desktop/Tilman Project")
#write.csv(All_Species_Table,file="All_Species_Table.csv",row.names=FALSE)


