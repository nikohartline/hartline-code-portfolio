#In the IUCN dataset, each species recorded on their website has "subcriteria" that provide information about why a species has the listing status that it does. For example if a species is found to fulfill the criteria: "Population size estimated to number fewer than 50 mature individuals." then it has a subcriteria "D" explaining why the species is listed as Critically Endangered. To read more on subcriteria, visit: http://www.iucnredlist.org/static/categories_criteria_3_1

#This code extracts data on which subcriteria are fulfilled by each species to quantify what primary threats endangered species face. 

All_Species_Table <- read.csv("~/DATA_TABLES/All_Species_Table.csv")

GROUPS=c("A1a",
       "A1b",
       "A1c",
       "A1d",
       "A1e",
       "A2a",
       "A2b",
       "A2c",
       "A2d",
       "A2e",
       "A3b",
       "A3c",
       "A3d",
       "A3e",
       "A4a",
       "A4b",
       "A4c",
       "A4d",
       "A4e",
       "B1a",
       "B1bi",
       "B1bii",
       "B1biii",
       "B1biv",
       "B1bv",
       "B1ci",
       "B1cii",
       "B1ciii",
       "B1civ",
       "B2a",
       "B2bi",
       "B2bii",
       "B2biii",
       "B2biv",
       "B2bv",
       "B2ci",
       "B2cii",
       "B2ciii",
       "B2civ",
       "C1",
       "C2ai",
       "C2aii",
       "C2b",
       "D",
       "D1",
       "D2",
       "E",
       "C2a",
       "B1",
       "B2a",
       "B2b",
       "B2c",
       "B2d",
       "B2e",
       "B3a",
       "B3b",
       "B3c",
       "B3d")
#C2a and onwards are from older versions of IUCN, not present in the current dataset.

#Create a ready-for-input table out of the All_Species_Table
DVtable=as.data.frame(matrix(nrow=nrow(All_Species_Table),ncol=length(GROUPS)+3))
DVtable[,1]=All_Species_Table[["IUCN_Species_ID"]]
DVtable[,2]=All_Species_Table[["Global_Status"]]
DVtable[,3]=All_Species_Table[["Subcriteria"]]
names(DVtable)=c("IUCN_Species_ID","Global_Status","Subcriteria",GROUPS)

#correcting bugs in IUCN data:
DVtable[18433,3]="A2c+3c" 
DVtable[20689,3]="A2c+3c+4c" 
DVtable[20695,3]="A2c+3c+4c"
DVtable[21115,3]="A2c+3c+4c"
DVtable[31855,3]="B1ab(i,ii,iii,iv,v)"

library(stringr)

for (h in 1:nrow(DVtable)){
  if(DVtable[["Subcriteria"]][h]==""){next}
  step1=str_extract_all(DVtable[["Subcriteria"]][h],"([A-Z])|([^; \n]+)")
  #Alpha=step1[[1]][grepl("([A-Z])",step1[[1]])]
  #step2=step1[[1]][!grepl("([A-Z])",step1[[1]])]
  c1=which(grepl("([A-Z])",step1[[1]]))+1
  splitcriteria=list()
  for(i in 1:length(c1)){
    step2=str_extract_all(step1[[1]][c1[i]],"([0-9])|([^\\+;\n]+)")
    if(any(step2[[1]][1]==c("A","B","C","D","E")|is.na(step2[[1]][1]))){
      splitcriteria=c(splitcriteria,step1[[1]][c1[i]-1])
      next
    }
    c2=which(grepl("([0-9])",step2[[1]]))+1
    for(j in 1:length(c2)){
      step3=str_extract_all(step2[[1]][c2[j]],"([a-z])|\\(([^\\)]*)\\)?")
      if(any(step3[[1]][1]==c("1","2","3","4","5")|is.na(step3[[1]][1]))){
        splitcriteria=c(splitcriteria,paste(step1[[1]][c1[i]-1],step2[[1]][c2[j]-1],sep=""))
        next
      }
      c3=which(grepl("([a-g])",step3[[1]]))+1
      for(k in 1:length(c3)){
        step4=str_extract_all(step3[[1]][c3[k]],"([a-e])|(iv|v?i{0,3})")
        step4=step4[[1]][step4[[1]]!=""]
        if(any(step4[1]==c("a","b","c","d","e")|is.na(step4[1]))){
          splitcriteria=c(splitcriteria,paste(step1[[1]][c1[i]-1],step2[[1]][c2[j]-1],step3[[1]][c3[k]-1],sep=""))
          next
        }
        splitcriteria=c(splitcriteria,paste(step1[[1]][c1[i]-1],step2[[1]][c2[j]-1],step3[[1]][c3[k]-1],step4,sep=""))
      }
    }
  }
  logicalvec=as.numeric(names(DVtable)[4:ncol(DVtable)] %in% splitcriteria)
  if(any(!splitcriteria %in% names(DVtable)[4:ncol(DVtable)])){stop(paste(unlist(splitcriteria[splitcriteria %in% names(DVtable)[4:ncol(DVtable)]]),collapse=' '))}
  DVtable[h,4:ncol(DVtable)]=logicalvec
}

#example output of program is saved as "Subcriteria Data Table.csv" in folder DATA_TABLES

