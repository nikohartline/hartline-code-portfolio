# Statistical Analyses Script

library(dplyr)

#Load in original datatable and apply calculations to obtain the sample mass for statistical analyses and visualization.
washtrialdata <- read.csv("washtrialdata.csv")
CalibrationTable=read.csv("CalibrationTable.csv")
#Masses are in grams, areas are in square centimeters
CalibrationTable[["MASS/AREA"]]=CalibrationTable[["MASS"]]/CalibrationTable[["AREA"]]
NewCalTable=data.frame(rbind("20","333"),rbind(mean(CalibrationTable[1:2,5]),mean(CalibrationTable[3:4,5])))
names(NewCalTable)=c("filtersize","MASS/AREA")
NewCalTable[["filtersize"]]=as.factor(NewCalTable[["filtersize"]])
washtrialdata$filtersize=as.factor(washtrialdata$filtersize)
washtrialdata=left_join(washtrialdata,NewCalTable)

washtrialdata$fibermass=washtrialdata$avgfiltermass-(washtrialdata$'MASS/AREA'*washtrialdata$avgarea) #calculation of the fiber mass for each trial

washtrialdata$scaledfibermass=washtrialdata$fibermass*(36/5)
washtrialdata$scaledfibermass[washtrialdata$load=="TOP"]=washtrialdata$scaledfibermass[washtrialdata$load=="TOP"]*(136/36)
#Calculates the total fiber mass shed on each filter for each trial (36 liters for front-load trials, 136 liters for top-load trials)

levels(washtrialdata$type)[2]="GB"
washtrialdata$type=factor(washtrialdata$type,levels=c("NR","ST","R2","BS","GB"))

washtrialdata[c(25,27)][washtrialdata[c(25,27)]<0]=0 #Set negative measurement values to zero for mean-based tests (attributed to error from contamination, humidity, and variability in area measurements).

modifiedtable=washtrialdata[washtrialdata$filtersize=="20",]
modifiedtable$scaledfibermass333=washtrialdata$scaledfibermass[washtrialdata$filtersize=="333"]
names(modifiedtable)[27]="scaledfibermass20" #creates a table with filter size as new columns (as opposed to factor levels)
modifiedtable[c(27,28)][modifiedtable[c(27,28)]<0]=0
modifiedtable$scaledfibermasscomb=modifiedtable$scaledfibermass333+modifiedtable$scaledfibermass20
modifiedtable$num_type_age_load=paste(modifiedtable$trialnum,modifiedtable$type,modifiedtable$age,modifiedtable$load,sep="_")

TestingTable=modifiedtable[,c(30,2,3,6,7,27,28,29)]

#### Pairwise Statistical Tests ####

median.test <- function(x, y){
  z <- c(x, y)
  g <- rep(1:2, c(length(x), length(y)))
  m <- median(z)
  fisher.test(z < m, g)$p.value
} #Mood's median test function (used below)

library(moments)
STcomp <- read.csv("STcomp.csv") #loads in the treatment combinations of interest for two-sample statistical tests.
TestOutput=data.frame(matrix(data=NA,nrow=nrow(STcomp),ncol=4))
StatsTests=list()
for(i in 1:nrow(STcomp)){
  
  PAIR=STcomp[["filtersize"]][i]!=STcomp[["filtersize2"]][i]|STcomp[["age"]][i]!=STcomp[["age2"]][i]
  
  idA1=grepl(STcomp$age[i],paste(TestingTable$age,"All"),fixed=FALSE)
  idA2=grepl(STcomp$age2[i],paste(TestingTable$age,"All"),fixed=FALSE)
  
  idL1=grepl(STcomp$load[i],paste(TestingTable$load,"All"),fixed=FALSE)
  idL2=grepl(STcomp$load2[i],paste(TestingTable$load,"All"),fixed=FALSE)
  
  idT1=grepl(STcomp$type[i],paste(TestingTable$type,"All"),fixed=FALSE)
  idT2=grepl(STcomp$type2[i],paste(TestingTable$type,"All"),fixed=FALSE)
  
  idS1=grep(STcomp$filtersize[i],names(TestingTable),fixed=FALSE)
  idS2=grep(STcomp$filtersize2[i],names(TestingTable),fixed=FALSE)
  
  INP1=unlist(TestingTable[idA1&idL1&idT1,idS1])
  INP2=unlist(TestingTable[idA2&idL2&idT2,idS2]) #Using the indices generated above (based on the input STcompORIG csv file with variable selections), selects values to be statistically analyzed.
  
  idNA1=is.na(INP1)
  idNA2=is.na(INP2)
  
  if(PAIR){INP1=INP1[!(idNA1|idNA2)]
           INP2=INP2[!(idNA1|idNA2)]}
  else{INP1=INP1[!idNA1]
  INP2=INP2[!idNA2]}
  
  StatsTests[[i]]=wilcox.test(INP1,INP2,paired=PAIR)
  TestOutput[i,1]=wilcox.test(INP1,INP2,paired=PAIR)[[3]] #Mann Whitney U or Wilcoxon Signed Rank test
  TestOutput[i,2]=median(INP1)
  TestOutput[i,3]=median(INP2)
  TestOutput[i,4]=mean(pmax(INP1,0))
  TestOutput[i,5]=mean(pmax(INP2,0))
  TestOutput[i,6]=qnorm(TestOutput[i,1],mean=0,sd=1,lower.tail=TRUE) #z score associated with MWU or WSR (above)
  TestOutput[i,7]=length(INP1)  #  \
  TestOutput[i,8]=length(INP2)  #   |
  TestOutput[i,9]=skewness(INP1)#   |
  TestOutput[i,10]=skewness(INP2) #  >-- Checks for normality  <_____________          
  TestOutput[i,11]=kurtosis(INP1) # |                                        \
  TestOutput[i,12]=kurtosis(INP2)# /                                          \
  TestOutput[i,13]=if(TestOutput[i,7]>2){shapiro.test(INP1)$p.value}else{NA} # \
  TestOutput[i,14]=if(TestOutput[i,8]>2){shapiro.test(INP2)$p.value}else{NA} # /
#   TestOutput[i,13]=NA
#   TestOutput[i,14]=NA
  TestOutput[i,15]=ks.test(INP1,INP2,paired=PAIR)[[2]] #Two-sample Kolmogorov-Smirnov test (equality of distribution of the two groups)- unused
  if(PAIR==T){TestOutput[i,16]=binom.test(sum(INP1>INP2),length(INP2))[[3]]}
  else{TestOutput[i,16]=median.test(INP1,INP2)} #Mood's median test for unpaired OR binomial/exact test for paired - used as a check
  TestOutput[i,17]=tryCatch(t.test(INP1,INP2,paired=PAIR)[[3]],error=function(e) NA)
  TestOutput[i,18]=tryCatch(t.test(INP1,INP2,paired=PAIR)[[1]],error=function(e) NA) #Student's t-test (for parametric analysis)- unused
  TestOutput[i,19]=tryCatch(t.test(INP1,INP2,paired=PAIR)[[2]],error=function(e) NA)
  TestOutput[i,20]=PAIR
  TestOutput[i,21]=sd(INP1)
  TestOutput[i,22]=sd(INP2)
  TestOutput[i,23]=if(any(i==c(10,11,20,21,3))){1}else{""}
  names(StatsTests)[i]=gsub("All_|_All","",paste(paste(as.character(unlist(STcomp[i,c(1:4)])),collapse="_")," vs ",paste(as.character(unlist(STcomp[i,c(5:8)])),collapse="_")))
  row.names(TestOutput)[i]=names(StatsTests)[i]
}
colnames(TestOutput)=c("p-value","median1","median2","mean1","mean2","z-score","samplesize1","samplesize2","skewness1","skewness2","kurtosis1","kurtosis2","shapiro1p","shapiro2p","K-S p-value","Exact Test","t-test","t-test z","t-test df","pairedYN","SD1","SD2","USE")

#### Between Jacket Statistical Comparisons & Interaction effects #### 

library(pgirmess)
library(car)
library(lmtest)
library(sandwich)
library(tidyr)

TestingTable[6:8][TestingTable[6:8]<0]=NA

ANOVAtesting=TestingTable
ANOVAtesting$type=as.factor(ANOVAtesting$type)
ANOVAtesting$type=relevel(ANOVAtesting$type,ref="ST")

ANOVAsep20and333=gather(data=ANOVAtesting[,-8],scaledfibermass20,scaledfibermass333,key="filtersize",value="scaledfibermass",-c(1:5))

#ANOVAs

anovatests=list()
STanova <- read.csv("STanova.csv")
for(i in 1:nrow(STanova)){
  if(grepl("comb",STanova[i,1])){inp=ANOVAtesting}else{inp=ANOVAsep20and333}
  useformula=as.character(STanova[i,1])
  STP=formula(useformula)
  linearmodel=lm(STP,inp)
  anovamodel=aov(linearmodel,na.rm=T) # ANOVA of inputted formula
  hetmodel=coeftest(linearmodel,vcov=vcovHC) #linear model with heteroskedasticity-consistent covariance matrix estimation
  anovatests[[i]]=list(anova(anovamodel),TukeyHSD(anovamodel),hetmodel,anovamodel)
  names(anovatests)[i]=as.character(STanova[i,1])
}

#Kruskal Wallis test

kruskal.test(scaledfibermasscomb~type,data=TestingTable)
kruskal.test(scaledfibermasscomb~type,data=TestingTable[TestingTable$load=="TOP",])
kruskal.test(scaledfibermasscomb~type,data=TestingTable[TestingTable$load=="FRONT",])
kruskalmc(scaledfibermasscomb~type,data=TestingTable[TestingTable$load=="FRONT",])
kruskal.test(scaledfibermasscomb~type,data=TestingTable[TestingTable$age=="NEW",])
kruskalmc(scaledfibermasscomb~type,data=TestingTable[TestingTable$age=="NEW",])
kruskal.test(scaledfibermasscomb~type,data=TestingTable[TestingTable$age=="AGED",])


