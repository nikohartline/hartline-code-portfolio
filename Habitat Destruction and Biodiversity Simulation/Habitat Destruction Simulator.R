#### Habitat Destruction Simulation ####

library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)

#FN = Field Column Name
#TN = Transect Column Name
#PN = Plot Column Name

#Function to count species and return an average across field or transect (or total for the entire study). Input includes the field and the total diversity
EXC=function(x,tot){
  if(any(x<0)){stop("Negative value input")}
  x[x>0]=1
  return(sum(x)/tot)
}

#Inputs include field column and transect column (if it exists). Takes a data table (either unmodified or partly destroyed) and counts its species diversity across all fields, average per field, and average per transect starting with column # startcol.
DiversityCounter=function(FN,TN,startcol,dat,datorig){
  Diversity=list()
  Diversity[["TotalDiversity"]]=EXC(colSums(dat[,startcol:ncol(dat)]),tot=1)
  Diversity[["FieldAverageDiversity"]]=EXC(rowsum(dat[,startcol:ncol(dat)],group=dat[[FN]]),tot=length(table(datorig[[FN]])))
  dat$FT=paste(dat[[FN]],dat[[TN]],sep="_")
  datorig$FT=paste(datorig[[FN]],datorig[[TN]],sep="_")
  Diversity[["TransectAverageDiversity"]]=EXC(rowsum(dat[,startcol:(ncol(dat)-1)],group=dat[["FT"]]),tot=length(unique(datorig$FT)))
  return(Diversity)
}

DestructionScenario=function(FN,TN,PN,startcol,dat){
  
  Scenarios=list()
  
  #Random Plot Destruction Scenario
  
  N=nrow(dat)
  RandomDestruction=sample(1:N,N,replace=F)
  RandomTable=DestructionSimulation(FN,TN,PN,startcol,dat,RandomDestruction)
  #RandomGraph=SimulationGrapher(RandomTable) #make this into a graphing function
  RandomGraph=NA
  Scenarios[["Random Plot Destruction"]]=RandomTable
  
  #Transect Destruction Scenario

  TF=paste(dat[[FN]],dat[[TN]])
  TFunique=unique(TF)
  N=length(TFunique)
  TransectOrder=sample(1:N,N,replace=F)
  TransectDestruction=vector(mode="numeric", length=length(TF))
  plots=0
  for(i in 1:N){
    start=plots+1
    plots=plots+sum(TF==TFunique[TransectOrder[i]])
    TransectDestruction[TF==TFunique[TransectOrder[i]]]=sample(start:plots,sum(TF==TFunique[TransectOrder[i]]),replace=F)
  }

  TransectTable=DestructionSimulation(FN,TN,PN,startcol,dat,TransectDestruction)
  #TransectGraph=SimulationGrapher(TransectTable) #make this into a graphing function
  TransectGraph=NA
  Scenarios[["Transect Destruction"]]=TransectTable
  
  #Field Destruction Scenario
  
  Funique=unique(dat[[FN]])
  N=length(Funique)
  FieldOrder=sample(1:N,N,replace=F)
  FieldDestruction=vector(mode="numeric", length=nrow(dat))
  plots=0
  for(i in 1:N){
    start=plots+1
    plots=plots+sum(dat[[FN]]==Funique[FieldOrder[i]])
    FieldDestruction[dat[[FN]]==Funique[FieldOrder[i]]]=sample(start:plots,sum(dat[[FN]]==Funique[FieldOrder[i]]),replace=F)
  }
  
  FieldTable=DestructionSimulation(FN,TN,PN,startcol,dat,FieldDestruction)
  #FieldGraph=SimulationGrapher(FieldTable) #make this into a graphing function
  FieldGraph=NA
  Scenarios[["Field Destruction"]]=FieldTable
  
  return(Scenarios)
}

#Inputs include field column, transect column, and plot column (if it exists). Start column of species and a vector giving order of habitat destruction should also be provided. The output is a table of 9 variables with various diversity indicators along with the percent of destruction.
DestructionSimulation=function(FN,TN,PN,startcol,dat,destructionorder){
  N=nrow(dat) #number of plots in dataset
  RandomPlotDestruction=data.frame(matrix(data=0:N,nrow=N+1,ncol=1))
  names(RandomPlotDestruction)="Step"
  for(i in 1:N){
    if(i==1){inp=dat}else{inp=dat[-destructionorder[1:(i-1)],]}
    RandomPlotDestruction[i,"Total Diversity"]=DiversityCounter(FN,TN,startcol,inp,dat)[[1]]
    RandomPlotDestruction[i,"Average Field Diversity"]=DiversityCounter(FN,TN,startcol,inp,dat)[[2]]
    RandomPlotDestruction[i,"Average Transect Diversity"]=DiversityCounter(FN,TN,startcol,inp,dat)[[3]]
    RandomPlotDestruction[i,"Number of Plots Destroyed"]=N-nrow(inp)
    RandomPlotDestruction[i,"Percent of Total Diversity Remaining"]=RandomPlotDestruction[i,"Total Diversity"]/RandomPlotDestruction[1,"Total Diversity"]
    RandomPlotDestruction[i,"Percent of Average Field Diversity Remaining"]=RandomPlotDestruction[i,"Average Field Diversity"]/RandomPlotDestruction[1,"Average Field Diversity"]
    RandomPlotDestruction[i,"Percent of Average Transect Diversity Remaining"]=RandomPlotDestruction[i,"Average Transect Diversity"]/RandomPlotDestruction[1,"Average Transect Diversity"]
    RandomPlotDestruction[i,"Percent of Plots Destroyed"]=(N-nrow(inp))/N
    if(i%%200==0){print(sprintf("%i out of %i plots destroyed",i,N))}
  }
  RandomPlotDestruction[(N+1),2:9]=c(0,0,0,N,0,0,0,1)
  return(RandomPlotDestruction)
}

SimulationGrapher=function(RandomPlotDestruction){
  names(RandomPlotDestruction)[9]="Percent_of_Plots_Destroyed"
  val=gather(RandomPlotDestruction[c(9,2:4)],key=metric,value=value,-Percent_of_Plots_Destroyed)
  pct=gather(RandomPlotDestruction[6:9],key=metric,value=value,-Percent_of_Plots_Destroyed)
  SAR10=function(x){return((1-x)^.1)}
  SAR15=function(x){return((1-x)^.15)}
  SAR20=function(x){return((1-x)^.2)}
  SAR25=function(x){return((1-x)^.25)}
  Graphics=list()
  Graphics[[1]]=ggplot(data=val)+
    geom_line(aes(x=Percent_of_Plots_Destroyed,y=value,color=metric))+
    ylab("Species Remaining")+
    xlab("Percent of Plots Destroyed")+
    scale_x_continuous(labels=percent)+
    scale_color_manual(values=c("red","green","blue"))
  Graphics[[2]]=ggplot(data=pct)+
    geom_line(aes(x=Percent_of_Plots_Destroyed,y=value,color=metric))+
    xlab("Percent of Plots Destroyed")+
    ylab("Percent of Species Remaining")+
    scale_x_continuous(labels=percent)+
    scale_y_continuous(labels=percent)+
    scale_color_manual(values=c("red","green","blue"))
  PctDiversityOnly=ggplot(data=pct[pct$metric=="Percent of Total Diversity Remaining",])+
    geom_line(aes(x=Percent_of_Plots_Destroyed,y=value),color="blue")+
    xlab("Plots Destroyed (%)")+
    ylab("Species Remaining (%)")+
    scale_x_continuous(labels=percent)+
    scale_y_continuous(labels=percent)+
    scale_color_manual(values=c("red","green","blue"))
  Sar1=PctDiversityOnly+
    stat_function(aes(x=Percent_of_Plots_Destroyed),fun=SAR10)+
    ggtitle("z=.10")
  Sar2=PctDiversityOnly+
    stat_function(aes(x=Percent_of_Plots_Destroyed),fun=SAR15)+
    ggtitle("z=.15")
  Sar3=PctDiversityOnly+
    stat_function(aes(x=Percent_of_Plots_Destroyed),fun=SAR20)+
    ggtitle("z=.20")
  Sar4=PctDiversityOnly+
    stat_function(aes(x=Percent_of_Plots_Destroyed),fun=SAR25)+
    ggtitle("z=.25")
  Graphics[[3]]=grid.arrange(Sar1,Sar2,Sar3,Sar4,nrow=2,ncol=2,top=textGrob("Overall Diversity and SAR model"))
  return(Graphics)
}


