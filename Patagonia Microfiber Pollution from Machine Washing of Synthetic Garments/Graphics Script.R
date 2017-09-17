# Graphics

library(ggplot2)
library(RColorBrewer)
library(scales)
library(colorspace)
library(gridExtra)
library(grid)
library(tidyr)
library(dplyr)

if(!"Graphics Folder"%in%dir()){dir.create("Graphics Folder")}
setwd("~/Desktop/Graphics Folder")

GraphDat=TestingTable #Uses modified data table from the Wash Trial Statistical Analysis Script 

GraphDat$age=gsub("NEW","New",GraphDat$age)
GraphDat$age=gsub("AGED","Aged",GraphDat$age)
GraphDat$load=gsub("TOP","Top",GraphDat$load)
GraphDat$load=gsub("FRONT","Front",GraphDat$load)
names(GraphDat)[3:8]=c("Type","Age","Load","ScaledFiberMass20","ScaledFiberMass333","ScaledFiberMassComb")

GraphDat[,6:8]=GraphDat[,6:8]*1000 #convert grams to milligrams for clarity in graphics
levels(GraphDat$Type)=c("A","B","C","D","Budget")
GraphDat$Age=as.factor(GraphDat$Age)
GraphDat$Age=relevel(GraphDat$Age,ref="New")

GraphDat[c(6,7,8)][GraphDat[c(6,7,8)]<0]=NA

GraphDatSep=gather(data=GraphDat[,-8],ScaledFiberMass20,ScaledFiberMass333,key="Filtersize",value="ScaledFiberMassComb",-c(1:5))
GraphDatSep$Filtersize=as.factor(GraphDatSep$Filtersize)
levels(GraphDatSep$Filtersize)=c("20","333")
GraphDatSep$ScaledFiberMassComb[GraphDatSep$ScaledFiberMassComb<0]=NA

#### Aesthetics ####

jacketname=c("Patagonia A ","Patagonia B ","Patagonia C ","Patagonia D ","Budget ")
jacketabbv=c("A","B","C","D","Budget")
jacketcolor=c("tomato","palegreen3","darkorange","yellow1","dodgerblue")
NFAFNTAT=c("royalblue","skyblue","darkorange2","#FDAE61") 
ANcolor=c("paleturquoise","tan3")
FTcolor=c("steelblue","orange2")
ft20ft333col=brewer.pal(9,"Purples")[c(4,7)]
loadfiltersizecolor=c(brewer.pal(11,"PuOr")[c(5,4)],brewer.pal(11,"RdBu")[c(7,8)])
agefiltersizecolor=brewer.pal(11,"BrBG")[c(7,8,4,3)]

p=1.1

PresentationDefault=theme_bw()+
                        theme(text=element_text(color="black"),
                        axis.text.x=element_text(size=22*p),
                        axis.text.y=element_text(size=20*p),
                        axis.title=element_text(size=21*p),
                        axis.title.y=element_text(vjust=1.5),
                        legend.position=c(0,1),
                        legend.justification=c(0,1),
                        legend.text=element_text(size=20*p),
                        legend.title=element_text(size=21*p),
                        legend.text.align=0,
                        legend.key.size=unit(0.8,"cm"),
                        panel.grid=element_blank(),
                        plot.title=element_blank())

ReportDefault=theme_bw()+
              theme(text=element_text(color="black"),
                axis.text=element_text(size=15),
                axis.title=element_text(size=16),
                axis.title.y=element_text(vjust=1),
                title=element_text(size=13),
                legend.position=c(0,1),
                legend.justification=c(0,1),
                legend.text=element_text(size=14),
                legend.title=element_text(size=13),
                legend.text.align=0,
                legend.key.size=unit(0.8,"cm"),
                plot.title=element_blank())

####add spacing below the title

#### Report Graphics ####

aggmass5jacket=data.frame(aggregate(ScaledFiberMassComb~Type,data=GraphDat,FUN=mean))
aggmass5jacket[['ci']]=data.frame(aggregate(ScaledFiberMassComb~Type,data=GraphDat,FUN=sd))[[2]]
graphic5=ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type),data=aggmass5jacket)+
  geom_bar(stat="identity",position=position_dodge())+
  geom_bar(stat="identity",color="black",size=.8,position=position_dodge(),show_guide=FALSE)+
  scale_fill_manual(name="Jacket Type",values=jacketcolor,breaks=jacketabbv,labels=jacketname)+
  scale_x_discrete(breaks=jacketabbv,labels=NULL)+
  ylab("Average Mass (mg)")+
  xlab(NULL)+
  ylim(0,3500)+
  ggtitle("Average fiber mass shed per wash by jacket type")+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=pmax(ScaledFiberMassComb-ci,0)), width=.2,position=position_dodge(),alpha=1)+
  ReportDefault+
  theme(legend.direction="horizontal",legend.title=element_blank(),legend.background=element_rect(color="gray"),legend.position="bottom",axis.ticks.x=element_blank(),legend.justification="center",axis.ticks.x=element_blank())


tiff("4All 5 jackets graphic.tiff", width = 600, height = 500)
graphic5
dev.off()

aggmass5jacketS=data.frame(aggregate(ScaledFiberMass20~Type,data=GraphDat,FUN=mean))
names(aggmass5jacketS)[2]="ScaledFiberMass333"
aggmass5jacketS=rbind(aggmass5jacketS,data.frame(aggregate(ScaledFiberMass333~Type,data=GraphDat,FUN=mean)))
names(aggmass5jacketS)[2]="ScaledFiberMass"
aggmass5jacketS[["Filtersize"]]=c(rep("20",5),rep("333",5))
graphic5size=ggplot(aes(x=Type,y=ScaledFiberMass,fill=Type,alpha=Filtersize),data=aggmass5jacketS)+
  geom_bar(stat="identity",position="fill")+
  geom_bar(stat="identity",color="black",size=.8,position="fill",show_guide=FALSE)+
  scale_fill_manual(name="Jacket Type",values=jacketcolor,breaks=jacketabbv,labels=jacketname,guide=F)+
  scale_alpha_manual(values=c(.4,1),guide=F)+
  ylab("Percent of Shedding")+
  scale_y_continuous(labels=scales::percent)+
  xlab(NULL)+
  ggtitle("Size percent of shedding by jacket type")+
  ReportDefault

tiff("All 5 jackets size graphic.tiff", width = 600, height = 500)
graphic5size
dev.off()

tiff("1top-load front-load bar graphs.tiff", width = 500, height = 500)
aggmassBR=data.frame(aggregate(ScaledFiberMassComb~Load,data=GraphDat,FUN=mean))
sdmassBR=data.frame(aggregate(ScaledFiberMassComb~Load,data=GraphDat,FUN=sd))
aggmassBR[['ci']]=sdmassBR[[2]]
ggplot(aes_string(x="Load",y="ScaledFiberMassComb",fill="Load"),data=aggmassBR)+
  geom_bar(stat="identity",width=.4,color="black",show_guide=FALSE)+
  scale_x_discrete(labels=c("Front-load (n=30)","Top-load (n=40)"))+
  scale_fill_manual(values=FTcolor)+
  guides(fill=FALSE)+
  ylab("Average Mass (mg)")+
  xlab(NULL)+
  ylim(0,3500)+
  ggtitle("Shedding per garment Front- and Top-load treatment")+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=pmax(ScaledFiberMassComb-ci,0)), width=.1,position=position_dodge(.9),alpha=1)+
  ReportDefault
dev.off()

tiff("2age and new bar graphs.tiff", width = 500, height = 500)
aggmassBR=data.frame(aggregate(ScaledFiberMassComb~Age,data=GraphDat,FUN=mean))
sdmassBR=data.frame(aggregate(ScaledFiberMassComb~Age,data=GraphDat,FUN=sd))
aggmassBR[['ci']]=sdmassBR[[2]]
ggplot(aes_string(x="Age",y="ScaledFiberMassComb",fill="Age"),data=aggmassBR)+
  geom_bar(stat="identity",width=.4,color="black",show_guide=FALSE)+
  scale_x_discrete(labels=c("New (n=35)","Aged (n=35)"))+
  scale_fill_manual(values=ANcolor)+
  guides(fill=FALSE)+
  ylab("Average Mass (mg)")+
  xlab(NULL)+
  ylim(0,3500)+
  ggtitle("Shedding per garment New and Aged treatment")+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=ScaledFiberMassComb-ci), width=.1,position=position_dodge(.9),alpha=1)+
  ReportDefault
dev.off()

graph2box=function(datainp,grouping,namegrp1,namegrp2,titleinp,filler,lims=c(0,3500)){
  propgraph=ggplot(aes_string(x=grouping,y="ScaledFiberMassComb",fill=grouping),data=datainp)+
    geom_line(aes_string(color=grouping),yintercept=1000,width=.1,stat="hline",size=1.3,show_guide=T)+
    scale_color_manual(values=c("red3","black"),name="",labels=c("Mean","Median"))+
    geom_boxplot(stat="boxplot",width=.5,color="black",show_guide=FALSE)+
    scale_x_discrete(labels=c(namegrp1,namegrp2))+
    scale_fill_manual(values=filler)+
    guides(fill=FALSE)+
    ylab("Mass (mg)")+
    xlab(NULL)+
    ylim(lims)+
    ggtitle(titleinp)+
    geom_errorbar(stat="summary", fun.y="mean", size=1, width=0.37,color="red3", aes(ymax=..y.., ymin=..y..))+
    ReportDefault+
    theme(legend.key=element_blank(),legend.title=element_blank(),legend.key.size=unit(.8,"cm"))
  return(propgraph)
}

tiff("top-load front-load box.tiff", width = 500, height = 500)
graph2box(datainp=GraphDat,grouping="Load",namegrp1="Front-load (n=30)",namegrp2="Top-load (n=40)",titleinp="Shedding per garment New and Aged treatment",filler=ANcolor)
dev.off()

tiff("New and aged box graphic.tiff", width = 500, height = 500)
graph2box(datainp=GraphDat,grouping="Age",namegrp1="New (n=35)",namegrp2="Aged (n=35)",titleinp="Shedding per garment New and Aged treatment",filler=ANcolor)
dev.off()

tiff("7filter 20 and 333 box graphic.tiff", width = 500, height = 500)
graph2box(datainp=GraphDatSep,grouping="Filtersize",namegrp1=expression(paste("20 ",mu,"m")),namegrp2=expression(paste("333 ",mu,"m")),titleinp=expression(paste("Fiber mass per garment on 20 ",mu,"m and 333 ",mu,"m filter size")),filler=ft20ft333col,lims=c(0,3500))
dev.off()

tiff("D vs Budget comb box graphic.tiff", width = 500, height = 500)
temp2jacket=GraphDat[GraphDat$Type=="D"|GraphDat$Type=="Budget",]
temp2jacket$Type=droplevels(temp2jacket$Type)
graph2box(datainp=temp2jacket,grouping="Type",namegrp1="Patagonia D",namegrp2="Budget",titleinp="Shedding comparison: Patagonia D and Budget",filler=jacketcolor[4:5])
dev.off()


singlebox=ggplot(aes(y=ScaledFiberMassComb,x=1),data=GraphDat)+
  geom_line(aes(group=Load,color=Load),yintercept=1000,width=.1,stat="hline",size=1.3,show_guide=T)+
  scale_color_manual(values=c("red3","black"),name="",labels=c("Mean","Median"))+
  geom_boxplot(stat="boxplot",width=.05,color="black",fill="tan",show_guide=FALSE)+
  scale_x_continuous(breaks=1,limits=c(0.9,1.1))+
  guides(fill=FALSE)+
  ylab("Mass (mg)")+
  xlab("n=70")+
  ylim(0,3500)+
  ggtitle("Shedding per garment all treatments")+
  geom_errorbar(stat="summary", fun.y="mean", size=1, width=0.037,color="red3", aes(ymax=..y.., ymin=..y..))+
  ReportDefault+
  theme(axis.text.x = element_blank())

tiff("All treatments box graphic.tiff", width = 500, height = 500)
singlebox
dev.off()


#### 4 boxes ####

graph4box=function(datainp,grouping,combination,namegrp1,namegrp2,titleinp,filler,labelfill,namefill,lims=c(0,650)){
  pd=position_dodge(.5)
  propgraph=ggplot(aes_string(x=grouping,y="ScaledFiberMassComb",fill=combination),data=datainp)+
    geom_boxplot(stat="boxplot",width=.8,color="black")+
    scale_x_discrete(labels=c(namegrp1,namegrp2))+
    scale_fill_manual(name=namefill,values=filler,labels=labelfill)+
    ylab("Mass (mg)")+
    xlab(NULL)+
    ylim(lims)+
    ggtitle(titleinp)+
    geom_errorbar(stat="summary",position=position_dodge(.6), fun.y="mean", size=1, width=0.58,color="red3", aes(ymax=..y.., ymin=..y..))+
    ReportDefault+
    theme(legend.position="right")
  return(propgraph)
}

names(GraphDatSep)[7]="ScaledFiberMassComb"
GraphDatSep$Filtersize=factor(GraphDatSep$Filtersize,levels=c("20","333"))
GraphDatSep$Loadfilt=factor(paste(GraphDatSep$Load,GraphDatSep$Filtersize,sep=" "),levels=c("Front 20","Front 333","Top 20","Top 333"))
GraphDatSep$Agefilt=factor(paste(GraphDatSep$Age,GraphDatSep$Filtersize,sep=" "),levels=c("New 20","New 333","Aged 20","Aged 333"))
GraphDatSep$Ageload=factor(paste(GraphDatSep$Age,GraphDatSep$Load,sep=" "),levels=c("New Front","Aged Front","New Top","Aged Top"))

tiff("3age load box graphic.tiff", width = 600, height = 500)
graph4box(datainp=GraphDatSep,grouping="Load",combination="Ageload",namegrp1="Front-load (New vs Aged)",namegrp2="Top-load (New vs Aged)",titleinp="Fiber mass shed per garment Load and Age treatments",filler=NFAFNTAT,labelfill=levels(GraphDatSep$Ageload),namefill="Age and Load",lims=c(0,3500))
dev.off()

tiff("9front filter age box graphic.tiff", width = 600, height = 500)
graph4box(datainp=GraphDatSep,grouping="Age",combination="Agefilt",namegrp1="New",namegrp2="Aged",titleinp="Fiber mass shed per garment Age treatment and Filter Size",filler=agefiltersizecolor,labelfill=c(expression(paste("New 20 ",mu,"m")),expression(paste("New 333 ",mu,"m")),expression(paste("Aged 20 ",mu,"m")),expression(paste("Aged 333 ",mu,"m"))),lims=c(0,3500),namefill="Age and Filter Size")
dev.off()

tiff("8filter load box graphic.tiff", width = 600, height = 500)
graph4box(datainp=GraphDatSep,grouping="Load",combination="Loadfilt",namegrp1="Front-load",namegrp2="Top-load",titleinp="Fiber mass shed per garment Load treatment and Filter Size",filler=loadfiltersizecolor,labelfill=c(expression(paste("Front 20 ",mu,"m")),expression(paste("Front 333 ",mu,"m")),expression(paste("Top 20 ",mu,"m")),expression(paste("Top 333 ",mu,"m"))),namefill="Age and Filter Size",lims=c(0,3500))
dev.off()


graph4boxjacket=function(datainp,grouping,combination,namegrp1,namegrp2,titleinp,filler,labelfill,namefill,lims=c(0,650)){
  pd=position_dodge(.5)
  propgraph=ggplot(aes_string(x=grouping,y="ScaledFiberMassComb",fill=combination),data=datainp)+
    geom_boxplot(stat="boxplot",width=.8,color="black")+
    scale_x_discrete(labels=c(namegrp1,namegrp2))+
    scale_fill_manual(name=namefill,breaks=c("D 20","Budget 20"),values=filler,labels=labelfill)+
    ylab("Mass (mg)")+
    xlab("Filter Size")+
    ylim(lims)+
    ggtitle(titleinp)+
    geom_errorbar(stat="summary",position=position_dodge(.6), fun.y="mean", size=1, width=0.58,color="red3", aes(ymax=..y.., ymin=..y..))+
    ReportDefault+
    theme(legend.position="right")
  return(propgraph)
}

#do the bs vs gb comparisons
temp2jacketsep=GraphDatSep[GraphDatSep$Type=="Budget"|GraphDatSep$Type=="D",]
temp2jacketsep$Type=droplevels(temp2jacketsep$Type)
temp2jacketsep$Typefilt=factor(paste(temp2jacketsep$Type,temp2jacketsep$Filtersize,sep=" "),levels=c("D 20","Budget 20","D 333","Budget 333"))

tiff("11Front D vs Budget filter graphic.tiff", width = 600, height = 500)
graph4boxjacket(datainp=temp2jacketsep[temp2jacketsep$Load=="Front",],grouping="Filtersize",combination="Typefilt",namegrp1=expression(paste("20 ",mu,"m")),namegrp2=expression(paste("333 ",mu,"m")),titleinp="Front-load shedding comparison: Patagonia D and Budget",filler=jacketcolor[c(4,5,4,5)],labelfill=c("Patagonia D","Budget"),namefill="Jacket Type",lims=c(0,1500))
dev.off()

tiff("10Top D vs Budget filter graphic.tiff", width = 600, height = 500)
graph4boxjacket(datainp=temp2jacketsep[temp2jacketsep$Load=="Top",],grouping="Filtersize",combination="Typefilt",namegrp1=expression(paste("20 ",mu,"m")),namegrp2=expression(paste("333 ",mu,"m")),titleinp="Top-load shedding comparison: Patagonia D and Budget",filler=jacketcolor[c(4,5,4,5)],labelfill=c("Patagonia D","Budget"),namefill="Jacket Type",lims=c(0,3000))
dev.off()


facetagg=data.frame(aggregate(ScaledFiberMassComb~Type+Load+Age,data=GraphDat,FUN=mean))
levels(facetagg$Load)=c("Front-load","Top-load")
tiff("12facet age and load graphic.tiff", width = 600, height = 600)
ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type),data=facetagg)+
  geom_point(aes(shape=Type),size=3.5)+
  facet_grid(Age~Load)+
  scale_fill_manual(name="Jacket Type",breaks=jacketabbv,labels=jacketname,values=jacketcolor)+
  scale_x_discrete(breaks=NULL)+
  scale_shape_manual(name="Jacket Type",breaks=jacketabbv,labels=jacketname,values=c(21:25))+
  ylab("Average Mass (mg)")+
  xlab(NULL)+
  ggtitle("Average fiber mass shed for Age and Load treatments")+
  ReportDefault+
  theme(strip.text=element_text(size=14,face="bold"),legend.direction="horizontal",legend.title=element_blank(),legend.background=element_rect(color="gray"),legend.position="bottom",legend.justification="center")
dev.off()

facetaggS=data.frame(aggregate(ScaledFiberMassComb~Type+Load+Age+Filtersize,data=GraphDatSep,FUN=mean))
levels(facetaggS$Load)=c("Front-load","Top-load")
tiff("13facet size age and load graphic.tiff", width = 600, height = 600)
ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type,alpha=Filtersize),data=facetaggS)+
  geom_bar(aes(fill=Type),stat="identity",position="fill")+
  geom_bar(aes(fill=Type),stat="identity",position="fill",color="black",show_guide=F)+
  facet_grid(Age~Load)+
  scale_fill_manual(name="Jacket Type",breaks=jacketabbv,labels=jacketname,values=jacketcolor)+
  scale_x_discrete(breaks=NULL)+
  scale_y_continuous(labels=scales::percent)+
  scale_alpha_manual(values=c(.3,.5),guide=F)+
  ylab("Mass Percent")+
  xlab(NULL)+
  ggtitle("Size percentage of fiber mass shed for Age and Load treatments")+
  ReportDefault+
  theme(strip.text=element_text(size=14,face="bold"),legend.direction="horizontal",legend.title=element_blank(),legend.background=element_rect(color="gray"),legend.position="bottom",panel.grid=element_blank(),legend.justification="center")
dev.off()

aggmass6=data.frame(aggregate(ScaledFiberMassComb~Type+Load,data=GraphDat,FUN=mean))
sdmass6=data.frame(aggregate(ScaledFiberMassComb~Type+Load,data=GraphDat,FUN=sd))
aggmass6[['ci']]=sdmass6[[3]]
levels(aggmass6$Load)=c("Front-load","Top-load")
tiff("5front and Top-Load 5 bars graphic.tiff", width = 700, height = 500)
ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type),data=aggmass6)+
  geom_bar(stat="identity",position=position_dodge(),show_guide=T)+
  geom_bar(stat="identity",color="black",size=.8,position=position_dodge(),show_guide=FALSE)+
  facet_grid(.~Load)+
  scale_x_discrete(breaks=jacketabbv,labels=NULL)+
  scale_fill_manual(name="Jacket Type",values=jacketcolor,breaks=jacketabbv,labels=jacketname)+
  xlab(NULL)+
  ylab("Average Fiber Mass (mg)")+
  ylim(0,3500)+
  ggtitle("Front- and Top-load treatment average fiber mass shed")+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=pmax(ScaledFiberMassComb-ci,0)), width=.2,position=position_dodge(),alpha=1)+
  ReportDefault+
  theme(strip.text=element_text(size=14,face="bold"),legend.direction="horizontal",legend.title=element_blank(),legend.background=element_rect(color="gray"),legend.position="bottom",axis.ticks.x=element_blank(),legend.justification="center")
dev.off()

tiff("6new and aged 5 bars graphic.tiff", width = 700, height = 500)
aggmass7=data.frame(aggregate(ScaledFiberMassComb~Type+Age,data=GraphDat,FUN=mean))
sdmass7=data.frame(aggregate(ScaledFiberMassComb~Type+Age,data=GraphDat,FUN=sd))
aggmass7[['ci']]=sdmass7[[3]]
ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type),data=aggmass7)+
  geom_bar(stat="identity",position=position_dodge(),show_guide=T)+
  geom_bar(stat="identity",color="black",size=.8,position=position_dodge(),show_guide=FALSE)+
  facet_grid(.~Age)+
  scale_x_discrete(breaks=jacketabbv,labels=NULL)+
  scale_fill_manual(name="Jacket Type",values=jacketcolor,breaks=jacketabbv,labels=jacketname)+
  xlab(NULL)+
  ylab("Average Fiber Mass (mg)")+
  ylim(0,3500)+
  ggtitle("New and Aged treatment average fiber mass shed")+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=pmax(ScaledFiberMassComb-ci,0)), width=.2,position=position_dodge(),alpha=1)+
  ReportDefault+
  theme(strip.text=element_text(size=14,face="bold"),legend.direction="horizontal",legend.title=element_blank(),legend.background=element_rect(color="gray"),legend.position="bottom",axis.ticks.x=element_blank(),legend.justification="center")
dev.off()

#### Poster Graphics ####

tiff("vert Front and top bar graphic.tiff", width = 600, height = 500)
aggmassBR=data.frame(aggregate(ScaledFiberMassComb~Load,data=GraphDat,FUN=mean))
sdmassBR=data.frame(aggregate(ScaledFiberMassComb~Load,data=GraphDat,FUN=sd))
aggmassBR[['ci']]=sdmassBR[[2]]
ggplot(aes_string(x="Load",y="ScaledFiberMassComb",fill="Load"),data=aggmassBR)+
  geom_bar(stat="identity",width=.4,color="black",show_guide=FALSE)+
  scale_x_discrete(labels=c("Front-load","Top-load"))+
  scale_fill_manual(values=c("#1B5386","#F17341"))+
  guides(fill=FALSE)+
  ylab("Average Mass (g)")+
  xlab(NULL)+
  ylim(0,2)+
  ggtitle("Shedding per garment Front- and Top-load treatment")+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=ScaledFiberMassComb-ci), width=.1,position=position_dodge(.9),alpha=1)+
  PresentationDefault
dev.off()

tiff("vert Aged and new bar graphic.tiff", width = 600, height = 500)
aggmassA=data.frame(aggregate(ScaledFiberMassComb~Age,data=GraphDat,FUN=mean))
sdmassA=data.frame(aggregate(ScaledFiberMassComb~Age,data=GraphDat,FUN=sd))
aggmassA[['ci']]=sdmassA[[2]]
ggplot(aes_string(x="Age",y="ScaledFiberMassComb",fill="Age"),data=aggmassA)+
  geom_bar(stat="identity",width=.4,color="black",show_guide=FALSE)+
  scale_x_discrete(labels=c("New","Aged"))+
  scale_fill_manual(values=c("#1B5386","#F17341"))+
  guides(fill=FALSE)+
  ylab("Average Mass (g)")+
  xlab(NULL)+
  ylim(0,2)+
  ggtitle("Shedding per garment aging treatment")+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=ScaledFiberMassComb-ci), width=.1,position=position_dodge(.9),alpha=1)+
  PresentationDefault
dev.off()

aggmass6=data.frame(aggregate(ScaledFiberMassComb~Type+Load,data=GraphDat,FUN=mean))
sdmass6=data.frame(aggregate(ScaledFiberMassComb~Type+Load,data=GraphDat,FUN=sd))
aggmass6[['ci']]=sdmass6[[3]]
levels(aggmass6$Load)=c("Front-load","Top-load")
tiff("front and Top-Load 5 bars graphic.tiff", width = 800*p, height = 500*p)
ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type),data=aggmass6)+
  geom_bar(stat="identity",position=position_dodge(),show_guide=F)+
  geom_bar(stat="identity",color="black",size=.8,position=position_dodge(),show_guide=FALSE)+
  facet_grid(.~Load)+
  aes(fill=as.factor(Load))+
  scale_x_discrete(breaks=jacketabbv)+
  scale_fill_manual(values=c("#1B5386","#F17341"))+
  xlab(NULL)+
  ylab("Average Fiber Mass (g)")+
  ylim(0,2500)+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=pmax(ScaledFiberMassComb-ci,0)), width=.2,position=position_dodge(),alpha=1)+
  PresentationDefault+
  theme(strip.text=element_text(size=20*p,face="bold"),panel.margin=unit(3, "lines"))
dev.off()

aggmass7=data.frame(aggregate(ScaledFiberMassComb~Type+Age,data=GraphDat,FUN=mean))
sdmass7=data.frame(aggregate(ScaledFiberMassComb~Type+Age,data=GraphDat,FUN=sd))
aggmass7[['ci']]=sdmass7[[3]]
tiff("New and Aged 5 bars graphic.tiff", width = 800*p, height = 500*p)
ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type),data=aggmass7)+
  geom_bar(stat="identity",position=position_dodge(),show_guide=F)+
  geom_bar(stat="identity",color="black",size=.8,position=position_dodge(),show_guide=FALSE)+
  facet_grid(.~Age)+
  aes(fill=as.factor(Age))+
  scale_x_discrete(breaks=jacketabbv,labels=jacketabbv)+
  xlab(NULL)+
  ylab("Average Fiber Mass (g)")+
  ylim(0,2500)+
  scale_fill_manual(values=c("#1B5386","#F17341"))+
  geom_errorbar(aes(ymax=ScaledFiberMassComb+ci,ymin=pmax(ScaledFiberMassComb-ci,0)), width=.2,position=position_dodge(),alpha=1)+
  PresentationDefault+
  theme(strip.text=element_text(size=20*p,face="bold"),panel.margin=unit(3, "lines"))
dev.off()

#### Presentation ####

aggmass8=data.frame(aggregate(ScaledFiberMassComb~Type,data=GraphDat,FUN=mean))
sdmass8=data.frame(aggregate(ScaledFiberMassComb~Type,data=GraphDat,FUN=sd))
aggmass8[['ci']]=sdmass8[[2]]
aggmass8[,2:3]=aggmass8[,2:3]
tiff("5 bars graphic updated.tiff", width = 600*p, height = 500*p)
ggplot(aes(x=Type,y=ScaledFiberMassComb,fill=Type),data=aggmass8)+
  geom_bar(stat="identity",position=position_dodge(),width=.6,show_guide=F)+
  geom_bar(stat="identity",color="black",size=1,width=.7,position=position_dodge(),show_guide=FALSE)+
  scale_x_discrete(breaks=jacketabbv,labels=jacketabbv)+
  xlab(NULL)+
  ylab("Average Fiber Mass (g)")+
  ylim(0,1.5)+
  scale_fill_manual(values=c(rep("#1B5386",3),"#F17341","#F17341"))+
  PresentationDefault
dev.off()

