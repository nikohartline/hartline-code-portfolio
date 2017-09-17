#ggplot code for organized IUCN data and food demand models.

library(ggplot2)
library(RColorBrewer)
library(scales)
library(colorspace)
library(tidyr)
library(dplyr)
library(grid)

#SpeciesTable=read.csv("~/Desktop/Tilman Project/All_Species_Table.csv")

#makes a threat column with combined wood crops, livestock, and food crops (Agricultural Land Clearing)
SpeciesTable[["THR_AgricultureLandClearing"]]=pmin(SpeciesTable[["THR_Livestock"]]+SpeciesTable[["THR_Wood_Crops"]]+SpeciesTable[["THR_Food_Crops"]],1)
SpeciesTable[["THR_Hunting"]]=pmin(SpeciesTable[["THR_Hunting"]]+SpeciesTable[["THR_MarineHunting"]],1)

#Converts extinct in the wild to extinct:
SpeciesTable$Global_Status[SpeciesTable$Global_Status=="EW"]="EX"

#threats selected by Dave to graph:
SelectedThreats=c("THR_AgricultureLandClearing",
                  "THR_Logging",
                  "THR_Hunting",
                  "THR_ClimateAndWeather")

#threats above for graphics labeling:
Threats=c("Agricultural Land Clearing",
          "Logging",
          "Hunting & Collecting",
          "Climate Change")

#four chordate groups of interest (for graphics labeling):
Classifications=c("Mammals",
                  "Birds",
                  "Amphibians",
                  "Reptiles")

#total diversity of the four chordate groups:
TotalDiversity=table(SpeciesTable$ClassificationForAggregation)[c(3,2,1,5)]

#Global statuses of interest:
Status=c("Extinct",
         "Critically Endangered",
         "Endangered",
         "Vulnerable")

#### Graphics ####

K=2 #Scalar to apply to all dimensions and fonts
pd=position_dodge(width=0.1)

ThemeDefault=theme_bw()+
  theme(text=element_text(color="black"),
        line=element_line(color="black"),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.line = element_line(color="black"),
        axis.text=element_text(size=26*K),
        axis.text.x=element_text(size=28*K),
        axis.title.y=element_text(size=28*K,vjust=1.8),
        legend.position=c(.03,.91),
        legend.justification=c(0,1),
        legend.text=element_text(size=25*K),
        legend.title=element_text(size=25*K),
        legend.text.align=0,
        legend.key.size=unit(1.6,"cm"),
        legend.key=element_rect(colour="black",size=1),
        plot.margin=unit(c(.4,.4,.4,.4),"cm"))

#### Threat Graphics ####

#Creates a table of threats by taxonomic family
df=data.frame(Threats)
df[,Classifications]=NA
for(i in 1:nrow(df)){
  THRtable=data.frame(table(SpeciesTable[[SelectedThreats[i]]],SpeciesTable[["ClassificationForAggregation"]]))
  Freq=THRtable[["Freq"]][THRtable$Var1==1]
  df[i,2:5]=Freq[c(3,2,1,5)]
}

#convert table of threats to long format
graphics=gather(df,key="Family",value="Number.of.Species",-Threats)
graphics[["Percent.of.Species"]]=graphics$Number.of.Species/TotalDiversity[match(graphics$Family,Classifications)]
graphics$Threats=factor(graphics$Threats,levels=df$Threats)

ThreatColor=c("gray0","gray33","gray66","gray100")

TG=ggplot(graphics,aes_string(x="Family",y="Number.of.Species",fill="Threats"))
TG=TG+geom_bar(stat="identity",position="dodge")
TG=TG+geom_bar(stat="identity",position="dodge",color="black",show_guide=F)
TG=TG+scale_fill_manual(values=ThreatColor)
TG=TG+scale_y_continuous(expand=c(0,0),breaks=seq(from=0,to=3250,by=250),labels=c(0,"",500,"",comma(1000),"",comma(1500),"",comma(2000),"",comma(2500),"",comma(3000),""),limits=c(0,3300))
TG=TG+ThemeDefault
TG=TG+xlab(NULL)
TG=TG+ylab("Number of Species")

# TGP=ggplot(graphics,aes_string(x="Family",y="Percent.of.Species",fill="Threats"))
# TGP=TGP+geom_bar(stat="identity",position="dodge")
# TGP=TGP+geom_bar(stat="identity",position="dodge",color="black",show_guide=F)
# TGP=TGP+scale_fill_manual(values=ThreatColor)
# TGP=TGP+scale_y_continuous(labels=scales::percent)
# TGP=TGP+ThemeDefault
# TGP=TGP+xlab(NULL)
# TGP=TGP+ylab("Percent of Family Biodiversity")
# TGP

#### Global Status Graphics ####

GSdf=data.frame(table(SpeciesTable$ClassificationForAggregation,SpeciesTable$Global_Status))
GSdf=GSdf[grepl("CR|EN|EX",GSdf$Var2)&GSdf$Var1!="PLANTAE",]
colnames(GSdf)=c("Family","Global.Status","Number.of.Species")

GSdf[["Percent.of.Species"]]=GSdf$Number.of.Species/TotalDiversity[match(GSdf$Family,names(TotalDiversity))]

GSdf$Family=factor(GSdf$Family,levels=c("MAMMALIA","AVES","AMPHIBIA","REPTILIA"))
levels(GSdf$Family)=Classifications

GSdf$Global.Status=factor(GSdf$Global.Status,levels=c("EX","CR","EN"))
levels(GSdf$Global.Status)=Status
GSdf$Global.Status=factor(GSdf$Global.Status,levels=Status)
GSdf=GSdf[order(GSdf$Global.Status),]

StatusColor=rev(brewer.pal(name="OrRd", n=4))[1:3]

SG=ggplot(GSdf,aes_string(x="Family",y="Number.of.Species",fill="Global.Status"))
SG=SG+geom_bar(stat="identity",width=.6)
SG=SG+geom_bar(stat="identity",width=.6,color="black",show_guide=F)
SG=SG+scale_fill_manual(name="Global Status",values=StatusColor,guide = guide_legend(reverse=TRUE))
SG=SG+scale_y_continuous(expand=c(0,0),breaks=seq(from=0,to=1500,by=125),labels=c(0,"",250,"",500,"",750,"",comma(1000),"",comma(1250),"",comma(1500)),limits=c(0,1550))
SG=SG+ThemeDefault
SG=SG+xlab(NULL)
SG=SG+ylab("Number of Species")

SGP=ggplot(GSdf,aes_string(x="Family",y="Percent.of.Species",fill="Global.Status"))
SGP=SGP+geom_bar(stat="identity",width=.6)
SGP=SGP+geom_bar(stat="identity",width=.6,color="black",show_guide=F)
SGP=SGP+scale_fill_manual(name="Global Status",values=StatusColor,guide = guide_legend(reverse=TRUE))
SGP=SGP+scale_y_continuous(expand=c(0,0),breaks=seq(from=0,to=.21,by=.01),labels=c("0%",rep("",4),scales::percent(.05),rep("",4),scales::percent(.1),rep("",4),scales::percent(.15),rep("",4),scales::percent(0.2),""),limits=c(0,.21))
SGP=SGP+ThemeDefault
SGP=SGP+xlab(NULL)
SGP=SGP+ylab("Percent of Total Group Diversity")

setwd("~/Desktop/Finalized Graphics 4-15")

png("Global IUCN Statuses By Class.png", width = 800*K, height = 600*K)
SG
dev.off()

png("Global IUCN Statuses as Percent of Class Biodiversity.png", width = 800*K, height = 600*K)
SGP
dev.off()

png("IUCN Threats By Class.png", width = 800*K, height = 600*K)
TG
dev.off()


#### Global Diet ####

DietLC=data.frame(c("B.A.U.\nClosed)","Intensification\n(Yield Gap","Mediterranean\nDiet","Vegetarian\nDiet"),c(590,131,129,-12))
colnames(DietLC)=c("Scenario","AddCL")

FG=ggplot(DietLC,aes_string(x="Scenario",y="AddCL"))
FG=FG+geom_bar(stat="identity",width=.7)
FG=FG+geom_bar(stat="identity",width=.7,color="black",fill="grey60",show_guide=F)
FG=FG+scale_y_continuous(breaks=seq(from=-100,to=600,by=50),labels=c(-100,"",0,"",100,"",200,"",300,"",400,"",500,"",600),limits=c(-20,610))
FG=FG+ThemeDefault
FG=FG+scale_x_discrete(expand=c(.05,0))
FG=FG+xlab(NULL)
FG=FG+ylab("Additional Cropland\n(million ha)")
FG=FG+geom_hline(y=0)
FG=FG+theme(axis.text.x=element_text(hjust=1,angle=45))

png("Cropland Scenarios.png", width = 700*K, height = 600*K)
FG
dev.off()

#### GHG emissions for three scenarios ####

DietGHG=data.frame(c("B.A.U.","Mediterranean\nDiet","Vegetarian\nDiet"),c(6.7*(2/7.3),2.5*(2/7.3),-1.9*(2/7.3)))
colnames(DietGHG)=c("Scenario","AddGHG")

GG=ggplot(DietGHG,aes_string(x="Scenario",y="AddGHG"))
GG=GG+geom_bar(stat="identity",width=.47)
GG=GG+geom_bar(stat="identity",width=.47,color="black",fill="grey60",show_guide=F)
GG=GG+scale_y_continuous(breaks=seq(from=-.6,to=2,by=.1),labels=c("",-.5,rep("",4),0,rep("",4),.5,rep("",4),1,rep("",4),1.5,rep("",4),2),limits=c(-.6,2.01))
GG=GG+ThemeDefault
GG=GG+xlab(NULL)
GG=GG+scale_x_discrete(expand=c(.05,0))
GG=GG+ylab("Change in Crop GHG Emissions\n(CO  -C   Gt/yr)")
GG=GG+geom_hline(y=0)
GG=GG+theme(axis.text.x=element_text(hjust=1,angle=45))

png("Cropland GHG Scenarios.png", width = 700*K, height = 600*K)
GG
dev.off()

