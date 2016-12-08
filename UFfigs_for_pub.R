#last edit: 10/20 fixed labelling error in tidying Sukh-hi/lo were switched.
#last edit 5/11 updating color lines and fixing subsetting issues
#last edit 4/26 adding in Sukh plotting code


#CAUTION!! If import new runs make sure Hi/Med/Lo are labelled appropriately!
#Old version of this code had levels labelled backwards!
#NOTE:  Must run "data.prep.R" file first, 
#with files from "Mongolia_SDM/data"
#which are exported model projections from Vensim

# setwd("/nfs/gallington-data/Mongolia_SDM/")

library(ggplot2)
library(scales)
library(dplyr)

##
#~~~~~~~~~~~~~~XILINGOL~~~~~~~~~~~
##

#This uses Xgl_All which was created in the data.prep.R file:


#Example subsetting code:
#test to get just crop for s4, uf0.75
# xS46<-Xgl_All%>%
#   filter(UF==0.75)%>%
#   filter(Scenario=="Scen4")%>%
#   select(CropArea)

#In order to make the main Figures so that Baseline-Scen3 represent UF=75%, 
#but Scen4=60% (based on the details of that scenario)
#Need to subset the main dataset Xgl_All, which was created in teh data.prep.R sheet
#will do it now in two steps


#Subset Step 1: Baseline-Scen3 @ UF= 75%:
xBS3<- Xgl_All%>%
  filter(UF==0.75)%>%
    filter(Scenario%in%c("Baseline","Scen1","Scen2","Scen3"))
#Subset Step 2: Scenario 4 @ 60% UF
x6S4<-Xgl_All%>%
  filter(UF==0.60)%>%
  filter(Scenario=="Scen4")
#Combine back together:
xFig<- rbind(xBS3, x6S4)
#change names of Scenarios in df to make legend more precise:
xFig$Scenario[xFig$Scenario=="Scen1"]<-"S1: Increased Precip"
xFig$Scenario[xFig$Scenario=="Scen2"]<-"S2: No Grass Protection"
xFig$Scenario[xFig$Scenario=="Scen3"]<- "S3: Cropland Expansion"
xFig$Scenario[xFig$Scenario=="Scen4"]<-"S4: No Policy"
                                                


#4/28: Now that we have subset the original compiled dataframe we don't need to subset the 
#data in the first line, so removed: data=subset(Xgl_All, UF %in%c("0.75")) & jsut specify xFig
#################################################
#Just plotting the midrange UFs (=0.75) except for Scen 4 which is =0.6
##################################################
#Grassland ALL Scenarios Black & White with symbols
xg<- ggplot(xFig, aes(x=Year, y=GrassArea/(10^6), group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=10))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=7, 
                                 angle=90))+
  theme(axis.text.y=element_text(size=7, 
                                 vjust=-0.35))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  theme(legend.position="none")+
  #theme(legend.text=element_text(labels=c("Base Model", "Increased Precip", "No Grass Policy", "No Crop Policy", "Worst Case")))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  labs(x=" ")+
  labs(y=expression(Grassland~area~ (ha) %*% 10^6)) 
xg


#----------------------
#Crop b&w
#--------------------
xc<- ggplot(xFig, aes(x=Year, y=CropArea/(10^6), group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=10))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=7, angle=90))+
  theme(axis.text.y=element_text(size=7, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="")+
  labs(y=expression(Cropland~area~ (ha) %*% 10^6))
  


#--------------------
#Livestock b&w
#-----------------
xl<- ggplot(xFig, aes(x=Year, y=LskPop/10^6, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=7, angle=90))+
  theme(axis.text.y=element_text(size=7, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="")+
  labs(y=expression(Livetock~Population~EOY~ (su) %*% 10^6))
#--------------
#Biomass
#--------------
xr<- ggplot(xFig, aes(x=Year, y=Biomass, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=10))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=7, angle=90))+
  theme(axis.text.y=element_text(size=7, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="Remaining Biomass EOY (kg/ha)")





###################
#   XILINGOL UF plots 
###################

#Subsetting to just get lo/med/hi=0.6,0.75,0.9 for UF isn't working, just
#outputs 0.9 only
Xgl14<- Xgl_All%>%
  filter(UF%in%c("0.6","0.75","0.9"))%>%
  filter(Year==2050)%>%
  filter(Scenario%in%c("Scen1","Scen2","Scen3", "Scen4"))

XglB<-Xgl_All%>%
  filter(UF=="0.75")%>%
  filter(Year==2050)%>%
  filter(Scenario%in%c("Baseline"))

Xgl<- rbind(XglB, Xgl14)
   
XglG<- select(Xgl, UF, Scenario, GrassArea)
XglC<- select(Xgl, UF, Scenario, CropArea)
XglL<- select(Xgl, UF, Scenario, LskPop)
XglB<- select(Xgl, UF, Scenario, Biomass)

require(scales)

#-----------
# Grassland projections under diff UF settings:
#---------------
ufg<- ggplot(data=XglG, aes(x=Scenario, y=GrassArea/(10^6), group=UF,color=as.factor(UF)))+
  geom_point(size=5, shape = 19)+ 
  scale_color_brewer(palette="Blues", name = "XILINGOL \nMax Urban Fraction", 
                     guide = guide_legend(direction = "horizontal", title.position = "top")) +
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","S1","S2","S3", "S4")) +
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=7, vjust=-0.35),
        axis.title.y=element_text(size=7))+
  labs(x="SCENARIOS")+
  labs(y = expression(Grass~Area~ (ha) %*% 10^6)) +
  #guide_legend(title="XILINGOL Max Urban")+ #, title.hjust = 0.5,label=TRUE )+
  # theme(legend.position = "left")+
  # theme(legend.title=element_text(face="bold"))
  # scale_color_discrete(name="XILINGOL\nMax Urban %")
  theme(legend.position="none")
ufg  



#-----------
# Lsk Pop projections under diff UF settings:
#---------------
ufl<- ggplot(data=XglL, aes(x=Scenario, y=LskPop/(10^6), group=UF, color=as.factor(UF)))+
  geom_point(shape=19, size=5)+scale_color_brewer(palette="Blues") +
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","S1","S2","S3", "S4")) +
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=10, vjust=-0.35),
        axis.title.y=element_text(size=10))+
  labs(x="SCENARIOS", y="Livestock Pop EOY (su)")+
  labs(y = expression(Livestock~Pop~EOY~ (su) %*% 10^6))+
  theme(legend.position="none")

ufl

#-----------
# Biomass projections under diff UF settings:
#---------------
ufb<- ggplot(data=XglB, aes(x=Scenario, y=Biomass, group=UF, color=as.factor(UF)))+
  geom_point(shape=19, size=5)+scale_color_brewer(palette="Blues") +
  theme_bw()+
  scale_y_continuous(labels = comma, limits = c(2000,3300))+
  scale_x_discrete(labels=c("Base","S1","S2","S3", "S4")) +
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=10, vjust=-0.35),
        axis.title.y=element_text(size=10))+
  labs(x="SCENARIOS", y="Biomass EOY (kg/ha)")+
  theme(legend.position="none")
ufb




###################################################
#
#
#~~~~~~~~~~~~~~~~~~SUKHBAATAR~~~~~~~~~~~~~~~~~~~~~~
#
#
#don't have to subset bc using UF==0.65 for all
####################################################

Sukh_All$Scenario[Sukh_All$Scenario=="Scen1"]<-"S1: Industrialization & Urbanization"
Sukh_All$Scenario[Sukh_All$Scenario=="Scen2"]<-"S2: Rural Infrastructure Investment"
Sukh_All$Scenario[Sukh_All$Scenario=="Scen3"]<- "S3: Privatization of Resources"



#GRASSLAND AREA

sg<- ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=GrassArea/(10^6), group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17))+
  theme_bw()+
  theme(axis.text.x=element_text(size=7, angle=90))+
  theme(axis.text.y=element_text(size=7, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x=" ")+
  labs(y = expression(Grassland~area~ (ha) %*% 10^6))
sg
# 

clr.red="#fb6a4a"
#LIVESTOCK POPULATION
sl<- ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=LskPop/(10^6), group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17))+
  theme_bw()+
  theme(axis.text.x=element_text(size=7, angle=90))+
  theme(axis.text.y=element_text(size=7, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x=" ")+
  labs(y=expression(Livestock~Population~EOY~ (su) %*% 10^6))
sl

#Biomass
sr<- ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=Biomass, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  # guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17))+
  theme_bw()+
  theme(axis.text.x=element_text(size=7, angle=90))+
  theme(axis.text.y=element_text(size=7, vjust=-0.35))+
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="Remaining Biomass EOY (kg/ha)")
sr
# 
# 


#############
#  UF plots #     this is to represnt the difference in the endpoints of each scenario under diff settings for UF=low/med/hi
#############


###################
#   UF plots 
###################
Sukh<-filter(Sukh_All, Year==2050 )#, UF>=0.7)
SukhG<- select(Sukh, UF, Scenario, GrassArea)
SukhL<- select(Sukh, UF, Scenario, LskPop)
SukhB<- select(Sukh, UF, Scenario, Biomass)

require(scales)

sufg<- ggplot(data=SukhG, aes(x=Scenario, y=GrassArea/(10^6), group=UF, color=as.factor(UF)))+
  geom_point(shape=19, size=5)+
  scale_color_brewer(palette="Blues", name = "SUKHBAATAR \nMax Urban Fraction",
                     guide = guide_legend(direction = "horizontal", title.position = "top")) +
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","S1","S2","S3" )) +
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=10, vjust=-0.35),
        axis.title.y=element_text(size=10))+
  labs(x="SCENARIOS", y="Grass Area (ha)")+
  labs(y = expression(Grass~Area~ (ha) %*% 10^6))+
  theme(legend.position="none")
sufg

sufl<- ggplot(data=SukhL, aes(x=Scenario, y=LskPop/(10^6), group=UF, color=as.factor(UF)))+
  geom_point(shape=19, size=5)+
  scale_color_brewer(palette="Blues") +
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","S1","S2","S3" )) +
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=10, vjust=-0.35),
        axis.title.y=element_text(size=10))+
  labs(x="SCENARIOS", y="Livestock Pop EOY (su)")+
  labs(y = expression(Livestock~Pop~EOY~ (su) %*% 10^6))+
  theme(legend.position="none")
sufl

sufb<- ggplot(data=SukhB, aes(x=Scenario, y=Biomass, group=UF, color=as.factor(UF)))+
  geom_point(shape=19, size=5)+
  scale_color_brewer(palette="Blues") +
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","S1","S2","S3" )) +
  theme(axis.text.x=element_text(size=10),
        axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=10, vjust=-0.35),
        axis.title.y=element_text(size=10))+
  labs(x="SCENARIOS", y="Biomass EOY (kg/ha)")+
  theme(legend.position="none")
sufb

