#last edit: 10/20 fixed labelling error in tidying Sukh-hi/lo were switched.
#4/26 adding in Sukh plotting code

#NOTE:  Must run "data.prep.R" AND "multiplot.R" files first, with files from "U:/GitHub/Mongolia/data"
#which are exported model projections from Vensim
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAkAAAAJCAYAAADgkQYQAAAAMElEQVR42mNgIAXY2Nj8x8cHC8AwMl9XVxe3QqwKcJmIVwFWhehW4LQSXQCnm3ABAHD6MDrmRgfrAAAAAElFTkSuQmCC
#Run Sgl again with UFs as Lo-Med-Hi?

library(ggplot2)
library(scales)

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

#Subset Step 1: Baseline-Scen3:
xBS3<- Xgl_All%>%
  filter(UF==0.75)%>%
  filter(Scenario==c("Baseline","Scen1","Scen2","Scen3"))
#Subset Step 2: Scenario 4 @ 60% UF
xS46<-Xgl_All%>%
  filter(UF==0.60)%>%
  filter(Scenario=="Scen4")
#Combine back together:
xFig<- rbind(xBS3, xS46)


##
#~~~~~~~~~~~~~~XILINGOL~~~~~~~~~~~
##
#4/28: Now that we have subset the original compiled dataframe we don't need to subset the 
#data in the first line, so removed: data=subset(Xgl_All, UF %in%c("0.75")) & jsut specify xFig
#################################################
#Just plotting the midrange UFs (=0.75)
##################################################
#Grassland
xg<- ggplot(xFig, aes(x=Year, y=GrassArea, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="Grassland area (ha)")
xg

xg+geom_line(data=xS46)+geom_point()  
    
#Crop
xc<- ggplot(xFig, aes(x=Year, y=CropArea, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="Cropland area (ha)")


#Livestock
xl<- ggplot(xFig, aes(x=Year, y=LskPop, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="Livestock Population EOY (s.u.)")

#Biomass
xr<- ggplot(xFig, aes(x=Year, y=Biomass, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="(Remaining Biomass EOY (kg/ha)")

#4/28 specified new file name to export these new figs w UF=0.6 for Scen4. They look diff so 
#keeping old ones for comparison instead of overriding.
setwd("U:/GitHub/Mongolia/Figs/IMAR")
ggsave(xg, file="Xg_grass2.png", height=5, width=8,dpi=600)
#ggsave(xg, file="Xg_grassColor.png", height=5, width=8,dpi=600)
ggsave(xc, file="Xg_crop2.png", height=5, width=8,dpi=600)
ggsave(xl, file="Xg_lsk2.png", height=5, width=8,dpi=600)
ggsave(xr, file="Xg_rmb2.png", height=5, width=8,dpi=600)


###################
#   UF plots 
###################

#Subsetting to just get lo/med/hi=0.6,0.75,0.9 for UF isn't working, just
#outputs 0.9 only
Xgl<- Xgl_All%>%
  filter(UF%in%c("0.6","0.75","0.9"))%>%
  filter(Year==2050)
   
XglG<- select(Xgl, UF, Scenario, GrassArea)
XglC<- select(Xgl, UF, Scenario, CropArea)
XglL<- select(Xgl, UF, Scenario, LskPop)
XglB<- select(Xgl, UF, Scenario, Biomass)

require(scales)

ufg<- ggplot(data=XglG, aes(x=Scenario, y=GrassArea, group=UF, color=UF))+
  geom_point(size=4)+ 
  scale_shape_manual(values=c(1,16,21))+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_blank())+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Grass Area (ha)")
ufg  

ufc<- ggplot(data=XglC, aes(x=Scenario, y=CropArea, group=UF, color=UF))+
  geom_point(shape=19, size=4)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_blank())+
        #axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Crop Area (ha)")


ufl<- ggplot(data=XglL, aes(x=Scenario, y=LskPop, group=UF, color=UF))+
  geom_point(shape=19, size=4)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Livestock Pop EOY (su)")

ufb<- ggplot(data=XglB, aes(x=Scenario, y=Biomass, group=UF, color=UF))+
  geom_point(shape=19, size=4)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Biomass EOY (kg/ha)")


#~~~~~~~~~~~RUN multiplot.R first~~~~~~~~~~~~~~~~#
##################################################
UFs<- multiplot(ufg, ufl, ufc, ufb, cols=2)

setwd("U:/GitHub/Mongolia/Figs/IMAR")

ggsave(UFs, file="UF-sens.png", height=5, width=8,dpi=600)
ggsave(ufg, file="Xg_ufg.png", height=5, width=8,dpi=600)
ggsave(ufc, file="Xg_ufc.png", height=5, width=8,dpi=600)
ggsave(ufl, file="Xg_ufl.png", height=5, width=8,dpi=600)
ggsave(ufb, file="Xg_ufb.png", height=5, width=8,dpi=600)
#
# ufgtrans<- ggplot(data=XglG, aes(x=Scenario, y=GrassArea, group=UF, color=UF))+
#   geom_point(shape=19, size=4)+
#   scale_y_continuous(trans=log_trans(), labels=trans_format())+
#   theme(axis.text.x=element_text(size=11, angle=90))+
#   theme(axis.text.y=element_text(size=11, vjust=-0.35))+
#   theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
#   theme_bw()


###################################################
#
#
#~~~~~~~~~~~~~~~~~~SUKHBAATAR~~~~~~~~~~~~~~~~~~~~~~
#
#
#
####################################################

#GRASSLAND AREA

sg<- ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=GrassArea, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="Grassland area (ha)")

setwd("U:/GitHub/Mongolia/Figs/MG")
ggsave(sg, file="Suk_grass.png", height=5, width=8,dpi=600)

#LIVESTOCK POPULATION
sl<- ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=LskPop, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="Livestock Population EOY (s.u.)")

setwd("U:/GitHub/Mongolia/Figs/MG")
ggsave(sl, file="Suk_lsk.png", height=5, width=8,dpi=600)


#Biomass
sr<- ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=Biomass, group=Scenario, shape=Scenario))+
  geom_line() + geom_point()+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(16,1,2,17))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Year", y="(Remaining Biomass EOY (kg/ha)")

setwd("U:/GitHub/Mongolia/Figs/MG")
ggsave(sr, file="Suk_rmb.png", height=5, width=8,dpi=600)


#############
#  UF plots #     this is to represnt teh difference in the endpoints of each scenario under diff settings for UF=low/med/hi
#############


###################
#   UF plots 
###################
Sukh<-filter(Sukh_All, Year==2050 )#, UF>=0.7)
SukhG<- select(Sukh, UF, Scenario, GrassArea)
SukhL<- select(Sukh, UF, Scenario, LskPop)
SukhB<- select(Sukh, UF, Scenario, Biomass)

require(scales)

#THESE NEED TO BE CHANGED TO REFLECT NEW STYLE. WHERE IS THAT CODE?? THIS IS OLD.

sufg<- ggplot(data=SukhG, aes(x=Scenario, y=GrassArea, group=UF, color=UF))+
  geom_point(shape=19, size=4)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3" )) +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_blank())+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Grass Area (ha)")



sufl<- ggplot(data=SukhL, aes(x=Scenario, y=LskPop, group=UF, color=UF))+
  geom_point(shape=19, size=4)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3" )) +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_blank())+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Livestock Pop EOY (su)")

sufb<- ggplot(data=SukhB, aes(x=Scenario, y=Biomass, group=UF, color=UF))+
  geom_point(shape=19, size=4)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3" )) +
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Biomass EOY (kg/ha)")


#UFs<- multiplot(ufg, ufc, ufl, ufb, cols=2)

setwd("U:/GitHub/Mongolia/Figs/MG")

ggsave(sufg, file="sUFg.png", height=5, width=8,dpi=600)
ggsave(sufl, file="sUFl.png", height=5, width=8,dpi=600)
ggsave(sufb, file="sUFb.png", height=5, width=8,dpi=600)

sUFs<- multiplot(sufg, sufl, sufb, cols=1)
