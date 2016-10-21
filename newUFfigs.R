#last edit: 10/20 fixed labelling error in tidying Sukh-hi/lo were switched.
#last edit 5/11 updating color lines and fixing subsetting issues
#last edit 4/26 adding in Sukh plotting code

#NOTE:  Must run "data.prep.R" AND "multiplot.R" files first, with files from "Mongolia_SDM/data"
#which are exported model projections from Vensim

setwd("/nfs/gallington-data/Mongolia_SDM/")

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
    filter(Scenario%in%c("Baseline","Scen1","Scen2","Scen3"))
#Subset Step 2: Scenario 4 @ 60% UF
x6S4<-Xgl_All%>%
  filter(UF==0.60)%>%
  filter(Scenario=="Scen4")
#Combine back together:
xFig<- rbind(xBS3, x6S4)


##
#~~~~~~~~~~~~~~XILINGOL~~~~~~~~~~~
##
#4/28: Now that we have subset the original compiled dataframe we don't need to subset the 
#data in the first line, so removed: data=subset(Xgl_All, UF %in%c("0.75")) & jsut specify xFig
#################################################
#Just plotting the midrange UFs (=0.75) except for Scen 4 which is =0.6
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
#color lines:
xg.col<-ggplot(data=xFig, aes(x=Year, y=GrassArea, group=Scenario, shape=Scenario, color=Scenario)) +
  geom_line(size=1.2) + #geom_point(size=2)+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(1:11))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  labs(x="Year", y="Grassland area (ha)")
xg.col


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
#Crop color
xc.col<-ggplot(data=xFig, aes(x=Year, y=CropArea, group=Scenario,  color=Scenario)) +
  #geom_jitter()+
  geom_line(size=1.2)+ # geom_jitter(width=1, height=2)+ +# geom_point(size=2)
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(1:11))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  labs(x="Year", y="Cropland area (ha)")
xc.col

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

#Lsk Color:
xl.col<-ggplot(data=xFig, aes(x=Year, y=LskPop, group=Scenario, shape=Scenario, color=Scenario)) +
  geom_line(size=1.2) +# geom_point(size=2)+ geom_jitter(width=1, height=2)+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(1:11))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  labs(x="Year", y="Livestock Population EOY (s.u.)")
xl.col

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

#Biomass Color:
xb.col<-ggplot(data=xFig, aes(x=Year, y=Biomass, group=Scenario, shape=Scenario, color=Scenario)) +
  geom_line(size=1.2) +# geom_point(size=2)+ geom_jitter(width=1, height=2)+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(1:11))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  labs(x="Year", y="Remaining Biomass EOY (kg/ha)")
xb.col

#4/28 specified new file name to export these new figs w UF=0.6 for Scen4. They look diff so 
#keeping old ones for comparison instead of overriding.
#5/11 fixed data subsetting code to get all years and also added plotting color lines 
setwd("./plots")

ggsave(xg, file="Xg_grass.png", height=5, width=8,dpi=600)
#ggsave(xg, file="Xg_grassColor.png", height=5, width=8,dpi=600)
ggsave(xc, file="Xg_crop.png", height=5, width=8,dpi=600)
ggsave(xl, file="Xg_lsk.png", height=5, width=8,dpi=600)
ggsave(xr, file="Xg_rmb.png", height=5, width=8,dpi=600)

#SAVE COLOR PLOTS:

ggsave(xg.col, file="Xg_grassColor.png", height=5, width=8,dpi=600)
ggsave(xc.col, file="Xg_cropColor.png", height=5, width=8,dpi=600)
ggsave(xl.col, file="Xg_lskColor.png", height=5, width=8,dpi=600)
ggsave(xb.col, file="Xg_rmbColor.png", height=5, width=8,dpi=600)

###################
#   UF plots 
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

ufg<- ggplot(data=XglG, aes(x=Scenario, y=GrassArea, group=UF, color=UF))+
  geom_point(size=10)+ 
  scale_shape_manual(values=c(1,16,21))+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=22),
        axis.title.x=element_blank())+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Grass Area (ha)")+
  theme(legend.position="none")
ufg  

ufc<- ggplot(data=XglC, aes(x=Scenario, y=CropArea, group=UF, color=UF))+
  geom_point(shape=19, size=10)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=22),
        axis.title.x=element_blank())+
        #axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Crop Area (ha)")+
  theme(legend.position="none")
ufc

ufl<- ggplot(data=XglL, aes(x=Scenario, y=LskPop, group=UF, color=UF))+
  geom_point(shape=19, size=10)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=22),axis.title.x=element_blank())+
        #axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Livestock Pop EOY (su)")+
  theme(legend.position="none")

ufl

ufb<- ggplot(data=XglB, aes(x=Scenario, y=Biomass, group=UF, color=UF))+
  geom_point(shape=19, size=10)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3", "4")) +
  theme(axis.text.x=element_text(size=22),axis.title.x=element_blank())+
        #axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Biomass EOY (kg/ha)")+
  theme(legend.position="none")
ufb

#~~~~~~~~~~~RUN multiplot.R first~~~~~~~~~~~~~~~~#
##################################################
UFs<- multiplot(ufg, ufl, ufc, ufb, cols=2)

setwd("./plots")

ggsave(UFs, file="UF-sens.png", height=5, width=6,dpi=600)
ggsave(ufg, file="Xg_ufg.png", height=5, width=6,dpi=600)
ggsave(ufc, file="Xg_ufc.png", height=5, width=6,dpi=600)
ggsave(ufl, file="Xg_ufl.png", height=5, width=6,dpi=600)
ggsave(ufb, file="Xg_ufb.png", height=5, width=6,dpi=600)
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
#don't have to subset bc using 0.65 for all
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
sg

#Sukh Grass Color:
sg.col<-ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=GrassArea, group=Scenario, shape=Scenario, color=Scenario)) +
  geom_line(size=1.2) +# geom_point(size=2)+ geom_jitter(width=1, height=2)+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(1:11))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  labs(x="Year", y="Grassland area (ha)")
sg.col



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
sl
#Lsk Color:
sl.col<-ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=LskPop, group=Scenario, shape=Scenario, color=Scenario)) +
  geom_line(size=1.2) +# geom_point(size=2)+ geom_jitter(width=1, height=2)+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(1:11))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  labs(x="Year", y="Livestock Population EOY (s.u.)")
sl.col


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
sr

#Biomass color
sb.col<-ggplot(data=subset(Sukh_All, UF %in%c("0.65")), aes(x=Year, y=Biomass, group=Scenario, shape=Scenario, color=Scenario)) +
  geom_line(size=1.2) +# geom_point(size=2)+ geom_jitter(width=1, height=2)+
  guides(override.aes = list(size=12))+
  scale_shape_manual(values=c(1:11))+
  theme_bw()+
  theme(axis.text.x=element_text(size=11, angle=90))+
  theme(axis.text.y=element_text(size=11, vjust=-0.35))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(breaks=seq(1990,2050,5))+
  #theme(legend.position="none")+
  labs(x="Year", y="Remaining Biomass EOY (kg/ha)")
sb.col

setwd("./plots")
ggsave(sr, file="Suk_rmb.png", height=5, width=8,dpi=600)
ggsave(sl, file="Suk_lsk.png", height=5, width=8,dpi=600)
ggsave(sg, file="Suk_grass.png", height=5, width=8,dpi=600)

ggsave(sb.col, file="Suk_rmbColor.png", height=5, width=8,dpi=600)
ggsave(sl.col, file="Suk_lskColor.png", height=5, width=8,dpi=600)
ggsave(sg.col, file="Suk_grassColor.png", height=5, width=8,dpi=600)

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

sufg<- ggplot(data=SukhG, aes(x=Scenario, y=GrassArea, group=UF, color=UF))+
  geom_point(shape=19, size=10)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3" )) +
  theme(axis.text.x=element_text(size=22),
        axis.title.x=element_blank())+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Grass Area (ha)")+
  theme(legend.position="none")

#NEed to flip Lo&Hi still, plotting now as Hi/Med/Lo
#Also need to figure out how to change xlim 
#maybe a diff type of plot?
uf<- ggplot(data=SukhG, aes(x=UF, y=GrassArea, group=Scenario, color=Scenario))+
  geom_point(shape=19, size=3)+
  geom_line()+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Lo","Med","Hi")) +
  theme(axis.text.x=element_text(size=22),
        axis.title.x=element_text(face="bold", size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="Percent of the Population that is Urban in 2050", y="Grass Area (ha) in 2050")+
  theme(legend.position="none") 
  

sufl<- ggplot(data=SukhL, aes(x=Scenario, y=LskPop, group=UF, color=UF))+
  geom_point(shape=19, size=10)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3" )) +
  theme(axis.text.x=element_text(size=22),
        axis.title.x=element_blank())+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Livestock Pop EOY (su)")+
  theme(legend.position="none")

sufb<- ggplot(data=SukhB, aes(x=Scenario, y=Biomass, group=UF, color=UF))+
  geom_point(shape=19, size=10)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels=c("Base","1","2","3" )) +
  theme(axis.text.x=element_text(size=22),
        axis.title.x=element_blank())+ #axis.title.x=element_text(face="bold",size=16))+  #axis.text=element_text(size=)
  theme(axis.text.y=element_text(size=11, vjust=-0.35),
        axis.title.y=element_text(face="bold", size=16))+
  labs(x="SCENARIOS", y="Biomass EOY (kg/ha)")+
  theme(legend.position="none")


setwd("/nfs/gallington-data/Mongolia_SDM/plots")

ggsave(sufg, file="sUFg.png", height=5, width=6,dpi=600)
ggsave(sufl, file="sUFl.png", height=5, width=6,dpi=600)
ggsave(sufb, file="sUFb.png", height=5, width=6,dpi=600)

#sUFs<- multiplot(sufg, sufl, sufb, cols=1)
