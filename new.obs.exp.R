setwd("../data")
m.oe<- read.csv("mg.obsexp.csv", header=TRUE)

mtidy<-m.oe %>%gather("Source", "Population", 2:5, na.rm=TRUE)
msep<- separate(mtidy, col=Source, into=c("OE", "Type")) 


mloe<-
  filter(msep, Type=="Lsk")%>%
  ggplot(aes(Year, Population, group=OE))+
  geom_line(aes(linetype=OE), size=1)+      #Set line type by obs/exp
  #geom_point()+
  expand_limits(y=0) +                      #Set y axis to start at 0
   scale_linetype_discrete(name="",
                      breaks=c("Base", "Obs"),
                      labels=c("Predicted", "Observed"))+
  theme(legend.title=element_blank())+       # to remove legend title
   #xlab("Year") + 
  ylab("Livestock Population (su) *10^6") +
  theme_bw()+
  #theme(legend.position=c(.9, .2))     #to put the legend inside the plot
  theme(legend.position="none")         #to remove the legend
ggsave(mloe, file="../plots/m.lsk.oe.ts.png", height=3, width=3, dpi=300)

mhoe<-
  filter(msep, Type=="Pop")%>%
  ggplot(aes(Year, Population, group=OE))+
  geom_line(aes(linetype=OE), size=1)+      #Set line type by obs/exp
  #geom_point()+
  expand_limits(y=c(45,60)) +                      #Set y axis to start at 0
  scale_linetype_discrete(name="",
                          breaks=c("Base", "Obs"),
                          labels=c("Predicted", "Observed"))+
  theme(legend.title=element_blank())+       # to remove legend title
  #xlab("Year") + 
  ylab("Human Population *10^3") +
  theme_bw()+
  #theme(legend.position=c(.9, .2))     
  theme(legend.position="none")

ggsave(mhoe, file="../plots/m.pop.oe.ts.png", height=3, width=3, dpi=300)


################IMAR###################

x.oe<- read.csv("xgl.obsexp.csv", header=TRUE)

xtidy<-x.oe %>%gather("Source", "Population", 2:5, na.rm=TRUE)
xsep<- separate(xtidy, col=Source, into=c("OE", "Type")) 

#Xilingol Livestock:
xloe<-
  filter(xsep, Type=="Lsk")%>%
  ggplot(aes(Year, Population, group=OE))+
  geom_line(aes(linetype=OE), size=1)+      #Set line type by obs/exp
  #geom_point()+
  expand_limits(y=0) +                      #Set y axis to start at 0
  scale_linetype_discrete(name="",
                          breaks=c("Base", "Obs"),
                          labels=c("Predicted", "Observed"))+
  theme(legend.title=element_blank())+       # to remove legend title
  #xlab("Year") + 
  ylab("Livestock Population (su) *10^6") +
  theme_bw()+
  #theme(legend.position=c(.9, .2))     #to put the legend inside the plot
  theme(legend.position="none")         #to remove the legend
ggsave(xloe, file="../plots/x.lsk.oe.ts.png", height=3, width=3, dpi=300)

#Xilingol Human Pop:
xhoe<-
  filter(xsep, Type=="Pop")%>%
  ggplot(aes(Year, Population, group=OE))+
  geom_line(aes(linetype=OE), size=1)+      #Set line type by obs/exp
  #geom_point()+
  expand_limits(y=c(0.8, 1.2)) +                      #Set y axis to start at 0
  scale_linetype_discrete(name="",
                          breaks=c("Base", "Obs"),
                          labels=c("Predicted", "Observed"))+
  theme(legend.title=element_blank())+       # to remove legend title
  #xlab("Year") + 
  ylab("Human Population *10^6") +
  theme_bw()+
  #theme(legend.position=c(.9, .2))     
  theme(legend.position="none")

ggsave(xhoe, file="../plots/x.pop.oe.ts.png", height=3, width=3, dpi=300)
