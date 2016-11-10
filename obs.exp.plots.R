#code to plot Observed v Expected plots for SDM manuscript

#old code in figures.R and MGfigures.R which plotted timeseries of both.
#this plots them against each other w a lm line

#code to plot the data from the regression on top of figure
#(not using this now but keeping it here just in case need it later)
#
#
#__ORIGINAL function to plot the lm with the coefficients:____________________________________________________________
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(Base.Lsk~Obs.Lsk, data=m.oe))
###########___________________________________________
#
#
#
#MODIFIED:
#10/21 bc of time limit just changing this next chunk of code for each plot and exporting. 
#need to edit original function so that it will pull in a predefined "axis title" var to populate.
#______________________________________________________________
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    theme_bw()+
    theme(axis.text.x=element_text(size=12, vjust=-0.35))+
    theme(axis.text.y=element_text(size=12, vjust=-0.35))+
    theme(axis.title=element_text(size=14))+
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                       "Intercept =",signif(fit$coef[[1]],3 ),
                       " Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3)),
         x="Observed Lsk Population (10^6 sheep units)", y="Predicted Lsk Population (10^6 sheep units)")
}

ggplotRegression(lm(Base.Lsk~Obs.Lsk, data=m.oe))
###########___________________________________________
setwd("./plots")
ggsave(moe.l, file="MG lsk ob.exp.png", height=5, width=5, dpi=600)








#ORIGINAL PLOTTING CODE :

library(ggplot2)

#Sukhbaatar, MONGOLIA
setwd("./data")
m.oe<- read.csv("mg.obsexp.csv", header=TRUE)






moe.l<- ggplot(m.oe, aes(Obs.Lsk, Base.Lsk))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, vjust=-0.35))+
  theme(axis.text.y=element_text(size=12, vjust=-0.35))+
  theme(axis.title=element_text(size=14))+
  labs(x="Observed Lsk Population (10^6 sheep units)", y="Predicted Lsk Population (10^6 sheep units)")

setwd("U:/GitHub/Mongolia/Figs/MG")
ggsave(moe.l, file="MG lsk ob.exp.png", height=5, width=5, dpi=600)


moe.p<- ggplot(mg.pop.oe, aes(Observed, Baserun))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, vjust=-0.35))+
  theme(axis.text.y=element_text(size=12, vjust=-0.35))+
  theme(axis.title=element_text(size=14))+
  labs(x="Observed Population (10^3)", y="Predicted Population (10^3)")
ggsave(moe.p, file="MG pop ob.exp.png", height=5, width=5, dpi=600)



#XILINGOL IMAR #
#pull the observed out:
setwd("./data")
liv<- read.csv("livestock.csv", header=TRUE, na.string="NA")
liv.oe<- liv[1:18,] #pulls just the years w observed data
tbl_df(liv.oe)
liv.oe<-liv.oe%>%
  filter(Year>=1991)%>%
  select(Year, Observed)

#get the baseline lsk data
#use XBLt from data.prep.R (XilingolBaseLsktidy)
XBLt
#subseting just lsk baseline
oe.xgl<- Xgl_All%>%
  filter(UF==0.70)%>%
  filter(Year<=2007)%>%
  filter(Scenario%in%c("Baseline"))%>%
  select(Year, LskPop)

#this join pulls in both Year columns
#need to fix that
  Xgl.lsk.oe<-bind_cols(oe.xgl, liv.oe)
#Xgl.lsk.oe<- Xgl.lsk.oe[,1-2,4]
 
#check the plot
ggplotRegression(lm(LskPop~Observed, data=Xgl.lsk.oe))

#mutate
Xgl.lsk.oe%>%mutate(Lsk.Short=(LskPop/100000))%>%mutate(Obs.Short=(Observed/100000))

#plot it for pub:
xg.oe.l<- ggplot(Xgl.lsk.oe, aes(Observed, LskPop))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, vjust=-0.35))+
  theme(axis.text.y=element_text(size=12, vjust=-0.35))+
  theme(axis.title=element_text(size=14))+
  labs(x="Observed Lsk Population (10^6 sheep units)", y="Predicted Lsk Population (10^6 sheep units)")
xg.oe.l
setwd("U:/GitHub/Mongolia/Figs/IMAR")
ggsave(xg.oe.l, file="Xgl lsk ob.exp.png", height=5, width=5, dpi=600)

#xg population

setwd("U:/GitHub/Mongolia/data/obs_exp")
xg.pop<-read.csv("xg.pop.obsexp.csv", header=TRUE, na.string="NA")
tbl_df(xg.pop)

fitxpop<-lm(Observed~Predicted.Value, data=xg.pop)
summary(fitxpop)

xoe.p<- ggplot(xg.pop, aes(Observed, Predicted.Value))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, vjust=-0.35))+
  theme(axis.text.y=element_text(size=12, vjust=-0.35))+
  theme(axis.title=element_text(size=14))+
  labs(x="Observed Population (10^3)", y="Predicted Population (10^3)")
  #annotate("text", x = 900, y = 1050, label = "Rsq=0.95")
xoe.p
setwd("U:/GitHub/Mongolia/Figs/IMAR")

ggsave(xoe.p, file="Xgl pop ob.exp.png", height=5, width=5, dpi=600)
