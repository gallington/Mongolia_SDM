#12/5 added new code to label subplots and create multiplot and export at high res

library(gridExtra)
library(ggplot2)

#Plotting observed v predicted values.

#setwd("../data")

#this code has Mongolia first for some reason, don't get confused.

#------------
#Sukhbaatar, Mongolia
#----------------
m.oe<- read.csv("mg.obsexp.csv", header=TRUE)

mtidy<-m.oe %>%gather("Source", "Population", 2:5, na.rm=TRUE)
msep<- separate(mtidy, col=Source, into=c("OE", "Type")) 

#livestock pop
mloe<-
    filter(msep, Type=="Lsk")%>%
    ggplot(aes(Year, Population, group=OE))+
    geom_line(aes(linetype=OE), size=1)+      #Set line type by obs/exp
    expand_limits(y=0) +                      #Set y axis to start at 0
     scale_linetype_discrete(name="",
                        breaks=c("Base", "Obs"),
                        labels=c("Predicted", "Observed"))+
    theme(legend.title=element_blank())+       # to remove legend title
     #xlab("Year") + 
    labs(y=expression(Livestock~Population~(su) %*% 10^6)) +
    theme_bw()+
    theme(plot.title=element_text(hjust=0,             #to left jutify to plot title/subplot label
                                  face='bold')) +
  annotate("text", x=2010, y=0.5,
           label=paste("R^2==",0.71),
           size=4, parse = TRUE)+
  annotate("text", x = 2010, y = 0.1,
           label = paste("p<",0.001), size = 4, parse = TRUE)+
  theme(legend.position="none")+                 #to remove the legend
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#ggsave(mloe, file="../plots/m.lsk.oe.ts.png", height=3, width=3, dpi=300)

#human pop
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
  labs(y=expression(Human~Population %*% 10^3)) +
  theme_bw()+
  theme(plot.title=element_text(hjust=0, 
                                face='bold'))+
  annotate("text", x=2010, y=47,
           label=paste("R^2==",0.86),
           size=4, parse = TRUE)+
  annotate("text", x = 2010, y = 46,
           label = paste("p<",0.001), size = 4, parse = TRUE)+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#ggsave(mhoe, file="../plots/m.pop.oe.ts.png", height=3, width=3, dpi=300)


################IMAR###################
#------------
# Xilingol, Inner Mongolia, China
#-------------------
x.oe<- read.csv("xgl.obsexp.csv", header=TRUE)

xtidy<-x.oe %>%gather("Source", "Population", 2:5, na.rm=TRUE)
xsep<- separate(xtidy, col=Source, into=c("OE", "Type")) 


#Xilingol Livestock:
xloe<-
  filter(xsep, Type=="Lsk")%>%
  ggplot(aes(Year, Population/10^6, group=OE))+
  geom_line(aes(linetype=OE), size=1)+      #Set line type by obs/exp
  #geom_point()+
  expand_limits(y=0) +                      #Set y axis to start at 0
  scale_linetype_discrete(name="",
                          breaks=c("Base", "Obs"),
                          labels=c("Predicted", "Observed"))+
  theme(legend.title=element_blank())+       # to remove legend title
  #xlab("Year") + 
  labs(y=expression(Livestock~Population (su) %*% 10^6)) +
  theme_bw()+
  theme(legend.position=c(.9, .2),                   #to put the legend inside the plot
        plot.title=element_text(hjust=0,             #to left jutify to plot title/subplot label
                                face='bold')) +
  annotate("text", x=2005, y=2,
           label=paste("R^2==",0.76),
           size=4, parse = TRUE)+
  annotate("text", x = 2005, y = 1,
           label = paste("p<",0.001), size = 4, parse = TRUE)+
  theme(legend.position="none")   +                  #to remove the legend
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# ggsave(xloe, file="../plots/x.lsk.oe.ts.png", height=3, width=3, dpi=300)

#Xilingol Human Pop:
xhoe<-
  filter(xsep, Type=="Pop")%>%
  ggplot(aes(Year, Population/10^6, group=OE))+
  geom_line(aes(linetype=OE), size=1)+      #Set line type by obs/exp
  expand_limits(y=c(0.8, 1.2)) +                      #Set y axis to start at 0.8
  scale_linetype_discrete(name="",
                          breaks=c("Base", "Obs"),
                          labels=c("Predicted", "Observed"))+
  theme(legend.title=element_blank())+       # to remove legend title
  labs(y=expression(Human~Population%*% 10^6)) +
  labs(x= "Year") +
  theme_bw()+
  annotate("text", x=2010, y=0.85,
           label=paste("R^2==",0.86),
           size=4, parse = TRUE)+
  annotate("text", x = 2010, y = 0.8,
           label = paste("p<",0.001), size = 4, parse = TRUE)+
  theme(legend.justification=c(0,1), legend.position=c(0.05,0.9))+
  theme(legend.background = element_rect(fill=NULL))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# ggsave(xhoe, file="../plots/x.pop.oe.ts.png", height=3, width=3, dpi=300)

# grid.arrange(xhoe, xloe, mhoe, mloe, ncol=2)

#
#to plot all together with teh legend on the bottom, 
#use function code from: 
#http://rpubs.com/sjackman/grid_arrange_shared_legend
#
library(ggplot2)
library(gridExtra)
library(grid)

#function to combine plots and put legend at bottom:
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}
#combine plots
# grid_arrange_shared_legend(xhoe, xloe, mhoe, mloe, ncol=2, nrow=2)
#assign to object
# combo<- grid_arrange_shared_legend(xhoe, xloe, mhoe, mloe, ncol=2, nrow=2) #this won't save to ggsave
#export
# ggsave(combo, file="./plots/combined_plot2.png", height=6, width=6, dpi=300)

#this doesn't use the call to put legend at bottom
#g<- arrangeGrob(xhoe, xloe, mhoe, mloe, ncol=2) 

############################################

# using COWPLOT

# draw_plot(plot, x = 0, y = 0, width = 1, height = 1)
# plot: the plot to place (ggplot2 or a gtable)
# x: The x location of the lower left corner of the plot.
# y: The y location of the lower left corner of the plot.
# width, height: the width and the height of the plot
# 
# The function ggdraw() is used to initialize an empty drawing canvas.
# install.packages("cowplot")
library(cowplot)
figure3 <- ggdraw() +
  draw_plot(xhoe, x = 0, y = .5, width = 0.5, height = 0.5) +
  draw_plot(xloe, x = .5, y = .5, width = 0.5, height = 0.5) +
  draw_plot(mhoe, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(mloe, x = .5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(c("a)", "b)", "c)", "d)"), 
                  c(0, 0.5, 0, 0.5), c(1,1, 0.5, 0.5), size = 15)
save_plot("../plots/figure3.png", figure3, ncol = 2, nrow = 2, base_aspect_ratio = 1.3)


# install.packages("extrafont")
# 
# library(extrafont)
# font_import
# font_install("fontcm")
# loadfonts()




