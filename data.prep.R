# This code aggregates all of the model outputs for each scenario
# and each variable and tidies them 
# All Xilingol outputs first, then Sukhbaatar
# SUKHBAATAR CODE STARTS AT line: ~250
setwd("./data") # I know this isn't legit to change this but i'm afraid i'll break something if I try to fix it now

library(dplyr)
library("tidyr")



#############################
#XILINGOL
#######################

# BASE Model run outputs for variables: Grass, Crop, 
# Livestock Pop at end of year and Remaining biomass at end of year

# Baseline-Grass

XBG<- read.csv("XglBaseGrass.csv", header=TRUE, check.names = FALSE)
colnames(XBG)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XBG$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)   #rename the UFs steps used
XBG["Scenario"]<-c("Baseline")  #Add a new column for Scenario name
XBG<-XBG[c(1,62,2:61)] #reorder the columns
XBGt<-tbl_df(XBG)  #convert to dplyr tbl class
XBGt<-gather(XBG, Year, GrassArea, 3:62)   #tidy the data

#Baseline-Crop
XBC<- read.csv("XglBaseCrop.csv", header=TRUE, check.names = FALSE)
colnames(XBC)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XBC$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XBC["Scenario"]<-c("Baseline")     #Add a new column for Scenario name
XBC<-XBC[c(1,62,2:61)]     #reorder the columns
XBCt<-tbl_df(XBC)      #convert to dplyr tbl class
XBCt<-gather(XBC, Year, CropArea, 3:62)     #tidy the data

#Baseline-Lsk
XBL<- read.csv("XglBaseLsk.csv", header=TRUE, check.names = FALSE)
colnames(XBL)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XBL$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XBL["Scenario"]<-c("Baseline")     #Add a new column for Scenario name
XBL<-XBL[c(1,62,2:61)]     #reorder the columns
XBLt<-tbl_df(XBL)      #convert to dplyr tbl class
XBLt<-gather(XBL, Year, LskPop, 3:62)     #tidy the data

#Baseline-Rmb
XBR<- read.csv("XglBaseRmb.csv", header=TRUE, check.names = FALSE)
colnames(XBR)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XBR$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XBR["Scenario"]<-c("Baseline")     #Add a new column for Scenario name
XBR<-XBR[c(1,62,2:61)]     #reorder the columns
XBRt<-tbl_df(XBR)      #convert to dplyr tbl class
XBRt<-gather(XBR, Year, Biomass, 3:62)     #tidy the data

#combine all baseline runs in to one df with a diff column for each var output

XBase<-cbind(XBGt, XBCt$CropArea, XBLt$LskPop, XBRt$Biomass)
colnames(XBase)[5:7]<-c("CropArea", "LskPop", "Biomass")


################################
#Scenario 1
################################
#Scenario 1-Grass

XS1G<- read.csv("XglS1Grass.csv", header=TRUE, check.names = FALSE)
colnames(XS1G)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS1G$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)   #rename the UFs steps used
XS1G["Scenario"]<-c("Scen1")  #Add a new column for Scenario name
XS1G<-XS1G[c(1,62,2:61)] #reorder the columns
XS1Gt<-tbl_df(XS1G)  #convert to dplyr tbl class
XS1Gt<-gather(XS1G, Year, GrassArea, 3:62)   #tidy the data

#Scen1-Crop
XS1C<- read.csv("XglS1Crop.csv", header=TRUE, check.names = FALSE)
colnames(XS1C)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS1C$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS1C["Scenario"]<-c("Scen1")     #Add a new column for Scenario name
XS1C<-XS1C[c(1,62,2:61)]     #reorder the columns
XS1Ct<-tbl_df(XS1C)      #convert to dplyr tbl class
XS1Ct<-gather(XS1C, Year, CropArea, 3:62)     #tidy the data

#Scen1-Lsk
XS1L<- read.csv("XglS1Lsk.csv", header=TRUE, check.names = FALSE)
colnames(XS1L)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS1L$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS1L["Scenario"]<-c("Scen1")     #Add a new column for Scenario name
XS1L<-XS1L[c(1,62,2:61)]     #reorder the columns
XS1Lt<-tbl_df(XS1L)      #convert to dplyr tbl class
XS1Lt<-gather(XS1L, Year, LskPop, 3:62)     #tidy the data

#Scen1-Rmb
XS1R<- read.csv("XglS1Rmb.csv", header=TRUE, check.names = FALSE)
colnames(XS1R)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS1R$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS1R["Scenario"]<-c("Scen1")     #Add a new column for Scenario name
XS1R<-XS1R[c(1,62,2:61)]     #reorder the columns
XS1Rt<-tbl_df(XS1R)      #convert to dplyr tbl class
XS1Rt<-gather(XS1R, Year, Biomass, 3:62)     #tidy the data

#combine all Scen1 runs in to one df with a diff column for each var output

XS1<-cbind(XS1Gt, XS1Ct$CropArea, XS1Lt$LskPop, XS1Rt$Biomass)
colnames(XS1)[5:7]<-c("CropArea", "LskPop", "Biomass")

#############################################
#Scenario 2
#############################################
#Scenario 2-Grass

XS2G<- read.csv("XglS2Grass.csv", header=TRUE, check.names = FALSE)
colnames(XS2G)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS2G$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)   #rename the UFs steps used
XS2G["Scenario"]<-c("Scen2")  #Add a new column for Scenario name
XS2G<-XS2G[c(1,62,2:61)] #reorder the columns
XS2Gt<-tbl_df(XS2G)  #convert to dplyr tbl class
XS2Gt<-gather(XS2G, Year, GrassArea, 3:62)   #tidy the data

#Scen2-Crop
XS2C<- read.csv("XglS2Crop.csv", header=TRUE, check.names = FALSE)
colnames(XS2C)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS2C$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS2C["Scenario"]<-c("Scen2")     #Add a new column for Scenario name
XS2C<-XS2C[c(1,62,2:61)]     #reorder the columns
XS2Ct<-tbl_df(XS2C)      #convert to dplyr tbl class
XS2Ct<-gather(XS2C, Year, CropArea, 3:62)     #tidy the data

#Scen2-Lsk
XS2L<- read.csv("XglS2Lsk.csv", header=TRUE, check.names = FALSE)
colnames(XS2L)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS2L$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS2L["Scenario"]<-c("Scen2")     #Add a new column for Scenario name
XS2L<-XS2L[c(1,62,2:61)]     #reorder the columns
XS2Lt<-tbl_df(XS2L)      #convert to dplyr tbl class
XS2Lt<-gather(XS2L, Year, LskPop, 3:62)     #tidy the data

#Scen2-Rmb
XS2R<- read.csv("XglS2Rmb.csv", header=TRUE, check.names = FALSE)
colnames(XS2R)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS2R$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS2R["Scenario"]<-c("Scen2")     #Add a new column for Scenario name
XS2R<-XS2R[c(1,62,2:61)]     #reorder the columns
XS2Rt<-tbl_df(XS2R)      #convert to dplyr tbl class
XS2Rt<-gather(XS2R, Year, Biomass, 3:62)     #tidy the data

#combine all Scen2 runs in to one df with a diff column for each var output

XS2<-cbind(XS2Gt, XS2Ct$CropArea, XS2Lt$LskPop, XS2Rt$Biomass)
colnames(XS2)[5:7]<-c("CropArea", "LskPop", "Biomass")

#############################################
#Scenario 3
#############################################
#Scenario3-Grass

XS3G<- read.csv("XglS3Grass.csv", header=TRUE, check.names = FALSE)
colnames(XS3G)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS3G$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)   #rename the UFs steps used
XS3G["Scenario"]<-c("Scen3")  #Add a new column for Scenario name
XS3G<-XS3G[c(1,62,2:61)] #reorder the columns
XS3Gt<-tbl_df(XS3G)  #convert to dplyr tbl class
XS3Gt<-gather(XS3G, Year, GrassArea, 3:62)   #tidy the data

#Scen3-Crop
XS3C<- read.csv("XglS3Crop.csv", header=TRUE, check.names = FALSE)
colnames(XS3C)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS3C$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS3C["Scenario"]<-c("Scen3")     #Add a new column for Scenario name
XS3C<-XS3C[c(1,62,2:61)]     #reorder the columns
XS3Ct<-tbl_df(XS3C)      #convert to dplyr tbl class
XS3Ct<-gather(XS3C, Year, CropArea, 3:62)     #tidy the data

#Scen3-Lsk
XS3L<- read.csv("XglS3Lsk.csv", header=TRUE, check.names = FALSE)
colnames(XS3L)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS3L$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS3L["Scenario"]<-c("Scen3")     #Add a new column for Scenario name
XS3L<-XS3L[c(1,62,2:61)]     #reorder the columns
XS3Lt<-tbl_df(XS3L)      #convert to dplyr tbl class
XS3Lt<-gather(XS3L, Year, LskPop, 3:62)     #tidy the data

#Scen3-Rmb
XS3R<- read.csv("XglS3Rmb.csv", header=TRUE, check.names = FALSE)
colnames(XS3R)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS3R$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS3R["Scenario"]<-c("Scen3")     #Add a new column for Scenario name
XS3R<-XS3R[c(1,62,2:61)]     #reorder the columns
XS3Rt<-tbl_df(XS3R)      #convert to dplyr tbl class
XS3Rt<-gather(XS3R, Year, Biomass, 3:62)     #tidy the data

#combine all Scen3 runs in to one df with a diff column for each var output

XS3<-cbind(XS3Gt, XS3Ct$CropArea, XS3Lt$LskPop, XS3Rt$Biomass)
colnames(XS3)[5:7]<-c("CropArea", "LskPop", "Biomass")

######################################################
#SCENARIO 4
################################################
#~~~~~~~~~~~~~Need to update from 0.5 to 0.6 once rerun model for Scen 4 with new code and import data 
#Scenario4-Grass

XS4G<- read.csv("XglS4Grass.csv", header=TRUE, check.names = FALSE)
colnames(XS4G)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS4G$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)   #rename the UFs steps used
XS4G["Scenario"]<-c("Scen4")  #Add a new column for Scenario name
XS4G<-XS4G[c(1,62,2:61)] #reorder the columns
XS4Gt<-tbl_df(XS4G)  #convert to dplyr tbl class
XS4Gt<-gather(XS4G, Year, GrassArea, 3:62)   #tidy the data

#Scen3-Crop
XS4C<- read.csv("XglS4Crop.csv", header=TRUE, check.names = FALSE)
colnames(XS4C)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS4C$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS4C["Scenario"]<-c("Scen4")     #Add a new column for Scenario name
XS4C<-XS4C[c(1,62,2:61)]     #reorder the columns
XS4Ct<-tbl_df(XS4C)      #convert to dplyr tbl class
XS4Ct<-gather(XS4C, Year, CropArea, 3:62)     #tidy the data

#Scen3-Lsk
XS4L<- read.csv("XglS4Lsk.csv", header=TRUE, check.names = FALSE)
colnames(XS4L)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS4L$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS4L["Scenario"]<-c("Scen4")     #Add a new column for Scenario name
XS4L<-XS4L[c(1,62,2:61)]     #reorder the columns
XS4Lt<-tbl_df(XS4L)      #convert to dplyr tbl class
XS4Lt<-gather(XS4L, Year, LskPop, 3:62)     #tidy the data

#Scen3-Rmb
XS4R<- read.csv("XglS4Rmb.csv", header=TRUE, check.names = FALSE)
colnames(XS4R)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
XS4R$UF<- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)  #rename the UFs steps used
XS4R["Scenario"]<-c("Scen4")     #Add a new column for Scenario name
XS4R<-XS4R[c(1,62,2:61)]     #reorder the columns
XS4Rt<-tbl_df(XS4R)      #convert to dplyr tbl class
XS4Rt<-gather(XS4R, Year, Biomass, 3:62)     #tidy the data

#combine all Scen3 runs in to one df with a diff column for each var output

XS4<-cbind(XS4Gt, XS4Ct$CropArea, XS4Lt$LskPop, XS4Rt$Biomass)
colnames(XS4)[5:7]<-c("CropArea", "LskPop", "Biomass")

#########################################################
#   combine Base plus all Scenario Runs in to one df ####
#########################################################

Xgl_All<-bind_rows(XBase, XS1, XS2, XS3, XS4)

# This is what wil be called in future steps

Xgl_All

#
#
#
# Now MONGOLIA:
#
#
#
#############################~################
#
#
#~~~~~~~~~~~~~~~~~SUKHBAATAR~~~~~~~~~~~~~
#
#
###########################################

# CAUTION!! If import new runs make sure Hi/Med/Lo are labelled appropriately!
# Old version of this code had levels labelled backwards!


# BASE Model run outputs for variables: 
# Grass, Livestock Pop at end of year and Remaining biomass at end of year

# In the run on 4/25 I just included the median UF scenario Max Urban %=0.65 
# as the baseline setting so there is just one projection for each var rather than
# three for each as with Xilingol above. 
# Change code to match that above if need to show all UF scenarios for each var

# Baseline- 

SukhBase <- read.csv("SukBase.csv", header=TRUE, check.names = FALSE)
SukhBase<- subset(SukhBase, select= -c(Var))
# already did this next step in Excel for this one
# colnames(XBG)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukhBase$UF<- c(0.65, 0.65, 0.65)   #rename the UFs steps used
SukhBase["Scenario"]<-c("Baseline")  #Add a new column for Scenario name. This becomes column #64
SukhBase<-SukhBase[c(1,63,2:62)] #reorder the columns
# Baserun Grass
SukhBaseGrass<- SukhBase[1,]
SukhBaseGrass<-tbl_df(SukhBaseGrass)  #convert to dplyr tbl class
SukBGrasst<-gather(SukhBaseGrass, Year, GrassArea, 3:63)   #tidy the data
# Baserun Lsk
SukhBaseLsk<-SukhBase[2,]
SukhBaseLsk<-tbl_df(SukhBaseLsk)  #convert to dplyr tbl class
SukBLskt<-gather(SukhBaseLsk, Year, LskPop, 3:63)   #tidy the data
# Baserun Biomass
SukhBaseRmb<-SukhBase[3,]
SukhBaseRmb<-tbl_df(SukhBaseRmb)  #convert to dplyr tbl class
SukBRmbt<-gather(SukhBaseRmb, Year, Biomass, 3:63)   #tidy the data
SukBRmbt
SukB<-cbind(SukBGrasst, SukBLskt$LskPop, SukBRmbt$Biomass)
colnames(SukB)[5:6]<- c("LskPop", "Biomass")

################################
# Scenario 1
################################
# Scenario 1-Grass

SukS1G<- read.csv("SukS1Grass.csv", header=TRUE, check.names = FALSE)
colnames(SukS1G)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS1G$UF<- c(0.75, 0.65, 0.55)   #rename the UFs steps used
SukS1G["Scenario"]<-c("Scen1")  #Add a new column for Scenario name
SukS1G<-SukS1G[c(1,63,2:62)] #reorder the columns
SukS1Gt<-tbl_df(SukS1G)  #convert to dplyr tbl class
SukS1Gt<-gather(SukS1G, Year, GrassArea, 3:63)   #tidy the data

# Scen1-Lsk
SukS1L<- read.csv("SukS1Lsk.csv", header=TRUE, check.names = FALSE)
colnames(SukS1L)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS1L$UF<- c(0.75, 0.65, 0.55)  #rename the UFs steps used
SukS1L["Scenario"]<-c("Scen1")     #Add a new column for Scenario name
SukS1L<-SukS1L[c(1,63,2:62)]     #reorder the columns
SukS1Lt<-tbl_df(SukS1L)      #convert to dplyr tbl class
SukS1Lt<-gather(SukS1L, Year, LskPop, 3:63)     #tidy the data

# Scen1-Rmb
SukS1B<- read.csv("SukS1Rmb.csv", header=TRUE, check.names = FALSE)
colnames(SukS1B)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS1B$UF<- c(0.75, 0.65, 0.55)  #rename the UFs steps used
SukS1B["Scenario"]<-c("Scen1")     #Add a new column for Scenario name
SukS1B<-SukS1B[c(1,63,2:62)]     #reorder the columns
SukS1Bt<-tbl_df(SukS1B)      #convert to dplyr tbl class
SukS1Bt<-gather(SukS1B, Year, Biomass, 3:63)     #tidy the data

# combine all Scen1 runs in to one df with a diff column for each var output

SukS1<-cbind(SukS1Gt, SukS1Lt$LskPop, SukS1Bt$Biomass)
colnames(SukS1)[5:6]<-c("LskPop", "Biomass")


################################
# Scenario 2
################################
# Scenario 2-Grass

SukS2G<- read.csv("SukS2Grass.csv", header=TRUE, check.names = FALSE)
colnames(SukS2G)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS2G$UF<- c(0.75, 0.65, 0.55)   #rename the UFs steps used
SukS2G["Scenario"]<-c("Scen2")  #Add a new column for Scenario name
SukS2G<-SukS2G[c(1,63,2:62)] #reorder the columns
SukS2Gt<-tbl_df(SukS2G)  #convert to dplyr tbl class
SukS2Gt<-gather(SukS2G, Year, GrassArea, 3:63)   #tidy the data

# Scen2-Lsk
SukS2L<- read.csv("SukS2Lsk.csv", header=TRUE, check.names = FALSE)
colnames(SukS2L)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS2L$UF<- c(0.75, 0.65, 0.55)  #rename the UFs steps used
SukS2L["Scenario"]<-c("Scen2")     #Add a new column for Scenario name
SukS2L<-SukS2L[c(1,63,2:62)]     #reorder the columns
SukS2Lt<-tbl_df(SukS2L)      #convert to dplyr tbl class
SukS2Lt<-gather(SukS2L, Year, LskPop, 3:63)     #tidy the data

# Scen2-Rmb
SukS2B<- read.csv("SukS2Rmb.csv", header=TRUE, check.names = FALSE)
colnames(SukS2B)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS2B$UF<- c(0.75, 0.65, 0.55)  #rename the UFs steps used
SukS2B["Scenario"]<-c("Scen2")     #Add a new column for Scenario name
SukS2B<-SukS2B[c(1,63,2:62)]     #reorder the columns
SukS2Bt<-tbl_df(SukS2B)      #convert to dplyr tbl class
SukS2Bt<-gather(SukS2B, Year, Biomass, 3:63)     #tidy the data

# combine all Scen1 runs in to one df with a diff column for each var output

SukS2<-cbind(SukS2Gt, SukS2Lt$LskPop, SukS2Bt$Biomass)
colnames(SukS2)[5:6]<-c("LskPop", "Biomass")

################################
# Scenario 3
################################
# Scenario 3-Grass

SukS3G<- read.csv("SukS3Grass.csv", header=TRUE, check.names = FALSE)
colnames(SukS3G)[1]<-"UF"      #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS3G$UF<- c(0.75, 0.65, 0.55)   #rename the UFs steps used
SukS3G["Scenario"]<-c("Scen3")  #Add a new column for Scenario name
SukS3G<-SukS3G[c(1,63,2:62)] #reorder the columns
SukS3Gt<-tbl_df(SukS3G)  #convert to dplyr tbl class
SukS3Gt<-gather(SukS3G, Year, GrassArea, 3:63)   #tidy the data

# Scen3-Lsk
SukS3L<- read.csv("SukS3Lsk.csv", header=TRUE, check.names = FALSE)
colnames(SukS3L)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS3L$UF<- c(0.75, 0.65, 0.55)  #rename the UFs steps used
SukS3L["Scenario"]<-c("Scen3")     #Add a new column for Scenario name
SukS3L<-SukS3L[c(1,63,2:62)]     #reorder the columns
SukS3Lt<-tbl_df(SukS3L)      #convert to dplyr tbl class
SukS3Lt<-gather(SukS3L, Year, LskPop, 3:63)     #tidy the data

# Scen3-Rmb
SukS3B<- read.csv("SukS3Rmb.csv", header=TRUE, check.names = FALSE)
colnames(SukS3B)[1]<-"UF"    #rename Urbanization Fraction Saturation Pt (Max Urban Pct)
SukS3B$UF<- c(0.75, 0.65, 0.55)  #rename the UFs steps used
SukS3B["Scenario"]<-c("Scen3")     #Add a new column for Scenario name
SukS3B<-SukS3B[c(1,63,2:62)]     #reorder the columns
SukS3Bt<-tbl_df(SukS3B)      #convert to dplyr tbl class
SukS3Bt<-gather(SukS3B, Year, Biomass, 3:63)     #tidy the data

# combine all Scen1 runs in to one df with a diff column for each var output

SukS3<-cbind(SukS3Gt, SukS3Lt$LskPop, SukS3Bt$Biomass)
colnames(SukS3)[5:6]<-c("LskPop", "Biomass")

#########################################################
#   combine Base plus all Scenario Runs in to one df ####
#########################################################

Sukh_All<-bind_rows(SukB, SukS1, SukS2, SukS3)

## inspect:
head(Sukh_All)
tail(Sukh_All)
nrow(Sukh_All)