##############
# Weekly Data Prep
##############

#####TODO
#UPDATE FANTASY SHARKS TO HIGH PRECISION
#http://www.fantasysharks.com/apps/bert/forecasts/projections.php?League=-1&Position=2&scoring=12&Segment=534&uid=4

###############
#Install all Libraries
###############
#install.packages(c("XML","seqinr","stringr","xtable","mclust","data.table","ggplot2","plyr"))

library("XML")
library("seqinr")
library("stringr")
library("xtable")
library("mclust")
library("ggplot2")
library("plyr")
library("data.table")

###############
# Load Functions
###############

#Set Week
week <- "8"
season <- "2015"

#Select a League Setting
#source(paste(getwd(),"/Scripts/Functions/DraftKings.R", sep=""))
source(paste(getwd(),"/Scripts/Functions/League Settings.R", sep=""))
source(paste(getwd(),"/Scripts/Functions/Directories.R", sep=""))


###############
# Projections
###############

source(paste(getwd(),"/Scripts/Projections/QB.R", sep=""), echo = T)
source(paste(getwd(),"/Scripts/Projections/RB.R", sep=""), echo = T)
source(paste(getwd(),"/Scripts/Projections/TE.R", sep=""), echo = T)
source(paste(getwd(),"/Scripts/Projections/WR.R", sep=""), echo = T)
#source(paste(getwd(),"/Scripts/Projections/IDP.R", sep=""), echo = T)
#source(paste(getwd(),"/Scripts/Projections/DST.R", sep=""), echo = T)
#source(paste(getwd(),"/Scripts/Projections/K.R", sep=""), echo = T)


#Value Gap Analysis
source(paste(getwd(),"/Scripts/Calculations/Value Gap Analysis.R", sep=""), echo = T)

