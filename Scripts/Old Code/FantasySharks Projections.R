###########################
# File: FantasySharks Projections.R
# Description: Downloads Fantasy Football Projections from FantasySharks.com
# Date: 5/26/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/Scripts/Functions/League Settings.R", sep=""))

#Suffix
suffix <- "fs"

#Download fantasy football projections from FantasySharks.com
projections_fs <- read.csv("http://www.fantasysharks.com/apps/Projections/SeasonProjections.php?pos=ALL&format=csv&l=1", stringsAsFactors = FALSE)

#Player position
projections_fs$pos <- as.factor(projections_fs$Pos)

#Keep only QB, RB, WR, TE
projections_fs <- projections_fs[which(projections_fs$pos %in% c("QB","RB","WR","TE")),]

#Player names
projections_fs$Name <- as.character(gsub(",","",projections_fs$Name))
projections_fs$Last <- str_sub(projections_fs$Name,end=str_locate(string=projections_fs$Name, ' ')[,1]+0)
projections_fs$First <- str_sub(projections_fs$Name,start =str_locate(string=projections_fs$Name, ' ')[,1]+0)


projections_fs$name_fs <- paste(projections_fs$First, projections_fs$Last, sep = " ")

projections_fs$name <- nameMerge(projections_fs$name_fs)

#Team
projections_fs$team_fs <- as.character(projections_fs$Team)
projections_fs$team <- projections_fs$team_fs


#Variables
projections_fs$passComp_fs <- projections_fs$Comp
projections_fs$passYds_fs <- projections_fs$Yards
projections_fs$passTds_fs <- projections_fs$TD
projections_fs$passInt_fs <- projections_fs$Int
projections_fs$rushAtt_fs <- projections_fs$Att
projections_fs$rushYds_fs <- projections_fs$Yards.1
projections_fs$rushTds_fs <- projections_fs$TD.1
projections_fs$rec_fs <- projections_fs$Rec
projections_fs$recYds_fs <- projections_fs$Yards.2
projections_fs$recTds_fs <- projections_fs$TD.2
projections_fs$fumbles_fs <- projections_fs$Fum
projections_fs$returnTds_fs <- 0
projections_fs$twoPts_fs <- 0
projections_fs$points_fs <- projections_fs$Fantasy.Points

#Convert to numeric
projections_fs[,c("passComp_fs","passYds_fs","passTds_fs","passInt_fs","rushAtt_fs","rushYds_fs","rushTds_fs","rec_fs","recYds_fs","recTds_fs","returnTds_fs","fumbles_fs","twoPts_fs","points_fs")] <-
  convert.magic(projections_fs[,c("passComp_fs","passYds_fs","passTds_fs","passInt_fs","rushAtt_fs","rushYds_fs","rushTds_fs","rec_fs","recYds_fs","recTds_fs","returnTds_fs","fumbles_fs","twoPts_fs","points_fs")], "numeric")

#Remove duplicate cases
projections_fs[projections_fs$name %in% projections_fs[duplicated(projections_fs$name),"name"],]

#Calculate overall rank
projections_fs$overallRank_fs <- rank(-projections_fs$points_fs, ties.method="min")

#Calculate Position Rank
projections_fs$positionRank_fs <- NA
projections_fs[which(projections_fs$pos == "QB"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "QB"), "points_fs"], ties.method="min")
projections_fs[which(projections_fs$pos == "RB"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "RB"), "points_fs"], ties.method="min")
projections_fs[which(projections_fs$pos == "WR"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "WR"), "points_fs"], ties.method="min")
projections_fs[which(projections_fs$pos == "TE"), "positionRank_fs"] <- rank(-projections_fs[which(projections_fs$pos == "TE"), "points_fs"], ties.method="min")

#Order variables in data set
projections_fs$sourceName <- suffix
allVars_fs <- c(prefix, paste(sourceSpecific,suffix, sep="_"),paste(varNames, suffix, sep="_"))
keepVars_fs <- allVars_fs[allVars_fs %in% names(projections_fs)]
projections_fs <- projections_fs[,keepVars_fs]

#Order players by overall rank
projections_fs <- projections_fs[order(projections_fs$overallRank_fs),]
row.names(projections_fs) <- 1:dim(projections_fs)[1]

#Density Plot
ggplot(projections_fs, aes(x=points_fs)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasySharks Projected Points")
ggsave(paste(getwd(),"/Figures/FantasySharks projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fs, file = paste(getwd(), "/Data/FantasySharks-Projections.RData", sep=""))
write.csv(projections_fs, file=paste(getwd(), "/Data/FantasySharks-Projections.csv", sep=""), row.names=FALSE)

save(projections_fs, file = paste(getwd(), "/Data/Historical Projections/FantasySharks-Projections-", season, ".RData", sep=""))
write.csv(projections_fs, file=paste(getwd(), "/Data/Historical Projections/FantasySharks-Projections-", season, ".csv", sep=""), row.names=FALSE)
