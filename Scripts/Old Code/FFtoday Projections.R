###########################
# File: FFtoday Projections.R
# Description: Downloads Fantasy Football Projections from fftoday.com
# Date: 6/7/2014
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/Scripts/Functions/League Settings.R", sep=""))

#Suffix
suffix <- "fftoday"

#Download fantasy football projections from FFtoday.com
qb_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2015&PosID=10&LeagueID=26955", stringsAsFactors = FALSE)[11]$'NULL'
rb_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2015&PosID=20&LeagueID=26955", stringsAsFactors = FALSE)[11]$'NULL'
wr1_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2015&PosID=30&LeagueID=26955", stringsAsFactors = FALSE)[11]$'NULL'
wr2_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2015&PosID=30&LeagueID=26955&order_by=FFPts&sort_order=DESC&cur_page=1", stringsAsFactors = FALSE)[11]$'NULL'
te_fftoday <- readHTMLTable("http://www.fftoday.com/rankings/playerproj.php?Season=2015&PosID=40&LeagueID=26955", stringsAsFactors = FALSE)[11]$'NULL'

#Add variable names for each object
names(qb_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","passComp_fftoday","passAtt_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","points_fftoday")
names(rb_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","points_fftoday")
names(wr1_fftoday) <- names(wr2_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","points_fftoday")
names(te_fftoday) <- c("star_fftoday","player_fftoday","team_fftoday","bye_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","points_fftoday")

#Trim dimensions
qb_fftoday <- qb_fftoday[2:(dim(qb_fftoday)[1]-1),]
rb_fftoday <- rb_fftoday[2:(dim(rb_fftoday)[1]-1),]
wr1_fftoday <- wr1_fftoday[2:(dim(wr1_fftoday)[1]-1),]
wr2_fftoday <- wr2_fftoday[2:(dim(wr2_fftoday)[1]-1),]
te_fftoday <- te_fftoday[2:(dim(te_fftoday)[1]-1),]

#Merge within position
wr_fftoday <- rbind(wr1_fftoday,wr2_fftoday)

#Add variable for player position
qb_fftoday$pos <- as.factor("QB")
rb_fftoday$pos <- as.factor("RB")
wr_fftoday$pos <- as.factor("WR")
te_fftoday$pos <- as.factor("TE")

#Merge across positions
projections_fftoday <- rbind.fill(qb_fftoday, rb_fftoday, wr_fftoday, te_fftoday)

#Add missing variables
projections_fftoday$returnTds_fftoday <- 0
projections_fftoday$twopoints_fftoday <- 0
projections_fftoday$fumbles_fftoday <- 0

#Remove special characters(commas)
projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","points_fftoday","returnTds_fftoday","twopoints_fftoday","fumbles_fftoday")] <-
  apply(projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","points_fftoday","returnTds_fftoday","twopoints_fftoday","fumbles_fftoday")], 2, function(x) gsub("\\,", "", x))

#Convert variables from character strings to numeric
projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","points_fftoday","returnTds_fftoday","twopoints_fftoday","fumbles_fftoday")] <- 
  convert.magic(projections_fftoday[,c("passAtt_fftoday","passComp_fftoday","passYds_fftoday","passTds_fftoday","passInt_fftoday","rushAtt_fftoday","rushYds_fftoday","rushTds_fftoday","rec_fftoday","recYds_fftoday","recTds_fftoday","points_fftoday","returnTds_fftoday","twopoints_fftoday","fumbles_fftoday")], "numeric")

#Player name, position, and team
projections_fftoday$name_fftoday <- str_trim(str_sub(projections_fftoday$player, start=2))
projections_fftoday$name <- nameMerge(projections_fftoday$name_fftoday)
projections_fftoday$team <- projections_fftoday$team_fftoday


#Remove duplicate cases
projections_fftoday[projections_fftoday$name %in% projections_fftoday[duplicated(projections_fftoday$name),"name"],]
#projections_fftoday <- projections_fftoday[-which(projections_fftoday$name_fftoday=="Dexter McCluster" & projections_fftoday$pos=="RB"),]

#Rename players
projections_fftoday[projections_fftoday$name=="BENWATSON", "name"] <- "BENJAMINWATSON"

#Calculate overall rank
projections_fftoday$overallRank_fftoday <- rank(-projections_fftoday$points_fftoday, ties.method="min")

#Calculate Position Rank
projections_fftoday$positionRank_fftoday <- NA
projections_fftoday[which(projections_fftoday$pos == "QB"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "QB"), "points_fftoday"], ties.method="min")
projections_fftoday[which(projections_fftoday$pos == "RB"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "RB"), "points_fftoday"], ties.method="min")
projections_fftoday[which(projections_fftoday$pos == "WR"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "WR"), "points_fftoday"], ties.method="min")
projections_fftoday[which(projections_fftoday$pos == "TE"), "positionRank_fftoday"] <- rank(-projections_fftoday[which(projections_fftoday$pos == "TE"), "points_fftoday"], ties.method="min")

#Order variables in data set
projections_fftoday$sourceName <- suffix
allVars_fftoday <- c(prefix, paste(sourceSpecific,suffix, sep="_"),paste(varNames, suffix, sep="_"))
keepVars_fftoday <- allVars_fftoday[allVars_fftoday %in% names(projections_fftoday)]
projections_fftoday <- projections_fftoday[,keepVars_fftoday]


#Order players by overall rank
projections_fftoday <- projections_fftoday[order(projections_fftoday$overallRank_fftoday),]
row.names(projections_fftoday) <- 1:dim(projections_fftoday)[1]

#Density Plot
ggplot(projections_fftoday, aes(x=points_fftoday)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FFtoday Projected Points")
ggsave(paste(getwd(),"/Figures/FFtoday projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fftoday, file = paste(getwd(), "/Data/FFtoday-Projections.RData", sep=""))
write.csv(projections_fftoday, file=paste(getwd(), "/Data/FFtoday-Projections.csv", sep=""), row.names=FALSE)

save(projections_fftoday, file = paste(getwd(), "/Data/Historical Projections/FFtoday-Projections-", season, ".RData", sep=""))
write.csv(projections_fftoday, file=paste(getwd(), "/Data/Historical Projections/FFtoday-Projections-", season, ".csv", sep=""), row.names=FALSE)
