###########################
# File: FOX Sports Projections.R
# Description: Downloads Fantasy Football Projections from FOX Sports
# Date: 6/8/2014
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
suffix <- "fox"

#Download fantasy football projections from FOX Sports
qb1_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=1&position=8&split=3", stringsAsFactors = FALSE)
qb2_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=2&position=8&split=3", stringsAsFactors = FALSE)
rb1_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=1&position=16&split=3", stringsAsFactors = FALSE)
rb2_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=2&position=16&split=3", stringsAsFactors = FALSE)
rb3_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=3&position=16&split=3", stringsAsFactors = FALSE)
rb4_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=4&position=16&split=3", stringsAsFactors = FALSE)
wr1_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=1&position=1&split=3", stringsAsFactors = FALSE)
wr2_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=2&position=1&split=3", stringsAsFactors = FALSE)
wr3_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=3&position=1&split=3", stringsAsFactors = FALSE)
wr4_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=4&position=1&split=3", stringsAsFactors = FALSE)
wr5_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=5&position=1&split=3", stringsAsFactors = FALSE)
wr6_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=6&position=1&split=3", stringsAsFactors = FALSE)
te1_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=1&position=4&split=3", stringsAsFactors = FALSE)
te2_fox <- readHTMLTable("http://msn.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=2&position=4&split=3", stringsAsFactors = FALSE)


#Add variable names for each object
#Variable names
qbNames_fox <- c("player_fox","status_fox","passTds_fox","passYds_fox","passInt_fox","rushTds_fox","rushYds_fox","twoPts_fox","fumlRecTds_fox","fumbles_fox","pts_fox") #,"passSack_fox","rushAtt_fox"
rbNames_fox <- c("player_fox","status_fox","rushTds_fox","rushYds_fox","recTds_fox","recYds_fox","twoPts_fox","fumlRecTds_fox","fumbles_fox","pts_fox")
wrNames_fox <- c("player_fox","status_fox","recTds_fox","recYds_fox","rushTds_fox","rushYds_fox","twoPts_fox","fumlRecTds_fox","fumbles_fox","pts_fox") #c("player_fox","status_fox","recTds_fox","recYds_fox","rec_fox","rushTds_fox","rushYds_fox","rushAtt_fox","puntReturnTds_fox","puntReturnYds_fox","kickReturnTds_fox","kickReturnYds_fox","twoPts_fox","fumlRecTds_fox","fumbles_fox","pts_fox")
teNames_fox <- c("player_fox","status_fox","recTds_fox","recYds_fox","rushTds_fox","rushYds_fox","twoPts_fox","fumlRecTds_fox","fumbles_fox","pts_fox") #,"fumlRecTds_fox","fumbles_fox"

#Merge players within position
qb_fox <- rbind(qb1_fox,qb2_fox)
names(qb_fox) <- qbNames_fox
rb_fox <- rbind(rb1_fox,rb2_fox,rb3_fox,rb4_fox)
names(rb_fox) <- rbNames_fox
wr_fox <- rbind(wr1_fox,wr2_fox,wr3_fox,wr4_fox,wr5_fox,wr6_fox)
names(wr_fox) <- wrNames_fox
te_fox <- rbind(te1_fox,te2_fox)
names(te_fox) <- teNames_fox


#Add variable for player position
qb_fox$pos <- as.factor("QB")
rb_fox$pos <- as.factor("RB")
wr_fox$pos <- as.factor("WR")
te_fox$pos <- as.factor("TE")

#Merge across positions
projections_fox <- rbind.fill(qb_fox,rb_fox,wr_fox,te_fox)

#Add missing variables
projections_fox$passAtt_fox <- NA
projections_fox$passComp_fox <- NA
projections_fox$rushAtt_fox <- NA
projections_fox$rec_fox <- NA
projections_fox$returnTds_fox <- NA

#Convert variables from character strings to numeric
projections_fox[,c("passAtt_fox","passComp_fox","passTds_fox","passYds_fox","passInt_fox","rushAtt_fox","rushTds_fox","rushYds_fox","twoPts_fox","fumlRecTds_fox","fumbles_fox","pts_fox","recTds_fox","recYds_fox","rec_fox","returnTds_fox")] <- 
  convert.magic(projections_fox[,c("passAtt_fox","passComp_fox","passTds_fox","passYds_fox","passInt_fox","rushAtt_fox","rushTds_fox","rushYds_fox","twoPts_fox","fumlRecTds_fox","fumbles_fox","pts_fox","recTds_fox","recYds_fox","rec_fox","returnTds_fox")], "numeric")

#Player name and team
projections_fox$name_fox <- str_trim(sapply(str_split(projections_fox$player_fox, "\r\n"), "[", 1))
projections_fox$name <- nameMerge(projections_fox$name_fox)
projections_fox$team_fox <- toupper(str_trim(str_sub(projections_fox$player_fox, start=str_locate(projections_fox$player_fox, "\\(")[,1]+1, end=str_locate(projections_fox$player_fox, "\\)")[,1]-6)))

#Remove duplicate cases
projections_fox[projections_fox$name %in% projections_fox[duplicated(projections_fox$name),"name"],]

#Same name, different player
projections_fox <- projections_fox[-which(projections_fox$name=="RYANGRIFFIN" & projections_fox$pos=="QB"),]

#Same player, different position

#Rename players
projections_fox[projections_fox$name=="STEVIEJOHNSON", "name"] <- "STEVEJOHNSON"

#Calculate overall rank
projections_fox$overallRank_fox <- rank(-projections_fox$pts_fox, ties.method="min")

#Calculate Position Rank
projections_fox$positionRank_fox <- NA
projections_fox[which(projections_fox$pos == "QB"), "positionRank_fox"] <- rank(-projections_fox[which(projections_fox$pos == "QB"), "pts_fox"], ties.method="min")
projections_fox[which(projections_fox$pos == "RB"), "positionRank_fox"] <- rank(-projections_fox[which(projections_fox$pos == "RB"), "pts_fox"], ties.method="min")
projections_fox[which(projections_fox$pos == "WR"), "positionRank_fox"] <- rank(-projections_fox[which(projections_fox$pos == "WR"), "pts_fox"], ties.method="min")
projections_fox[which(projections_fox$pos == "TE"), "positionRank_fox"] <- rank(-projections_fox[which(projections_fox$pos == "TE"), "pts_fox"], ties.method="min")

#Order variables in data set
projections_fox$sourceName <- suffix
allVars_fox <- c(prefix, paste(sourceSpecific,suffix, sep="_"),paste(varNames, suffix, sep="_"))
keepVars_fox <- allVars_fox[allVars_fox %in% names(projections_fox)]
projections_fox <- projections_fox[,keepVars_fox]

#Order players by overall rank
projections_fox <- projections_fox[order(projections_fox$overallRank_fox),]
row.names(projections_fox) <- 1:dim(projections_fox)[1]

#Density Plot
ggplot(projections_fox, aes(x=pts_fox)) + geom_density(fill="blue", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FOX Projected Points")
ggsave(paste(getwd(),"/Figures/FOX projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fox, file = paste(getwd(), "/Data/FOX-Projections.RData", sep=""))
write.csv(projections_fox, file=paste(getwd(), "/Data/FOX-Projections.csv", sep=""), row.names=FALSE)

save(projections_fox, file = paste(getwd(), "/Data/Historical Projections/FOX-Projections-", season, ".RData", sep=""))
write.csv(projections_fox, file=paste(getwd(), "/Data/Historical Projections/FOX-Projections-", season, ".csv", sep=""), row.names=FALSE)
