###########################
# File: FantasyPros Projections.R
# Description: Downloads Fantasy Football Projections from FantasyPros.com
# Date: 3/3/2013
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
suffix <- "fp"

#Download fantasy football projections from FantasyPros.com
qb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/qb.php", stringsAsFactors = FALSE)$data
rb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/rb.php", stringsAsFactors = FALSE)$data
wr_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/wr.php", stringsAsFactors = FALSE)$data
te_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/te.php", stringsAsFactors = FALSE)$data
k_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/k.php", stringsAsFactors = FALSE)$data

#Add variable names for each object
names(qb_fp) <- c("player_fp","passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp","rushYds_fp","rushTds_fp","fumbles_fp","points_fp")
names(rb_fp) <- c("player_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","points_fp")
names(wr_fp) <- c("player_fp","rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","points_fp")
names(te_fp) <- c("player_fp","rec_fp","recYds_fp","recTds_fp","fumbles_fp","points_fp")
names(k_fp) <- c("player_fp","fg_fp","fga_fp","xp_fp","points_fp")


#Add variable for player position
qb_fp$pos <- as.factor("QB")
rb_fp$pos <- as.factor("RB")
wr_fp$pos <- as.factor("WR")
te_fp$pos <- as.factor("TE")
k_fp$pos <- as.factor("K")

#Merge players across positions
projections_fp <- rbind.fill(qb_fp, rb_fp, wr_fp, te_fp, k_fp)

#Make all NA's
projections_fp[is.na(projections_fp)] <- 0

#Add variables from other projection sources
projections_fp$returnTds_fp <- 0
projections_fp$twoPts_fp <- 0
projections_fp$passIncomp_fp <- 0
projections_fp$dstBlk_fp <- 0

#Remove special characters(commas)
projections_fp$passYds_fp <- as.numeric(gsub(",","",projections_fp$passYds_fp))

#Convert variables from character strings to numeric
projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp","rushAtt_fp",
                  "rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","returnTds_fp","twoPts_fp","fumbles_fp","points_fp",
                  "fg_fp","fga_fp","xp_fp","passIncomp_fp","dstBlk_fp")] <-
  convert.magic(projections_fp[,c("passAtt_fp","passComp_fp","passYds_fp","passTds_fp","passInt_fp",
                                  "rushAtt_fp","rushYds_fp","rushTds_fp","rec_fp","recYds_fp","recTds_fp","returnTds_fp","twoPts_fp","fumbles_fp","points_fp",
                                  "fg_fp","fga_fp","xp_fp","passIncomp_fp","dstBlk_fp")], "numeric")


#Player names
projections_fp$name_fp <- str_sub(projections_fp$player_fp, end= -4)

#Name for merging
projections_fp$name <- nameMerge(projections_fp$name_fp)

#Player teams
projections_fp$team_fp <- str_trim(str_sub(projections_fp$player_fp, start= -3))
projections_fp$team <- projections_fp$team_fp


#Remove rows with all NAs
projections_fp <- projections_fp[apply(projections_fp, 1, function(x) any(!is.na(x))),]

#Remove rows with missing player name
if(length(which(projections_fp$name_fp == "")) > 0){
  projections_fp <- projections_fp[-which(projections_fp$name_fp == ""),]
}

#Remove duplicate cases
projections_fp[projections_fp$name %in% projections_fp[duplicated(projections_fp$name),"name"],]

#Calculate overall rank
projections_fp$overallRank_fp <- rank(-projections_fp$points_fp, ties.method="min")

#Calculate Position Rank
projections_fp$positionRank_fp <- NA
projections_fp[which(projections_fp$pos == "QB"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "QB"), "points_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "RB"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "RB"), "points_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "WR"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "WR"), "points_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "TE"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "TE"), "points_fp"], ties.method="min")
projections_fp[which(projections_fp$pos == "K"), "positionRank_fp"] <- rank(-projections_fp[which(projections_fp$pos == "K"), "points_fp"], ties.method="min")

#Order variables in data set
projections_fp$sourceName <- suffix
allVars_fp <- c(prefix, paste(sourceSpecific,suffix, sep="_"),paste(varNames, suffix, sep="_"))
keepVars_fp <- allVars_fp[allVars_fp %in% names(projections_fp)]
projections_fp <- projections_fp[,keepVars_fp]



#Order players by overall rank
projections_fp <- projections_fp[order(projections_fp$overallRank_fp),]
row.names(projections_fp) <- 1:dim(projections_fp)[1]

#Density Plot
ggplot(projections_fp, aes(x=points_fp)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasyPros Projected Points")
ggsave(paste(getwd(),"/Figures/FantasyPros projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fp, file = paste(getwd(), "/Data/FantasyPros-Projections.RData", sep=""))
write.csv(projections_fp, file=paste(getwd(), "/Data/FantasyPros-Projections.csv", sep=""), row.names=FALSE)

save(projections_fp, file = paste(getwd(), "/Data/Historical Projections/FantasyPros-Projections-", season, ".RData", sep=""))
write.csv(projections_fp, file=paste(getwd(), "/Data/Historical Projections/FantasyPros-Projections-", season, ".csv", sep=""), row.names=FALSE)
