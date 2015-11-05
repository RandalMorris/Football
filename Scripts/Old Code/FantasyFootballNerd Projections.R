###########################
# File: FantasyFootballNerd Projections.R
# Description: Downloads Fantasy Football Projections from FantasyFootballNerd.com
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
suffix <- "ffn"

#Download fantasy football projections from FantasyFootballNerd.com
qb_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections", stringsAsFactors = FALSE)$projections
rb_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/RB", stringsAsFactors = FALSE)$projections
wr_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/WR", stringsAsFactors = FALSE)$projections
te_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/TE", stringsAsFactors = FALSE)$projections
kickers_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/K", stringsAsFactors = FALSE)$projections
dst_ffn <- readHTMLTable("http://www.fantasyfootballnerd.com/fantasy-football-projections/DEF", stringsAsFactors = FALSE)$projections

#Add variable names for each object
names(qb_ffn) <- c("name_ffn","team_ffn","passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn","points_ffn")
names(rb_ffn) <- c("name_ffn","team_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","fumbles_ffn","points_ffn")
names(wr_ffn) <- c("name_ffn","team_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn","points_ffn")
names(te_ffn) <- c("name_ffn","team_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","rushAtt_ffn","rushYds_ffn","rushTds_ffn","fumbles_ffn","points_ffn")
names(kickers_ffn) <- c("name_ffn","team_ffn","xp_ffn","fg_ffn","points_ffn")
names(dst_ffn) <- c("name_ffn","dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn","points_ffn")

#Add variable for player position
qb_ffn$pos <- as.factor("QB")
rb_ffn$pos <- as.factor("RB")
wr_ffn$pos <- as.factor("WR")
te_ffn$pos <- as.factor("TE")
kickers_ffn$pos <- as.factor("K")
dst_ffn$pos <- as.factor("DST")

#Merge players across positions
projections_ffn <- rbind.fill(qb_ffn, rb_ffn, wr_ffn, te_ffn, kickers_ffn, dst_ffn)
projections_ffn$team <- projections_ffn$team_ffn

#Make all NA's
projections_ffn[is.na(projections_ffn)] <- 0

#Add variables from other projection sources
projections_ffn$returnTds_ffn <- 0
projections_ffn$twopts_ffn <- 0
projections_ffn$passIncomp_ffn <- 0
projections_ffn$dstBlk_ffn<- 0


#Remove special characters (percentage sign)
projections_ffn$passCompPct_ffn <- str_sub(projections_ffn$passCompPct_ffn, end=str_locate(string=projections_ffn$passCompPct_ffn, '%')[,1]-1)

#Convert variables from character strings to numeric
projections_ffn[,c("passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","returnTds_ffn","twopts_ffn","fumbles_ffn","points_ffn",
                   "xp_ffn","fg_ffn",
                   "dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn","passIncomp_ffn","dstBlk_ffn")] <-
  convert.magic(projections_ffn[,c("passComp_ffn","passAtt_ffn","passCompPct_ffn","passYds_ffn","passTds_ffn","passInt_ffn","rushAtt_ffn","rushYds_ffn","rushYpc_ffn","rushTds_ffn","rec_ffn","recYds_ffn","recTds_ffn","recYpc_ffn","returnTds_ffn","twopts_ffn","fumbles_ffn","points_ffn",
                                   "xp_ffn","fg_ffn",
                                   "dstSack_ffn","dstInt_ffn","dstFumlRec_ffn","dstDefTd_ffn","dstStTd_ffn","passIncomp_ffn","dstBlk_ffn")], "numeric")

#Name for merging
projections_ffn$name <- nameMerge(projections_ffn$name_ffn)

#Remove duplicate cases
projections_ffn[projections_ffn$name %in% projections_ffn[duplicated(projections_ffn$name),"name"],]

#Pass Incomplete
projections_ffn$passIncomp_ffn <- (projections_ffn$passAtt_ffn - projections_ffn$passComp_ffn)


#Calculate overall rank
projections_ffn$overallRank_ffn <- rank(-projections_ffn$points_ffn, ties.method="min")

#Calculate Position Rank
projections_ffn$positionRank_ffn <- NA
projections_ffn[which(projections_ffn$pos == "QB"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "QB"), "points_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "RB"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "RB"), "points_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "WR"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "WR"), "points_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "TE"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "TE"), "points_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "K"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "K"), "points_ffn"], ties.method="min")
projections_ffn[which(projections_ffn$pos == "DST"), "positionRank_ffn"] <- rank(-projections_ffn[which(projections_ffn$pos == "DST"), "points_ffn"], ties.method="min")

#Order variables in data set
projections_ffn$sourceName <- suffix
allVars_ffn <- c(prefix, paste(sourceSpecific,suffix, sep="_"),paste(varNames, suffix, sep="_"))
keepVars_ffn <- allVars_ffn[allVars_ffn %in% names(projections_ffn)]
projections_ffn <- projections_ffn[,keepVars_ffn]



#Order players by overall rank
projections_ffn <- projections_ffn[order(projections_ffn$overallRank_ffn),]
row.names(projections_ffn) <- 1:dim(projections_ffn)[1]


#Density Plot
ggplot(projections_ffn, aes(x=points_ffn)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasyFootballNerd Projected Points")
ggsave(paste(getwd(),"/Figures/FantasyFootballNerd projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_ffn, file = paste(getwd(), "/Data/FantasyFootballNerd-Projections.RData", sep=""))
write.csv(projections_ffn, file=paste(getwd(), "/Data/FantasyFootballNerd-Projections.csv", sep=""), row.names=FALSE)

save(projections_ffn, file = paste(getwd(), "/Data/Historical Projections/FantasyFootballNerd-Projections-", season, ".RData", sep=""))
write.csv(projections_ffn, file=paste(getwd(), "/Data/Historical Projections/FantasyFootballNerd-Projections-", season, ".csv", sep=""), row.names=FALSE)

