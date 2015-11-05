#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")

#Functions
source(paste(getwd(),"/Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/Scripts/Functions/League Settings.R", sep=""))

#Projection Info
year <- 2015
suffix <- "cbs"

#Download fantasy football projections from cbssports.com
qb_cbs <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/QB/season/avg/ppr?&print_rows=9999", stringsAsFactors = FALSE)[7]$'NULL'
rb_cbs <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/season/avg/ppr?&print_rows=9999", stringsAsFactors = FALSE)[7]$'NULL'
wr_cbs <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/season/avg/ppr?&print_rows=9999", stringsAsFactors = FALSE)[7]$'NULL'
te_cbs <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/TE/season/avg/ppr?&print_rows=9999", stringsAsFactors = FALSE)[7]$'NULL'
kickers_cbs <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/K/season/avg/ppr", stringsAsFactors = FALSE)[7]$'NULL'
dst_cbs <- readHTMLTable("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/DST/season/avg/ppr", stringsAsFactors = FALSE)[7]$'NULL'

#Add variable names for each object
names(qb_cbs) <- c("player_cbs","passAtt_cbs","passComp_cbs","passYds_cbs","passTds_cbs","passInt_cbs","passCompPct_cbs","passYdsPerAtt_cbs","rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs","rushTds_cbs","fumbles_cbs","points_cbs")
names(rb_cbs) <- c("player_cbs","rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs","rushTds_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs","fumbles_cbs","points_cbs")
names(wr_cbs) <- c("player_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs","fumbles_cbs","points_cbs")
names(te_cbs) <- c("player_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs","fumbles_cbs","points_cbs")
names(kickers_cbs) <- c("player_cbs","fg_cbs","fga_cbs","xp_cbs","points_cbs")
names(dst_cbs) <- c("player_cbs","dstInt_cbs","dstFumlRec_cbs","dstFumlForce_cbs","dstSack_cbs","dstTd_cbs","dstSafety_cbs","dstPtsAllowed_cbs","dstYdsAllowed_cbs","points_cbs")


#Trim dimensions
qb_cbs <- qb_cbs[3:(dim(qb_cbs)[1]-1),]
rb_cbs <- rb_cbs[3:(dim(rb_cbs)[1]-1),]
wr_cbs <- wr_cbs[3:(dim(wr_cbs)[1]-1),]
te_cbs <- te_cbs[3:(dim(te_cbs)[1]-1),]
kickers_cbs <- kickers_cbs[2:(dim(kickers_cbs)[1]-1),]
dst_cbs <- dst_cbs[2:(dim(dst_cbs)[1]-1),]


#Add variable for player position
qb_cbs$pos <- as.factor("QB")
rb_cbs$pos <- as.factor("RB")
wr_cbs$pos <- as.factor("WR")
te_cbs$pos <- as.factor("TE")
kickers_cbs$pos <- as.factor("K")
dst_cbs$pos <- as.factor("DST")

#Merge across positions
projections_cbs <- rbind.fill(qb_cbs, rb_cbs, wr_cbs, te_cbs, kickers_cbs, dst_cbs)

#Make all NA's
projections_cbs[is.na(projections_cbs)] <- 0

#Add variables from other projection sources
projections_cbs$returnTds_cbs <- 0
projections_cbs$twoPts_cbs <- 0
projections_cbs$passIncomp_cbs <- 0
projections_cbs$dstBlk_cbs <- 0


#Convert variables from character strings to numeric
projections_cbs[,c("passAtt_cbs","passComp_cbs","passYds_cbs","passTds_cbs","passInt_cbs",
                    "passCompPct_cbs","passYdsPerAtt_cbs","rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs",
                    "rushTds_cbs","fumbles_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs",
                    "fg_cbs","fga_cbs","xp_cbs","dstInt_cbs","dstFumlRec_cbs","dstFumlForce_cbs","dstSack_cbs","dstTd_cbs","dstSafety_cbs",
                    "dstPtsAllowed_cbs","dstYdsAllowed_cbs","points_cbs","passIncomp_cbs","dstBlk_cbs")] <- 
  convert.magic(projections_cbs[,c("passAtt_cbs","passComp_cbs","passYds_cbs","passTds_cbs","passInt_cbs",
                                    "passCompPct_cbs","passYdsPerAtt_cbs","rushAtt_cbs","rushYds_cbs","rushYdsPerAtt_cbs",
                                    "rushTds_cbs","fumbles_cbs","rec_cbs","recYds_cbs","recYdsPerRec_cbs","recTds_cbs",
                                    "fg_cbs","fga_cbs","xp_cbs","dstInt_cbs","dstFumlRec_cbs","dstFumlForce_cbs","dstSack_cbs","dstTd_cbs","dstSafety_cbs",
                                    "dstPtsAllowed_cbs","dstYdsAllowed_cbs","points_cbs","passIncomp_cbs","dstBlk_cbs")], "numeric")

#Player names
projections_cbs$name_cbs <- str_sub(projections_cbs$player, end=str_locate(string=projections_cbs$player, ',')[,1]-1)
projections_cbs$name <- nameMerge(projections_cbs$name_cbs)

#Remove Duplicates
projections_cbs[projections_cbs$name %in% projections_cbs[duplicated(projections_cbs$name),"name"],]
#projections_cbs[projections_cbs$name_cbs == "David Johnson","pos"] <- "TE"

#Rename Players
#projections_cbs[projections_cbs$name=="TIMOTHYWRIGHT", "name"] <- "TIMWRIGHT"

#Player teams
projections_cbs$team_cbs <- str_trim(str_sub(projections_cbs$player, start= -3))
projections_cbs$team <- projections_cbs$team_cbs
#Calculate overall rank
projections_cbs$overallRank_cbs <- rank(-projections_cbs$points_cbs, ties.method="min")

#Calculate Position Rank
projections_cbs$positionRank_cbs <- NA
projections_cbs[which(projections_cbs$pos == "QB"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "QB"), "points_cbs"], ties.method="min")
projections_cbs[which(projections_cbs$pos == "RB"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "RB"), "points_cbs"], ties.method="min")
projections_cbs[which(projections_cbs$pos == "WR"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "WR"), "points_cbs"], ties.method="min")
projections_cbs[which(projections_cbs$pos == "TE"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "TE"), "points_cbs"], ties.method="min")
projections_cbs[which(projections_cbs$pos == "K"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "K"), "points_cbs"], ties.method="min")
projections_cbs[which(projections_cbs$pos == "DST"), "positionRank_cbs"] <- rank(-projections_cbs[which(projections_cbs$pos == "DST"), "points_cbs"], ties.method="min")

#Calculate pass Attempts
projections_cbs$passIncomp_cbs <- (projections_cbs$passAtt_cbs - projections_cbs$passComp_cbs)


#Order variables in data set
projections_cbs$sourceName <- suffix
allVars_cbs <- c(prefix, paste(sourceSpecific,suffix, sep="_"),paste(varNames, suffix, sep="_"))
keepVars_cbs <- allVars_cbs[allVars_cbs %in% names(projections_cbs)]
projections_cbs <- projections_cbs[,keepVars_cbs]


#Order players by overall rank
projections_cbs <- projections_cbs[order(projections_cbs$overallRank_cbs),]
row.names(projections_cbs) <- 1:dim(projections_cbs)[1]

#Density Plot
ggplot(projections_cbs, aes(x=points_cbs)) + geom_density(fill="red", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of cbs Projected Points from")
ggsave(paste(getwd(),"/Figures/cbs projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_cbs, file = paste(getwd(),"/Data/cbs-Projections.RData", sep=""))
write.csv(projections_cbs, file=paste(getwd(),"/Data/cbs-Projections.csv", sep=""), row.names=FALSE)

save(projections_cbs, file = paste(getwd(),"/Data/Historical Projections/cbs-Projections-2015.RData", sep=""))
write.csv(projections_cbs, file=paste(getwd(),"/Data/Historical Projections/cbs-Projections-2015.csv", sep=""), row.names=FALSE)

