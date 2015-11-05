#Weekly Projections

#DATA SETTINGS ####

#URLS
url_cbs <- paste("http://www.cbssports.com/fantasy/football/stats/weeklyprojections/WR/", week, "/avg/ppr?&print_rows=9999", sep="")
url_espn <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=4&leagueId=",espn_std,sep="")
url_espn1 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=4&leagueId=",espn_std,"&startIndex=40",sep="")
url_espn2 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=4&leagueId=",espn_std,"&startIndex=80",sep="")
url_fftoday <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=30,&LeagueID=", fft_espnSTD, sep="")
url_fftoday1 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=30,&LeagueID=", fft_espnSTD,"&order_by=FFPts&sort_order=DESC&cur_page=1", sep="")
url_fantasy_pro_ranks <- ("http://www.fantasypros.com/nfl/rankings/ppr-wr.php")
url_fantasy_pro <- ("http://www.fantasypros.com/nfl/projections/wr.php")
url_numfire <- paste("http://www.numberfire.com/nfl/fantasy/fantasy-football-ppr-projections/wr")

##STANDARIZATION TIME##

#CBS ####
##Create data from urls
cbs_html <- htmlParse(url_cbs)
cbs_html <- readHTMLTable(cbs_html)
cbs_wrproj <- do.call(rbind.data.frame, cbs_html)
rm(cbs_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
cbs_wrproj <- subset(cbs_wrproj, select = c(1:3,5:7))
#clean headers
cbs_wrproj <- cbs_wrproj[3:(dim(cbs_wrproj)[1]-1),]
#name the columns
names(cbs_wrproj) <- c("player","cbs_recpts","cbs_yd","cbs_td","cbs_fl","cbs_fpts")

#convert to Character so we dont remove the Decimal later
cbs_wrproj$cbs_recpts <- as.character(cbs_wrproj$cbs_recpts)
cbs_wrproj$cbs_yd <- as.character(cbs_wrproj$cbs_yd)
cbs_wrproj$cbs_td <- as.character(cbs_wrproj$cbs_td)
cbs_wrproj$cbs_fl <- as.character(cbs_wrproj$cbs_fl)
cbs_wrproj$cbs_fpts <- as.character(cbs_wrproj$cbs_fpts)

#convert to numeric
cbs_wrproj$cbs_recpts <- as.numeric(cbs_wrproj$cbs_recpts)
cbs_wrproj$cbs_yd <- as.numeric(cbs_wrproj$cbs_yd)
cbs_wrproj$cbs_td <- as.numeric(cbs_wrproj$cbs_td)
cbs_wrproj$cbs_fl <- as.numeric(cbs_wrproj$cbs_fl)
cbs_wrproj$cbs_fpts <- as.numeric(cbs_wrproj$cbs_fpts)

#Seprate names and team
cbs_wrproj$name <- str_sub(cbs_wrproj$player, end=str_locate(string=cbs_wrproj$player, ',')[,1]-1)
cbs_wrproj$cbs_team <- str_trim(str_sub(cbs_wrproj$player, start= -3))

#Fix names to match rest of sources
cbs_wrproj[cbs_wrproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."

#Calculate points based on League Settings
cbs_wrproj$cbs_fpts <- (cbs_wrproj$cbs_yd*recYdsMultiplier) + (cbs_wrproj$cbs_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  cbs_wrproj$cbs_fpts <- ifelse(cbs_wrproj$cbs_yd >= RecydBonus,cbs_wrproj$cbs_fpts + Rydbonus, cbs_wrproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_wrproj$cbs_fpts <- ifelse(cbs_wrproj$cbs_yd >= RecydBonus,cbs_wrproj$cbs_fpts + Rydbonus, cbs_wrproj$cbs_fpts)
}

#Add in TD probability
cbs_wrproj$cbs_td <- round(cbs_wrproj$cbs_td, 0)
cbs_wrproj$cbs_fpts <- ifelse(cbs_wrproj$cbs_td >= 1,cbs_wrproj$cbs_fpts + (cbs_wrproj$cbs_td*recTdsMultiplier), cbs_wrproj$cbs_fpts)


#Remove all insginicant projections
cbs_wrproj1 <- subset(cbs_wrproj, cbs_wrproj$cbs_yd > 0)
cbs_wrproj <- cbs_wrproj1
rm(cbs_wrproj1)

#resort data and reorder by name
cbs_wrproj <- subset(cbs_wrproj, select = c("name","cbs_team","cbs_recpts","cbs_yd","cbs_td","cbs_fl","cbs_fpts"))
cbs_wrproj <- cbs_wrproj[order(cbs_wrproj$name),]

#ESPN #####

##Create data from urls
espn_wrproj <- readHTMLTable(url_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
espn_wrproj1 <- readHTMLTable(url_espn1, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
espn_wrproj2 <- readHTMLTable(url_espn2, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
espn_wrproj <- subset(espn_wrproj, select = c(1,11:14))
espn_wrproj1 <-  subset(espn_wrproj1, select = c(1,11:14))
espn_wrproj2 <-  subset(espn_wrproj2, select = c(1,11:14))
#clean headers
espn_wrproj <- espn_wrproj[2:(dim(espn_wrproj)[1]),]
espn_wrproj1 <- espn_wrproj1[2:(dim(espn_wrproj1)[1]),]
espn_wrproj2 <- espn_wrproj2[2:(dim(espn_wrproj2)[1]),]
#name the columns
names(espn_wrproj) <- names(espn_wrproj1) <- names(espn_wrproj2) <- c("player","espn_recpts","espn_yd","espn_td","espn_fpts")

#Merge and remove other dataframes to reduce clutter
espn_wrproj <- rbind(espn_wrproj, espn_wrproj1, espn_wrproj2)
rm(espn_wrproj1, espn_wrproj2)

#convert to numeric
espn_wrproj$espn_recpts <- as.numeric(espn_wrproj$espn_recpts)
espn_wrproj$espn_yd <- as.numeric(espn_wrproj$espn_yd)
espn_wrproj$espn_td <- gsub("--", "0",espn_wrproj$espn_td)
espn_wrproj$espn_td <- as.numeric(espn_wrproj$espn_td)
espn_wrproj$espn_fpts <- as.numeric(espn_wrproj$espn_fpts)

#Seperate names and team
espn_wrproj$name <- str_sub(espn_wrproj$player, end=str_locate(string=espn_wrproj$player, ',')[,1]-1)
#espn_wrproj$name <- str_replace_all(espn_wrproj$name, "\\*", "")
espn_wrproj$espn_team <- str_sub(espn_wrproj$player, start=str_locate(string=espn_wrproj$player, ',')[,1]+1)
espn_wrproj$espn_team <- str_to_upper(str_sub(espn_wrproj$espn_team, end=str_locate(string=espn_wrproj$espn_team, ' ')[,1]+3))

#Fix names to match rest of sources
espn_wrproj[espn_wrproj$name=="Stevie Johnson", "name"] <- "Steve Johnson"
espn_wrproj[espn_wrproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
espn_wrproj$espn_fpts <- (espn_wrproj$espn_yd*recYdsMultiplier) + (espn_wrproj$espn_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  espn_wrproj$espn_fpts <- ifelse(espn_wrproj$espn_yd >= RecydBonus,espn_wrproj$espn_fpts + Rydbonus, espn_wrproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_wrproj$espn_fpts <- ifelse(espn_wrproj$espn_yd >= RecydBonus,espn_wrproj$espn_fpts + Rydbonus, espn_wrproj$espn_fpts)
}

#Add in TD probability
espn_wrproj$espn_td <- round(espn_wrproj$espn_td, 0)
espn_wrproj$espn_fpts <- ifelse(espn_wrproj$espn_td >= 1,espn_wrproj$espn_fpts + (espn_wrproj$espn_td*recTdsMultiplier), espn_wrproj$espn_fpts)

#Remove all insginicant projections
espn_wrproj1 <- subset(espn_wrproj, espn_wrproj$espn_yd > 0)
espn_wrproj <- espn_wrproj1
rm(espn_wrproj1)

#resort and reorder by name
espn_wrproj <- subset(espn_wrproj, select = c("name","espn_team","espn_recpts","espn_yd","espn_td","espn_fpts"))
espn_wrproj <- espn_wrproj[order(espn_wrproj$name),]

#FANTASY SHARKS ####

##Create data from urls

ffs_wrproj <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=wr&l=12&format=csv")

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
ffs_wrproj <- subset(ffs_wrproj, select = c(3,4,7:10))

#name the columns
names(ffs_wrproj) <- c("player","ffs_team","ffs_recpts","ffs_yd","ffs_td","ffs_fpts")

#convert to numeric
ffs_wrproj$ffs_recpts <- as.numeric(ffs_wrproj$ffs_recpts)
ffs_wrproj$ffs_yd <- as.numeric(ffs_wrproj$ffs_yd)
ffs_wrproj$ffs_td <- as.numeric(ffs_wrproj$ffs_td)
ffs_wrproj$ffs_fpts <- as.numeric(ffs_wrproj$ffs_fpts)

#Add Recpts for PPR
ffs_wrproj$ffs_fpts <- ffs_wrproj$ffs_fpts + ffs_wrproj$ffs_recpts

#convert name and team to Char
ffs_wrproj$player <- as.character(ffs_wrproj$player)
ffs_wrproj$ffs_team <- as.character(ffs_wrproj$ffs_team)

#Seprate names and  fix 2 Char teams
ffs_wrproj$last <- str_sub(ffs_wrproj$player, end=str_locate(string=ffs_wrproj$player, ' ')[,1]-2)
ffs_wrproj$first <- str_sub(ffs_wrproj$player, start=str_locate(string=ffs_wrproj$player,' ')[,1]+1)
ffs_wrproj$name <- paste(ffs_wrproj$first, ffs_wrproj$last)
ffs_wrproj[ffs_wrproj$ffs_team=="NOS", "ffs_team"] <- "NO"
ffs_wrproj[ffs_wrproj$ffs_team=="KCC", "ffs_team"] <- "KC"
ffs_wrproj[ffs_wrproj$ffs_team=="GBP", "ffs_team"] <- "GB"
ffs_wrproj[ffs_wrproj$ffs_team=="SDC", "ffs_team"] <- "SD"
ffs_wrproj[ffs_wrproj$ffs_team=="SFF", "ffs_team"] <- "SF"
ffs_wrproj[ffs_wrproj$ffs_team=="TBB", "ffs_team"] <- "TB"

#Fix names to match rest of sources

#Calculate points based on League Settings
ffs_wrproj$ffs_fpts <- (ffs_wrproj$ffs_yd*recYdsMultiplier) + (ffs_wrproj$ffs_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  ffs_wrproj$ffs_fpts <- ifelse(ffs_wrproj$ffs_yd >= RecydBonus,ffs_wrproj$ffs_fpts + Rydbonus, ffs_wrproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_wrproj$ffs_fpts <- ifelse(ffs_wrproj$ffs_yd >= RecydBonus,ffs_wrproj$ffs_fpts + Rydbonus, ffs_wrproj$ffs_fpts)
}

#Add in TD probability
ffs_wrproj$ffs_td <- round(ffs_wrproj$ffs_td, 0)
ffs_wrproj$ffs_fpts <- ifelse(ffs_wrproj$ffs_td >= 1,ffs_wrproj$ffs_fpts + (ffs_wrproj$ffs_td*recTdsMultiplier), ffs_wrproj$ffs_fpts)

#Remove all insginicant projections
ffs_wrproj2 <- subset(ffs_wrproj, ffs_wrproj$ffs_yd > 0)
ffs_wrproj <- ffs_wrproj2
rm(ffs_wrproj2)

#resort data and reorder by name
ffs_wrproj <- subset(ffs_wrproj, select = c("name","ffs_team","ffs_recpts","ffs_yd","ffs_td","ffs_fpts"))
ffs_wrproj <- ffs_wrproj[order(ffs_wrproj$name),]

#FANTASYPRO ####

#Create data from urls
data_ranks <- readHTMLTable(url_fantasy_pro_ranks, as.data.frame=TRUE, stringsAsFactors=FALSE)$data
fp_wrproj <- readHTMLTable(url_fantasy_pro, as.data.frame=TRUE, stringsAsFactors=FALSE)$data

##CLEAN AND STANDARDIZE DATA##
#get rid of unwanted columns
data_ranks <- subset(data_ranks, select = c(1:2,4:7))

#clean headers
names(data_ranks) <- c("rank","player","best","worst","avg","sd")
names(fp_wrproj) <- c("player","fp_rushatt","fp_rushyd","fp_rushtd","fp_recpts","fp_recyd","fp_rectd","fp_fl","fp_fpts")

#remove non complete ranks
data_ranks <- data_ranks[complete.cases(data_ranks),]

#convert to numeric
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$avg <- as.numeric(data_ranks$avg)
data_ranks$sd <- as.numeric(data_ranks$sd)
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$best <- as.numeric(data_ranks$best)
data_ranks$worst <- as.numeric(data_ranks$worst)
fp_wrproj$fp_rushatt <- as.numeric(fp_wrproj$fp_rushatt)
fp_wrproj$fp_rushyd <- as.numeric(fp_wrproj$fp_rushyd)
fp_wrproj$fp_rushtd <- as.numeric(fp_wrproj$fp_rushtd)
fp_wrproj$fp_recpts <- as.numeric(fp_wrproj$fp_recpts)
fp_wrproj$fp_recyd <- as.numeric(fp_wrproj$fp_recyd)
fp_wrproj$fp_rectd <- as.numeric(fp_wrproj$fp_rectd)
fp_wrproj$fp_fpts <- as.numeric(fp_wrproj$fp_fpts)
fp_wrproj$fp_fl <- as.numeric(fp_wrproj$fp_fl)

##Seperate Name and Team
data_ranks$name1 <- str_replace_all(data_ranks$player, " ", ",")
data_ranks$first <- str_sub(data_ranks$name1, end=str_locate(string=data_ranks$name1, ',')[,1]-1)
data_ranks$last <- str_sub(data_ranks$name1,start=str_locate(string=data_ranks$name1, ',')[,1]+1)
data_ranks$last <- str_sub(data_ranks$last,end=str_locate(string=data_ranks$last, ',')[,1]-1)
data_ranks$name <- paste(data_ranks$first,data_ranks$last, sep = " ")

fp_wrproj$name1 <- str_replace_all(fp_wrproj$player, " ", ",")
fp_wrproj$fp_team <- str_sub(fp_wrproj$player, start = -3)
fp_wrproj$first <- str_sub(fp_wrproj$name1, end=str_locate(string=fp_wrproj$name1, ',')[,1]-1)
fp_wrproj$last <- str_sub(fp_wrproj$name1,start=str_locate(string=fp_wrproj$name1, ',')[,1]+1)
fp_wrproj$last <- str_sub(fp_wrproj$last,end=str_locate(string=fp_wrproj$last, ',')[,1]-1)
fp_wrproj$name <- paste(fp_wrproj$first,fp_wrproj$last, sep = " ")

#Fix Teams or names
#data_ranks[data_ranks$fp_team=="Q", "fp_team"] <- "SEA"

#Calculate points based on League Settings
fp_wrproj$fp_fpts <- (fp_wrproj$fp_recyd*recYdsMultiplier) + (fp_wrproj$fp_rushyd*rushYdsMultiplier) + (fp_wrproj$fp_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  fp_wrproj$fp_fpts <- ifelse(fp_wrproj$fp_recyd >= RecydBonus,fp_wrproj$fp_fpts + Rydbonus, fp_wrproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_wrproj$fp_fpts <- ifelse(fp_wrproj$fp_recyd >= RecydBonus,fp_wrproj$fp_fpts + Rydbonus, fp_wrproj$fp_fpts)
}
#Rushing Yd
if(DailyFantasy == T){
  fp_wrproj$fp_fpts <- ifelse(fp_wrproj$fp_rushyd >= RushBonus,fp_wrproj$fp_fpts + wronus, fp_wrproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_wrproj$fp_fpts <- ifelse(fp_wrproj$fp_rushyd >= RushBonus,fp_wrproj$fp_fpts + wronus, fp_wrproj$fp_fpts)
}

#Add in TD probability
fp_wrproj$fp_rectd <- round(fp_wrproj$fp_rectd, 0)
fp_wrproj$fp_rushtd <- round(fp_wrproj$fp_rushtd, 0)
fp_wrproj$fp_fpts <- ifelse(fp_wrproj$fp_rectd >= 1,fp_wrproj$fp_fpts + (fp_wrproj$fp_rectd*recTdsMultiplier), fp_wrproj$fp_fpts)
fp_wrproj$fp_fpts <- ifelse(fp_wrproj$fp_rushtd >= 1,fp_wrproj$fp_fpts + (fp_wrproj$fp_rushtd*rushTdsMultiplier), fp_wrproj$fp_fpts)

#Remove all insginicant projections
fp_wrproj1 <- subset(fp_wrproj, fp_wrproj$fp_recyd > 0)
fp_wrproj <- fp_wrproj1
rm(fp_wrproj1)

#resort and reorder by name
data_ranks <- subset(data_ranks,select = c("rank","name","best","worst","avg","sd"))
fp_wrproj <- subset(fp_wrproj, select = c("name","fp_team","fp_rushatt","fp_rushyd","fp_rushtd","fp_recpts","fp_recyd","fp_rectd",
                                          "fp_fl","fp_fpts"))
data_ranks <- data_ranks[order(data_ranks$name),]
fp_wrproj <- fp_wrproj[order(fp_wrproj$name),]

#FFTODAY ####

#Create data from urls
fftoday_wrproj <- readHTMLTable(url_fftoday, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
fftoday_wrproj <- subset(fftoday_wrproj, select = c(2:3,5:8))
#clean headers
fftoday_wrproj <- fftoday_wrproj[2:(dim(fftoday_wrproj)[1]-1),]
#name the columns
names(fftoday_wrproj) <- c("player","fftoday_team","fftoday_recpts","fftoday_yd","fftoday_td","fftoday_fpts")

#Convert to numeric
fftoday_wrproj$fftoday_recpts <- as.numeric(fftoday_wrproj$fftoday_recpts)
fftoday_wrproj$fftoday_yd <- as.numeric(fftoday_wrproj$fftoday_yd)
fftoday_wrproj$fftoday_td <- as.numeric(fftoday_wrproj$fftoday_td)
fftoday_wrproj$fftoday_fpts <- as.numeric(fftoday_wrproj$fftoday_fpts)
#remove symbol from fftoday name
fftoday_wrproj$name <- str_sub(fftoday_wrproj$player, start = +3)

#fix names
fftoday_wrproj[fftoday_wrproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."
fftoday_wrproj[fftoday_wrproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
fftoday_wrproj$fftoday_fpts <- (fftoday_wrproj$fftoday_yd*recYdsMultiplier) + (fftoday_wrproj$fftoday_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  fftoday_wrproj$fftoday_fpts <- ifelse(fftoday_wrproj$fftoday_yd >= RecydBonus,fftoday_wrproj$fftoday_fpts + Rydbonus, fftoday_wrproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_wrproj$fftoday_fpts <- ifelse(fftoday_wrproj$fftoday_yd >= RecydBonus,fftoday_wrproj$fftoday_fpts + Rydbonus, fftoday_wrproj$fftoday_fpts)
}

#Add in TD probability
fftoday_wrproj$fftoday_td <- round(fftoday_wrproj$fftoday_td, 0)
fftoday_wrproj$fftoday_fpts <- ifelse(fftoday_wrproj$fftoday_td >= 1,fftoday_wrproj$fftoday_fpts + (fftoday_wrproj$fftoday_td*recTdsMultiplier), fftoday_wrproj$fftoday_fpts)

#Calculate points based on League Settings
fftoday_wrproj$fftoday_fpts <- (fftoday_wrproj$fftoday_yd*recYdsMultiplier) + (fftoday_wrproj$fftoday_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  fftoday_wrproj$fftoday_fpts <- ifelse(fftoday_wrproj$fftoday_yd >= RecydBonus,fftoday_wrproj$fftoday_fpts + Rydbonus, fftoday_wrproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_wrproj$fftoday_fpts <- ifelse(fftoday_wrproj$fftoday_yd >= RecydBonus,fftoday_wrproj$fftoday_fpts + Rydbonus, fftoday_wrproj$fftoday_fpts)
}

#Add in TD probability
fftoday_wrproj$fftoday_td <- round(fftoday_wrproj$fftoday_td, 0)
fftoday_wrproj$fftoday_fpts <- ifelse(fftoday_wrproj$fftoday_td >= 1,fftoday_wrproj$fftoday_fpts + (fftoday_wrproj$fftoday_td*recTdsMultiplier), fftoday_wrproj$fftoday_fpts)


#Remove all insginicant projections
fftoday_wrproj <- subset(fftoday_wrproj, fftoday_wrproj$fftoday_yd > 0)
#resort and order by name
fftoday_wrproj <- subset(fftoday_wrproj, select = c("name","fftoday_team","fftoday_recpts","fftoday_yd","fftoday_td","fftoday_fpts"))
fftoday_wrproj <- fftoday_wrproj[order(fftoday_wrproj$name),]

#NUMBERFIRE #####

#NUMBERFIRE
##Create data from urls
numfire_html <- htmlParse(url_numfire)
numfire_html <- readHTMLTable(numfire_html)
numfire_wrproj <- do.call(rbind.data.frame, numfire_html)
rm(numfire_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns

numfire_wrproj <- subset(numfire_wrproj, select = c(1,5:11,13,21))

#name the columns
names(numfire_wrproj) <- c("player","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts","numfire_dkcost")

#convert to Character so we dont remove the Decimal later
numfire_wrproj$player <- as.character(numfire_wrproj$player)
numfire_wrproj$numfire_rushatt <- as.character(numfire_wrproj$numfire_rushatt)
numfire_wrproj$numfire_rushyd <- as.character(numfire_wrproj$numfire_rushyd)
numfire_wrproj$numfire_rushtd <- as.character(numfire_wrproj$numfire_rushtd)
numfire_wrproj$numfire_recpts <- as.character(numfire_wrproj$numfire_recpts)
numfire_wrproj$numfire_recyd <- as.character(numfire_wrproj$numfire_recyd)
numfire_wrproj$numfire_rectd <- as.character(numfire_wrproj$numfire_rectd)
numfire_wrproj$numfire_fpts <- as.character(numfire_wrproj$numfire_fpts)

#convert to numeric
numfire_wrproj$numfire_rushatt <- as.numeric(numfire_wrproj$numfire_rushatt)
numfire_wrproj$numfire_rushyd <- as.numeric(numfire_wrproj$numfire_rushyd)
numfire_wrproj$numfire_rushtd <- as.numeric(numfire_wrproj$numfire_rushtd)
numfire_wrproj$numfire_recpts <- as.numeric(numfire_wrproj$numfire_recpts)
numfire_wrproj$numfire_recyd <- as.numeric(numfire_wrproj$numfire_recyd)
numfire_wrproj$numfire_rectd <- as.numeric(numfire_wrproj$numfire_rectd)
numfire_wrproj$numfire_fpts <- as.numeric(numfire_wrproj$numfire_fpts)

#Seprate names and team
numfire_wrproj$numfire_team <- str_sub(numfire_wrproj$player, start=str_locate(string=numfire_wrproj$player,',')[,1]+2)
numfire_wrproj$numfire_team <- gsub(pattern = ")", replacement = "", x = numfire_wrproj$numfire_team)
numfire_wrproj$name <- str_sub(numfire_wrproj$player, end=str_locate(string=numfire_wrproj$player,',')[,1]-5)

#Fix names to match rest of sources


#Calculate points based on League Settings
numfire_wrproj$numfire_numfirets <- (numfire_wrproj$numfire_recyd*recYdsMultiplier) + (numfire_wrproj$numfire_rushyd*rushYdsMultiplier) + (numfire_wrproj$numfire_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  numfire_wrproj$numfire_numfirets <- ifelse(numfire_wrproj$numfire_recyd >= RecydBonus,numfire_wrproj$numfire_numfirets + Rydbonus, numfire_wrproj$numfire_numfirets)
}
if(DailyFantasy == F){
  numfire_wrproj$numfire_numfirets <- ifelse(numfire_wrproj$numfire_recyd >= RecydBonus,numfire_wrproj$numfire_numfirets + Rydbonus, numfire_wrproj$numfire_numfirets)
}
#Rushing Yd
if(DailyFantasy == T){
  numfire_wrproj$numfire_numfirets <- ifelse(numfire_wrproj$numfire_rushyd >= RushBonus,numfire_wrproj$numfire_numfirets + wronus, numfire_wrproj$numfire_numfirets)
}
if(DailyFantasy == F){
  numfire_wrproj$numfire_numfirets <- ifelse(numfire_wrproj$numfire_rushyd >= RushBonus,numfire_wrproj$numfire_numfirets + wronus, numfire_wrproj$numfire_numfirets)
}

#Add in TD probability
numfire_wrproj$numfire_rectd <- round(numfire_wrproj$numfire_rectd, 0)
numfire_wrproj$numfire_rushtd <- round(numfire_wrproj$numfire_rushtd, 0)
numfire_wrproj$numfire_numfirets <- ifelse(numfire_wrproj$numfire_rectd >= 1,numfire_wrproj$numfire_numfirets + (numfire_wrproj$numfire_rectd*recTdsMultiplier), numfire_wrproj$numfire_numfirets)
numfire_wrproj$numfire_numfirets <- ifelse(numfire_wrproj$numfire_rushtd >= 1,numfire_wrproj$numfire_numfirets + (numfire_wrproj$numfire_rushtd*rushTdsMultiplier), numfire_wrproj$numfire_numfirets)

#Remove all insginicant projections
numfire_wrproj <- subset(numfire_wrproj, numfire_wrproj$numfire_recyd > 0)

#resort data and reorder by name
numfire_wrproj <- subset(numfire_wrproj, select = c("name","numfire_team","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd",
                                                    "numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts","numfire_dkcost"))
numfire_wrproj <- numfire_wrproj[order(numfire_wrproj$name),]

#MERGE DATA & CALCULATIONS #####

##Create csv for Value Gap Analysis##
#need recyards, rec, rectd

##MERGE DATA##
projections <- merge(cbs_wrproj, espn_wrproj, by="name", all.x = T)
projections <- merge(projections, fftoday_wrproj, by="name", all.x = T)
projections <- merge(projections, ffs_wrproj, by="name", all.x = T)
projections <- merge(projections, fp_wrproj, by="name", all.x = T)
projections <- merge(projections, numfire_wrproj, by="name", all.x = T)

projections <- projections[order(projections$name),]

wrVGArecyd <- subset(projections, select = c("name","cbs_yd","espn_yd","ffs_yd","fp_recyd","fftoday_yd","numfire_recyd"))
columns_wrVGArecyd <- ncol(wrVGArecyd)
wrVGArecyd$recYds <- apply(wrVGArecyd[2:columns_wrVGArecyd], 1, mean, na.rm=TRUE)
wrVGArecyd$recYds <- round(wrVGArecyd$recYds, digits = 1)
wrVGArecyd <- subset(wrVGArecyd, select = c("name","recYds"))

wrVGArectd <- subset(projections, select = c("name","cbs_td","espn_td","ffs_td","fp_rectd","fftoday_td","numfire_rectd"))
wrVGArectd$recTds <- apply(wrVGArectd[2:columns_wrVGArecyd], 1, mean, na.rm=TRUE)
wrVGArectd$recTds <- round(wrVGArectd$recTds, digits = 1)
wrVGArectd <- subset(wrVGArectd, select = c("recTds"))

wrVGArec <- subset(projections, select = c("name","cbs_recpts","espn_recpts","ffs_recpts","fp_recpts","fftoday_recpts","numfire_recpts"))
wrVGArec$rec <- apply(wrVGArec[2:columns_wrVGArecyd], 1, mean, na.rm=TRUE)
wrVGArec$rec <- round(wrVGArec$rec, digits = 1)
wrVGArec <- subset(wrVGArec, select = c("rec"))

wrVGA <- cbind(wrVGArecyd,wrVGArectd,wrVGArec)

write.csv(wrVGA, file=paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Wide Reciever.csv", sep=""), row.names=FALSE)

#Redo the merge so projections are only ones with all sources
projections <- merge(cbs_wrproj, espn_wrproj, by="name", all.x = F)
projections <- merge(projections, fftoday_wrproj, by="name", all.x = F)
projections <- merge(projections, ffs_wrproj, by="name", all.x = F)
projections <- merge(projections, fp_wrproj, by="name", all.x = F)
projections <- merge(projections, numfire_wrproj, by="name", all.x = F)

projections <- projections[order(projections$name),]

##Calculations##

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","numfire_dkcost","cbs_fpts","espn_fpts","ffs_fpts","fftoday_fpts","fp_fpts","numfire_fpts"))
fantasyproj$cbs_fpts <- as.numeric(fantasyproj$cbs_fpts)
fantasyproj$espn_fpts <- as.numeric(fantasyproj$espn_fpts)
fantasyproj$fftoday_fpts <- as.numeric(fantasyproj$fftoday_fpts)
fantasyproj$ffs_fpts <- as.numeric(fantasyproj$ffs_fpts)
fantasyproj$fp_fpts <- as.numeric(fantasyproj$fp_fpts)
fantasyproj$numfire_fpts <- as.numeric(fantasyproj$numfire_fpts)

fantasyproj$numfire_dkcost <- str_sub(fantasyproj$numfire_dkcost, start = +2)
fantasyproj$numfire_dkcost <- as.numeric(fantasyproj$numfire_dkcost)
fantasyproj <- subset(fantasyproj, fantasyproj$numfire_dkcost !=0)

fantasyproj <- fantasyproj[order(fantasyproj$name),]

#ncol count
columns_fantasyproj <- ncol(fantasyproj)
rows_fantasyproj <- nrow(fantasyproj)

#Hodges-Lehmann estimator

#Make zeros NA
fantasyproj[fantasyproj == 0] <- NA
fantasyproj$hodges.lehmann <- apply(fantasyproj[3:columns_fantasyproj],1, function(x) wilcox.test(x, conf.int=T)$estimate)
fantasyproj$hodges.lehmann <- round(fantasyproj[,'hodges.lehmann'], 1)


#find standard deviation of projections
fantasyproj_stdev <- apply(fantasyproj[3:columns_fantasyproj], 1, sd, na.rm=TRUE)

#find mean of projections
fantasyproj_mean <- apply(fantasyproj[3:columns_fantasyproj], 1, mean, na.rm=TRUE)

#round mean/stdev
fantasyproj_mean <- round(fantasyproj_mean, digits = 1)
fantasyproj_stdev <- round(fantasyproj_stdev, digits = 1)

#add mean/stdev
fantasyproj$mean <- fantasyproj_mean
fantasyproj$risk <- fantasyproj_stdev

#add in ranks to fantasyprojections
fantasyproj <- merge(fantasyproj, data_ranks, by="name")

#add ceiling / floor columns
fantasyproj$Ceiling <- fantasyproj$mean + fantasyproj$risk
fantasyproj$Floor <- fantasyproj$mean - fantasyproj$risk

#Make NAs & Remove Floor below 2
fantasyproj$Floor[is.na(fantasyproj$Floor)] <- 0
fantasyproj <- subset(fantasyproj, fantasyproj$Floor > 1.99)

#clustering
#cluster <- Mclust(training$Floor, G=7)
cluster <- kmeans(fantasyproj[,c("mean")],centers = 4)
fantasyproj$Tier <- cluster$cluster

#sort by name
fantasyproj <- fantasyproj[order(-fantasyproj$mean),]
rows_fantasyproj <- nrow(fantasyproj)
fantasyproj$rank <- 1:rows_fantasyproj
data_ranks <- data_ranks[order(data_ranks$name),]

#Top 
fantasyproj2 <- subset(fantasyproj, fantasyproj$rank <= 40)
fantasyproj <- fantasyproj2
rm(fantasyproj2)

#put table into html
htmltable_fantasyproj <- xtable(fantasyproj)
htmltable_projections <- xtable(projections)

#Weekly Fantasy Graphing####
if(DailyFantasy == F){
  ggplot(fantasyproj, aes(x=mean, y=rank, color=factor(Tier))) +
    geom_errorbarh(aes(xmin=Floor,xmax=Ceiling),height=.3)+
    geom_point(size=5,color="white")+
    geom_text(aes(x=mean,label=round(mean,0)),size=3)+
    geom_text(aes(x=mean, label=name, hjust=(-.75), vjust=(0), angle=(0), size=1))+
    theme(
      plot.background = element_blank()
      ,panel.grid.major.x = element_blank()
      ,panel.grid.minor.y = element_blank()
      ,panel.border=element_rect(color="grey",fill=NA)
      ,panel.background = element_blank()
      ,legend.position = "none"
    ) + scale_y_reverse()+
    ylab("Average Rank") + xlab("Median FPTS Projection") +
    labs(title = paste("Week ", week , " Wide Reciever Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == F && ppr == T){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/PPR/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)
  #Save Data
  save(fantasyproj, file = paste(getwd(),"/Data/Weekly/PPR/Week",week," Wide Reciever-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/PPR/Week",week," Wide Reciever-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Wide Reciever-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Wide Reciever-fantasyproj.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," wrtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," wrprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," wrtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," wrprojections.html",sep=""))
}
if(DailyFantasy == F && ppr == F){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/STD/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)
  #Save Data
  save(projections, file = paste(getwd(),"/Data/Weekly/STD/Week",week,"-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Weekly/STD/Week",week,"-projections.csv", sep=""), row.names=FALSE)
  save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"-projections.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," wrtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," wrprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," wrtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," wrprojections.html",sep=""))
}

#Daily Fantasy Graphing####
if(DailyFantasy == T){
  ggplot(fantasyproj, aes(x=mean, y=rank, color=factor(Tier))) +
    geom_errorbarh(aes(xmin=Floor,xmax=Ceiling),height=.3)+
    geom_point(size=5,color="white")+
    geom_text(aes(x=mean,label=round(mean,0)),size=3)+
    geom_text(aes(x=mean, label=name, hjust=(-.75), vjust=(0), angle=(0), size=1))+
    theme(
      plot.background = element_blank()
      ,panel.grid.major.x = element_blank()
      ,panel.grid.minor.y = element_blank()
      ,panel.border=element_rect(color="grey",fill=NA)
      ,panel.background = element_blank()
      ,legend.position = "none"
    ) + scale_y_reverse()+
    ylab("Average Rank") + xlab("Median FPTS Projection") +
    labs(title = paste("Week ", week , " Wide Reciever Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)
}
if(DailyFantasy == T){
  #Cost vs points
  ggplot(fantasyproj, aes(x=mean, y=numfire_dkcost, color=factor(Tier))) +
    geom_errorbarh(aes(xmin=Floor,xmax=Ceiling),height=.3)+
    geom_point(size=5,color="white")+
    geom_text(aes(x=mean,label=round(mean,0)),size=3)+
    geom_text(aes(x=mean, label=name, hjust=(-.75), vjust=(0), angle=(0), size=1))+
    theme(
      plot.background = element_blank()
      ,panel.grid.major.x = element_blank()
      ,panel.grid.minor.y = element_blank()
      ,panel.border=element_rect(color="grey",fill=NA)
      ,panel.background = element_blank()
      ,legend.position = "none"
    ) +
    ylab("Cost") + xlab("Median FPTS Projection") +
    labs(title = paste("Week ", week , " Wide Reciever Projections vs Cost", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever Cost.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever Cost.jpg", sep=""), width=10, height=10)
  #Save file
  save(fantasyproj, file = paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Wide Reciever-fantasyproj.csv", sep=""), row.names=FALSE)
  #save the html file
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," wrtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," wrprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," wrtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," wrprojections.html",sep=""))
}
