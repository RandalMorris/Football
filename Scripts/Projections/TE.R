#Weekly Projections

#DATA SETTINGS ####

#URLS
url_cbs <- paste("http://www.cbssports.com/fantasy/football/stats/weeklyprojections/TE/", week, "/avg/ppr?&print_rows=9999", sep="")
url_espn <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=6&leagueId=",espn_std,sep="")
url_espn1 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=6&leagueId=",espn_std,"&startIndex=40",sep="")
url_espn2 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=6&leagueId=",espn_std,"&startIndex=80",sep="")
url_fftoday <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=40,&LeagueID=", fft_espnSTD, sep="")
url_fftoday1 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=40,&LeagueID=", fft_espnSTD,"&order_by=FFPts&sort_order=DESC&cur_page=1", sep="")
url_fantasy_pro_ranks <- ("http://www.fantasypros.com/nfl/rankings/ppr-te.php")
url_fantasy_pro <- ("http://www.fantasypros.com/nfl/projections/te.php")
url_numfire <- paste("http://www.numberfire.com/nfl/fantasy/fantasy-football-ppr-projections/te")

##STANDARIZATION TIME##

#CBS ####
##Create data from urls
cbs_html <- htmlParse(url_cbs)
cbs_html <- readHTMLTable(cbs_html)
cbs_teproj <- do.call(rbind.data.frame, cbs_html)
rm(cbs_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
cbs_teproj <- subset(cbs_teproj, select = c(1:3,5:7))
#clean headers
cbs_teproj <- cbs_teproj[3:(dim(cbs_teproj)[1]-1),]
#name the columns
names(cbs_teproj) <- c("player","cbs_recpts","cbs_yd","cbs_td","cbs_fl","cbs_fpts")

#convert to Character so we dont remove the Decimal later
cbs_teproj$cbs_recpts <- as.character(cbs_teproj$cbs_recpts)
cbs_teproj$cbs_yd <- as.character(cbs_teproj$cbs_yd)
cbs_teproj$cbs_td <- as.character(cbs_teproj$cbs_td)
cbs_teproj$cbs_fl <- as.character(cbs_teproj$cbs_fl)
cbs_teproj$cbs_fpts <- as.character(cbs_teproj$cbs_fpts)

#convert to numeric
cbs_teproj$cbs_recpts <- as.numeric(cbs_teproj$cbs_recpts)
cbs_teproj$cbs_yd <- as.numeric(cbs_teproj$cbs_yd)
cbs_teproj$cbs_td <- as.numeric(cbs_teproj$cbs_td)
cbs_teproj$cbs_fl <- as.numeric(cbs_teproj$cbs_fl)
cbs_teproj$cbs_fpts <- as.numeric(cbs_teproj$cbs_fpts)

#Seprate names and team
cbs_teproj$name <- str_sub(cbs_teproj$player, end=str_locate(string=cbs_teproj$player, ',')[,1]-1)
cbs_teproj$cbs_team <- str_trim(str_sub(cbs_teproj$player, start= -3))

#Fix names to match rest of sources
cbs_teproj[cbs_teproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."

#Calculate points based on League Settings
cbs_teproj$cbs_fpts <- (cbs_teproj$cbs_yd*recYdsMultiplier) + (cbs_teproj$cbs_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  cbs_teproj$cbs_fpts <- ifelse(cbs_teproj$cbs_yd >= RecydBonus,cbs_teproj$cbs_fpts + Rydbonus, cbs_teproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_teproj$cbs_fpts <- ifelse(cbs_teproj$cbs_yd >= RecydBonus,cbs_teproj$cbs_fpts + Rydbonus, cbs_teproj$cbs_fpts)
}

#Add in TD probability
cbs_teproj$cbs_td <- round(cbs_teproj$cbs_td, 0)
cbs_teproj$cbs_fpts <- ifelse(cbs_teproj$cbs_td >= 1,cbs_teproj$cbs_fpts + (cbs_teproj$cbs_td*recTdsMultiplier), cbs_teproj$cbs_fpts)


#Remove all insginicant projections
cbs_teproj1 <- subset(cbs_teproj, cbs_teproj$cbs_yd > 0)
cbs_teproj <- cbs_teproj1
rm(cbs_teproj1)

#resort data and reorder by name
cbs_teproj <- subset(cbs_teproj, select = c("name","cbs_team","cbs_recpts","cbs_yd","cbs_td","cbs_fl","cbs_fpts"))
cbs_teproj <- cbs_teproj[order(cbs_teproj$name),]

#ESPN #####
#ESPN
##Create data from urls
espn_teproj <- readHTMLTable(url_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
espn_teproj1 <- readHTMLTable(url_espn1, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
espn_teproj2 <- readHTMLTable(url_espn2, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
espn_teproj <- subset(espn_teproj, select = c(1,11:14))
espn_teproj1 <-  subset(espn_teproj1, select = c(1,11:14))
espn_teproj2 <-  subset(espn_teproj2, select = c(1,11:14))
#clean headers
espn_teproj <- espn_teproj[2:(dim(espn_teproj)[1]),]
espn_teproj1 <- espn_teproj1[2:(dim(espn_teproj1)[1]),]
espn_teproj2 <- espn_teproj2[2:(dim(espn_teproj2)[1]),]
#name the columns
names(espn_teproj) <- names(espn_teproj1) <- names(espn_teproj2) <- c("player","espn_recpts","espn_yd","espn_td","espn_fpts")

#Merge and remove other dataframes to reduce clutter
espn_teproj <- rbind(espn_teproj, espn_teproj1, espn_teproj2)
rm(espn_teproj1, espn_teproj2)

#convert to numeric
espn_teproj$espn_recpts <- as.numeric(espn_teproj$espn_recpts)
espn_teproj$espn_yd <- as.numeric(espn_teproj$espn_yd)
espn_teproj$espn_td <- gsub("--", "0",espn_teproj$espn_td)
espn_teproj$espn_td <- as.numeric(espn_teproj$espn_td)
espn_teproj$espn_fpts <- as.numeric(espn_teproj$espn_fpts)

#Seperate names and team
espn_teproj$name <- str_sub(espn_teproj$player, end=str_locate(string=espn_teproj$player, ',')[,1]-1)
#espn_teproj$name <- str_replace_all(espn_teproj$name, "\\*", "")
espn_teproj$espn_team <- str_sub(espn_teproj$player, start=str_locate(string=espn_teproj$player, ',')[,1]+1)
espn_teproj$espn_team <- str_to_upper(str_sub(espn_teproj$espn_team, end=str_locate(string=espn_teproj$espn_team, ' ')[,1]+3))

#Fix names to match rest of sources
espn_teproj[espn_teproj$name=="Stevie Johnson", "name"] <- "Steve Johnson"
espn_teproj[espn_teproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
espn_teproj$espn_fpts <- (espn_teproj$espn_yd*recYdsMultiplier) + (espn_teproj$espn_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  espn_teproj$espn_fpts <- ifelse(espn_teproj$espn_yd >= RecydBonus,espn_teproj$espn_fpts + Rydbonus, espn_teproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_teproj$espn_fpts <- ifelse(espn_teproj$espn_yd >= RecydBonus,espn_teproj$espn_fpts + Rydbonus, espn_teproj$espn_fpts)
}

#Add in TD probability
espn_teproj$espn_td <- round(espn_teproj$espn_td, 0)
espn_teproj$espn_fpts <- ifelse(espn_teproj$espn_td >= 1,espn_teproj$espn_fpts + (espn_teproj$espn_td*recTdsMultiplier), espn_teproj$espn_fpts)

#Remove all insginicant projections
espn_teproj1 <- subset(espn_teproj, espn_teproj$espn_yd > 0)
espn_teproj <- espn_teproj1
rm(espn_teproj1)

#resort and reorder by name
espn_teproj <- subset(espn_teproj, select = c("name","espn_team","espn_recpts","espn_yd","espn_td","espn_fpts"))
espn_teproj <- espn_teproj[order(espn_teproj$name),]

#FANTASY SHARKS ####

#FANTASY SHARKS
##Create data from urls

ffs_teproj <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=te&l=12&format=csv")

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
ffs_teproj <- subset(ffs_teproj, select = c(3,4,7:10))

#name the columns
names(ffs_teproj) <- c("player","ffs_team","ffs_recpts","ffs_yd","ffs_td","ffs_fpts")

#convert to numeric
ffs_teproj$ffs_recpts <- as.numeric(ffs_teproj$ffs_recpts)
ffs_teproj$ffs_yd <- as.numeric(ffs_teproj$ffs_yd)
ffs_teproj$ffs_td <- as.numeric(ffs_teproj$ffs_td)
ffs_teproj$ffs_fpts <- as.numeric(ffs_teproj$ffs_fpts)

#Add Recpts for PPR
ffs_teproj$ffs_fpts <- ffs_teproj$ffs_fpts + ffs_teproj$ffs_recpts

#convert name and team to Char
ffs_teproj$player <- as.character(ffs_teproj$player)
ffs_teproj$ffs_team <- as.character(ffs_teproj$ffs_team)

#Seprate names and  fix 2 Char teams
ffs_teproj$last <- str_sub(ffs_teproj$player, end=str_locate(string=ffs_teproj$player, ' ')[,1]-2)
ffs_teproj$first <- str_sub(ffs_teproj$player, start=str_locate(string=ffs_teproj$player,' ')[,1]+1)
ffs_teproj$name <- paste(ffs_teproj$first, ffs_teproj$last)
ffs_teproj[ffs_teproj$ffs_team=="NOS", "ffs_team"] <- "NO"
ffs_teproj[ffs_teproj$ffs_team=="KCC", "ffs_team"] <- "KC"
ffs_teproj[ffs_teproj$ffs_team=="GBP", "ffs_team"] <- "GB"
ffs_teproj[ffs_teproj$ffs_team=="SDC", "ffs_team"] <- "SD"
ffs_teproj[ffs_teproj$ffs_team=="SFF", "ffs_team"] <- "SF"
ffs_teproj[ffs_teproj$ffs_team=="TBB", "ffs_team"] <- "TB"

#Fix names to match rest of sources

#Calculate points based on League Settings
ffs_teproj$ffs_fpts <- (ffs_teproj$ffs_yd*recYdsMultiplier) + (ffs_teproj$ffs_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  ffs_teproj$ffs_fpts <- ifelse(ffs_teproj$ffs_yd >= RecydBonus,ffs_teproj$ffs_fpts + Rydbonus, ffs_teproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_teproj$ffs_fpts <- ifelse(ffs_teproj$ffs_yd >= RecydBonus,ffs_teproj$ffs_fpts + Rydbonus, ffs_teproj$ffs_fpts)
}

#Add in TD probability
ffs_teproj$ffs_td <- round(ffs_teproj$ffs_td, 0)
ffs_teproj$ffs_fpts <- ifelse(ffs_teproj$ffs_td >= 1,ffs_teproj$ffs_fpts + (ffs_teproj$ffs_td*recTdsMultiplier), ffs_teproj$ffs_fpts)

#Remove all insginicant projections
ffs_teproj2 <- subset(ffs_teproj, ffs_teproj$ffs_yd > 0)
ffs_teproj <- ffs_teproj2
rm(ffs_teproj2)

#resort data and reorder by name
ffs_teproj <- subset(ffs_teproj, select = c("name","ffs_team","ffs_recpts","ffs_yd","ffs_td","ffs_fpts"))
ffs_teproj <- ffs_teproj[order(ffs_teproj$name),]

#FFTODAY ####

#FFTODAY
#Create data from urls
fftoday_teproj <- readHTMLTable(url_fftoday, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
fftoday_teproj <- subset(fftoday_teproj, select = c(2:3,5:8))
#clean headers
fftoday_teproj <- fftoday_teproj[2:(dim(fftoday_teproj)[1]-1),]
#name the columns
names(fftoday_teproj) <- c("player","fftoday_team","fftoday_recpts","fftoday_yd","fftoday_td","fftoday_fpts")

#Convert to numeric
fftoday_teproj$fftoday_recpts <- as.numeric(fftoday_teproj$fftoday_recpts)
fftoday_teproj$fftoday_yd <- as.numeric(fftoday_teproj$fftoday_yd)
fftoday_teproj$fftoday_td <- as.numeric(fftoday_teproj$fftoday_td)
fftoday_teproj$fftoday_fpts <- as.numeric(fftoday_teproj$fftoday_fpts)

#remove symbol from fftoday name
fftoday_teproj$name <- str_sub(fftoday_teproj$player, start = +3)

#fix names
fftoday_teproj[fftoday_teproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."
fftoday_teproj[fftoday_teproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
fftoday_teproj$fftoday_fpts <- (fftoday_teproj$fftoday_yd*recYdsMultiplier) + (fftoday_teproj$fftoday_recpts*recMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  fftoday_teproj$fftoday_fpts <- ifelse(fftoday_teproj$fftoday_yd >= RecydBonus,fftoday_teproj$fftoday_fpts + Rydbonus, fftoday_teproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_teproj$fftoday_fpts <- ifelse(fftoday_teproj$fftoday_yd >= RecydBonus,fftoday_teproj$fftoday_fpts + Rydbonus, fftoday_teproj$fftoday_fpts)
}

#Add in TD probability
fftoday_teproj$fftoday_td <- round(fftoday_teproj$fftoday_td, 0)
fftoday_teproj$fftoday_fpts <- ifelse(fftoday_teproj$fftoday_td >= 1,fftoday_teproj$fftoday_fpts + (fftoday_teproj$fftoday_td*recTdsMultiplier), fftoday_teproj$fftoday_fpts)

#Remove all insginicant projections
fftoday_teproj <- subset(fftoday_teproj, fftoday_teproj$fftoday_yd > 0)
#resort and order by name

fftoday_teproj <- subset(fftoday_teproj, select = c("name","fftoday_team","fftoday_recpts","fftoday_yd","fftoday_td","fftoday_fpts"))
fftoday_teproj <- fftoday_teproj[order(fftoday_teproj$name),]

#FANTASYPRO ####

#FANTASY PROS Ranks & Projections
#Create data from urls
data_ranks <- readHTMLTable(url_fantasy_pro_ranks, as.data.frame=TRUE, stringsAsFactors=FALSE)$data
fp_teproj <- readHTMLTable(url_fantasy_pro, as.data.frame=TRUE, stringsAsFactors=FALSE)$data

##CLEAN AND STANDARDIZE DATA##
#get rid of unwanted columns
data_ranks <- subset(data_ranks, select = c(1:2,4:7))

#clean headers
names(data_ranks) <- c("rank","player","best","worst","avg","sd")
names(fp_teproj) <- c("player","fp_recpts","fp_yd","fp_td","fp_fl","fp_fpts")

#remove non complete ranks
data_ranks <- data_ranks[complete.cases(data_ranks),]

#convert to numeric
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$avg <- as.numeric(data_ranks$avg)
data_ranks$sd <- as.numeric(data_ranks$sd)
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$best <- as.numeric(data_ranks$best)
data_ranks$worst <- as.numeric(data_ranks$worst)
fp_teproj$fp_recpts <- as.numeric(fp_teproj$fp_recpts)
fp_teproj$fp_yd <- as.numeric(fp_teproj$fp_yd)
fp_teproj$fp_td <- as.numeric(fp_teproj$fp_td)
fp_teproj$fp_fpts <- as.numeric(fp_teproj$fp_fpts)

##Seperate Name and Team
data_ranks$name1 <- str_replace_all(data_ranks$player, " ", ",")
data_ranks$first <- str_sub(data_ranks$name1, end=str_locate(string=data_ranks$name1, ',')[,1]-1)
data_ranks$last <- str_sub(data_ranks$name1,start=str_locate(string=data_ranks$name1, ',')[,1]+1)
data_ranks$last <- str_sub(data_ranks$last,end=str_locate(string=data_ranks$last, ',')[,1]-1)
data_ranks$name <- paste(data_ranks$first,data_ranks$last, sep = " ")

fp_teproj$name1 <- str_replace_all(fp_teproj$player, " ", ",")
fp_teproj$fp_team <- str_sub(fp_teproj$player, start = -3)
fp_teproj$first <- str_sub(fp_teproj$name1, end=str_locate(string=fp_teproj$name1, ',')[,1]-1)
fp_teproj$last <- str_sub(fp_teproj$name1,start=str_locate(string=fp_teproj$name1, ',')[,1]+1)
fp_teproj$last <- str_sub(fp_teproj$last,end=str_locate(string=fp_teproj$last, ',')[,1]-1)
fp_teproj$name <- paste(fp_teproj$first,fp_teproj$last, sep = " ")

#Fix Teams or names
#data_ranks[data_ranks$fp_team=="Q", "fp_team"] <- "SEA"

#Calculate points based on League Settings
fp_teproj$fp_fpts <- (fp_teproj$fp_yd*recYdsMultiplier) + (fp_teproj$fp_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  fp_teproj$fp_fpts <- ifelse(fp_teproj$fp_yd >= RecydBonus,fp_teproj$fp_fpts + Rydbonus, fp_teproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_teproj$fp_fpts <- ifelse(fp_teproj$fp_yd >= RecydBonus,fp_teproj$fp_fpts + Rydbonus, fp_teproj$fp_fpts)
}

#Add in TD probability
fp_teproj$fp_rectd <- round(fp_teproj$fp_td, 0)
fp_teproj$fp_fpts <- ifelse(fp_teproj$fp_td >= 1,fp_teproj$fp_fpts + (fp_teproj$fp_td*recTdsMultiplier), fp_teproj$fp_fpts)

#Remove all insginicant projections
fp_teproj1 <- subset(fp_teproj, fp_teproj$fp_yd > 0)
fp_teproj <- fp_teproj1
rm(fp_teproj1)

#resort and reorder by name
data_ranks <- subset(data_ranks,select = c("rank","name","best","worst","avg","sd"))
fp_teproj <- subset(fp_teproj, select = c("name","fp_team","fp_recpts","fp_yd","fp_td","fp_fl","fp_fpts"))
data_ranks <- data_ranks[order(data_ranks$name),]
fp_teproj <- fp_teproj[order(fp_teproj$name),]

#NUMBERFIRE #####

#NUMBERFIRE
##Create data from urls
numfire_html <- htmlParse(url_numfire)
numfire_html <- readHTMLTable(numfire_html)
numfire_teproj <- do.call(rbind.data.frame, numfire_html)
rm(numfire_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns

numfire_teproj <- subset(numfire_teproj, select = c(1,9:11,13,21))

#name the columns
names(numfire_teproj) <- c("player","numfire_recpts","numfire_yd","numfire_td","numfire_fpts","numfire_dkcost")

#convert to Character so we dont remove the Decimal later
numfire_teproj$player <- as.character(numfire_teproj$player)
numfire_teproj$numfire_recpts <- as.character(numfire_teproj$numfire_recpts)
numfire_teproj$numfire_yd <- as.character(numfire_teproj$numfire_yd)
numfire_teproj$numfire_td <- as.character(numfire_teproj$numfire_td)
numfire_teproj$numfire_fpts <- as.character(numfire_teproj$numfire_fpts)

#convert to numeric
numfire_teproj$numfire_recpts <- as.numeric(numfire_teproj$numfire_recpts)
numfire_teproj$numfire_yd <- as.numeric(numfire_teproj$numfire_yd)
numfire_teproj$numfire_td <- as.numeric(numfire_teproj$numfire_td)
numfire_teproj$numfire_fpts <- as.numeric(numfire_teproj$numfire_fpts)

#Seprate names and team
numfire_teproj$numfire_team <- str_sub(numfire_teproj$player, start=str_locate(string=numfire_teproj$player,',')[,1]+2)
numfire_teproj$numfire_team <- gsub(pattern = ")", replacement = "", x = numfire_teproj$numfire_team)
numfire_teproj$name <- str_sub(numfire_teproj$player, end=str_locate(string=numfire_teproj$player,',')[,1]-5)

#Fix names to match rest of sources

#Calculate points based on League Settings
numfire_teproj$numfire_fpts <- (numfire_teproj$numfire_yd*recYdsMultiplier) + (numfire_teproj$numfire_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  numfire_teproj$numfire_fpts <- ifelse(numfire_teproj$numfire_yd >= RecydBonus,numfire_teproj$numfire_fpts + Rydbonus, numfire_teproj$numfire_fpts)
}
if(DailyFantasy == F){
  numfire_teproj$numfire_fpts <- ifelse(numfire_teproj$numfire_td >= RecydBonus,numfire_teproj$numfire_fpts + Rydbonus, numfire_teproj$numfire_fpts)
}


#Add in TD probability
numfire_teproj$numfire_td <- round(numfire_teproj$numfire_td, 0)
numfire_teproj$numfire_fpts <- ifelse(numfire_teproj$numfire_td >= 1,numfire_teproj$numfire_fpts + (numfire_teproj$numfire_td*recTdsMultiplier), numfire_teproj$numfire_fpts)

#Remove all insginicant projections
numfire_teproj <- subset(numfire_teproj, numfire_teproj$numfire_yd > 0)

#resort data and reorder by name
numfire_teproj <- subset(numfire_teproj, select = c("name","numfire_team","numfire_recpts","numfire_yd","numfire_td","numfire_fpts","numfire_dkcost"))
numfire_teproj <- numfire_teproj[order(numfire_teproj$name),]

#MERGE DATA & CALCULATIONS #####

##Create csv for Value Gap Analysis##
#need recyards, rec, rectd

##MERGE DATA##
projections <- merge(cbs_teproj, espn_teproj, by="name", all.x = T)
projections <- merge(projections, fftoday_teproj, by="name", all.x = T)
projections <- merge(projections, ffs_teproj, by="name", all.x = T)
projections <- merge(projections, fp_teproj, by="name", all.x = T)
projections <- merge(projections, numfire_teproj, by="name", all.x = T)

projections <- projections[order(projections$name),]

teVGArecyd <- subset(projections, select = c("name","cbs_yd","espn_yd","ffs_yd","fp_yd","fftoday_yd","numfire_yd"))
columns_teVGArecyd <- ncol(teVGArecyd)
teVGArecyd$recYds <- apply(teVGArecyd[2:columns_teVGArecyd], 1, mean, na.rm=TRUE)
teVGArecyd$recYds <- round(teVGArecyd$recYds, digits = 1)
teVGArecyd <- subset(teVGArecyd, select = c("name","recYds"))

teVGArectd <- subset(projections, select = c("name","cbs_td","espn_td","ffs_td","fp_td","fftoday_td","numfire_td"))
teVGArectd$recTds <- apply(teVGArectd[2:columns_teVGArecyd], 1, mean, na.rm=TRUE)
teVGArectd$recTds <- round(teVGArectd$recTds, digits = 1)
teVGArectd <- subset(teVGArectd, select = c("recTds"))

teVGArec <- subset(projections, select = c("name","cbs_recpts","espn_recpts","ffs_recpts","fp_recpts","fftoday_recpts","numfire_recpts"))
teVGArec$rec <- apply(teVGArec[2:columns_teVGArecyd], 1, mean, na.rm=TRUE)
teVGArec$rec <- round(teVGArec$rec, digits = 1)
teVGArec <- subset(teVGArec, select = c("rec"))

teVGA <- cbind(teVGArecyd,teVGArectd,teVGArec)

write.csv(teVGA, file=paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Tight End.csv", sep=""), row.names=FALSE)

#Redo the merge so projections are only ones with all sources
projections <- merge(cbs_teproj, espn_teproj, by="name", all.x = F)
projections <- merge(projections, fftoday_teproj, by="name", all.x = F)
projections <- merge(projections, ffs_teproj, by="name", all.x = F)
projections <- merge(projections, fp_teproj, by="name", all.x = F)
projections <- merge(projections, numfire_teproj, by="name", all.x = F)

projections <- projections[order(projections$name),]

##Calculations##

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","numfire_dkcost","cbs_fpts","espn_fpts","ffs_fpts","fftoday_fpts","fp_fpts","numfire_fpts"))
fantasyproj$cbs_fpts <- as.numeric(fantasyproj$cbs_fpts)
fantasyproj$espn_fpts <- as.numeric(fantasyproj$espn_fpts)
fantasyproj$fft_fpts <- as.numeric(fantasyproj$fftoday_fpts)
fantasyproj$ffs_fpts <- as.numeric(fantasyproj$ffs_fpts)
fantasyproj$fp_fpts <- as.numeric(fantasyproj$fp_fpts)
fantasyproj$numfire_fpts <- as.numeric(fantasyproj$numfire_fpts)

fantasyproj$numfire_dkcost <- str_sub(fantasyproj$numfire_dkcost, start = +2)
fantasyproj$numfire_dkcost <- as.numeric(fantasyproj$numfire_dkcost)
fantasyproj <- subset(fantasyproj, fantasyproj$numfire_dkcost !=0)

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
fantasyproj2 <- subset(fantasyproj, fantasyproj$rank <= 30)
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
    labs(title = paste("Week ", week , " Tight End Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == F && ppr == T){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/Week ",week," Tight End.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/PPR/Week ",week," Tight End.jpg", sep=""), width=10, height=10)
  #Save Data
  save(fantasyproj, file = paste(getwd(),"/Data/Weekly/PPR/Week",week," Tight End-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/PPR/Week",week," Tight End-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Tight End-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Tight End-fantasyproj.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," tetier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," teprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," tetier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," teprojections.html",sep=""))
}
if(DailyFantasy == F && ppr == F){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/Week ",week," Tight End.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/STD/Week ",week," Tight End.jpg", sep=""), width=10, height=10)
  #Save Data
  save(projections, file = paste(getwd(),"/Data/Weekly/STD/Week",week,"-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Weekly/STD/Week",week,"-projections.csv", sep=""), row.names=FALSE)
  save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"-projections.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," tetier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," teprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," tetier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," teprojections.html",sep=""))
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
    labs(title = paste("Week ", week , " Tight End Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Tight End.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Tight End.jpg", sep=""), width=10, height=10)
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
    labs(title = paste("Week ", week , " Tight End Projections vs Cost", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Tight End Cost.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Tight End Cost.jpg", sep=""), width=10, height=10)
  #Save file
  save(fantasyproj, file = paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Tight End-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Tight End-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Tight End-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Tight End-fantasyproj.csv", sep=""), row.names=FALSE)
  #save the html file
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," tetier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," teprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," tetier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," teprojections.html",sep=""))
}
