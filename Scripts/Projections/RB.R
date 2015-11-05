#Weekly Projections

#DATA SETTINGS ####

#URLS ####
url_cbs <- paste("http://www.cbssports.com/fantasy/football/stats/weeklyprojections/RB/", week, "/avg/ppr?&print_rows=9999", sep="")
url_espn <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=2&leagueId=",espn_std,sep="")
url_espn1 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=2&leagueId=",espn_std,"&startIndex=40",sep="")
url_espn2 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=2&leagueId=",espn_std,"&startIndex=80",sep="")
url_fftoday <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=20,&LeagueID=", fft_espnSTD, sep="")
url_fftoday1 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=20,&LeagueID=", fft_espnSTD,"&order_by=FFPts&sort_order=DESC&cur_page=1", sep="")
url_fantasy_pro_ranks <- ("http://www.fantasypros.com/nfl/rankings/ppr-rb.php")
url_fantasy_pro <- ("http://www.fantasypros.com/nfl/projections/rb.php")
url_numfire <- paste("http://www.numberfire.com/nfl/fantasy/fantasy-football-ppr-projections/rb")

##STANDARIZATION TIME##

#CBS ####
##Create data from urls
cbs_html <- htmlParse(url_cbs)
cbs_html <- readHTMLTable(cbs_html)
cbs_rbproj <- do.call(rbind.data.frame, cbs_html)
rm(cbs_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
cbs_rbproj <- subset(cbs_rbproj, select = c(1:3,5:7,9:11))
#clean headers
cbs_rbproj <- cbs_rbproj[3:(dim(cbs_rbproj)[1]-1),]
#name the columns
names(cbs_rbproj) <- c("player","cbs_rushatt","cbs_rushyd","cbs_rushtd","cbs_recpts","cbs_recyd","cbs_rectd","cbs_fl","cbs_fpts")

#convert to Character so we dont remove the Decimal later
cbs_rbproj$cbs_rushatt <- as.character(cbs_rbproj$cbs_rushatt)
cbs_rbproj$cbs_rushyd <- as.character(cbs_rbproj$cbs_rushyd)
cbs_rbproj$cbs_rushtd <- as.character(cbs_rbproj$cbs_rushtd)
cbs_rbproj$cbs_recpts <- as.character(cbs_rbproj$cbs_recpts)
cbs_rbproj$cbs_recyd <- as.character(cbs_rbproj$cbs_recyd)
cbs_rbproj$cbs_rectd <- as.character(cbs_rbproj$cbs_rectd)
cbs_rbproj$cbs_fl <- as.character(cbs_rbproj$cbs_fl)
cbs_rbproj$cbs_fpts <- as.character(cbs_rbproj$cbs_fpts)

#convert to numeric
cbs_rbproj$cbs_rushatt <- as.numeric(cbs_rbproj$cbs_rushatt)
cbs_rbproj$cbs_rushyd <- as.numeric(cbs_rbproj$cbs_rushyd)
cbs_rbproj$cbs_rushtd <- as.numeric(cbs_rbproj$cbs_rushtd)
cbs_rbproj$cbs_recpts <- as.numeric(cbs_rbproj$cbs_recpts)
cbs_rbproj$cbs_recyd <- as.numeric(cbs_rbproj$cbs_recyd)
cbs_rbproj$cbs_rectd <- as.numeric(cbs_rbproj$cbs_rectd)
cbs_rbproj$cbs_fl <- as.numeric(cbs_rbproj$cbs_fl)
cbs_rbproj$cbs_fpts <- as.numeric(cbs_rbproj$cbs_fpts)

#Seprate names and team
cbs_rbproj$name <- str_sub(cbs_rbproj$player, end=str_locate(string=cbs_rbproj$player, ',')[,1]-1)
cbs_rbproj$cbs_team <- str_trim(str_sub(cbs_rbproj$player, start= -3))

#Fix names to match rest of sources
cbs_rbproj[cbs_rbproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."

#Calculate points based on League Settings
cbs_rbproj$cbs_fpts <- (cbs_rbproj$cbs_recyd*recYdsMultiplier) + (cbs_rbproj$cbs_rushyd*rushYdsMultiplier) + (cbs_rbproj$cbs_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  cbs_rbproj$cbs_fpts <- ifelse(cbs_rbproj$cbs_recyd >= RecydBonus,cbs_rbproj$cbs_fpts + Rydbonus, cbs_rbproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_rbproj$cbs_fpts <- ifelse(cbs_rbproj$cbs_recyd >= RecydBonus,cbs_rbproj$cbs_fpts + Rydbonus, cbs_rbproj$cbs_fpts)
}
#Rushing Yd
if(DailyFantasy == T){
  cbs_rbproj$cbs_fpts <- ifelse(cbs_rbproj$cbs_rushyd >= RushBonus,cbs_rbproj$cbs_fpts + Rbonus, cbs_rbproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_rbproj$cbs_fpts <- ifelse(cbs_rbproj$cbs_rushyd >= RushBonus,cbs_rbproj$cbs_fpts + Rbonus, cbs_rbproj$cbs_fpts)
}

#Add in TD probability
cbs_rbproj$cbs_rectd <- round(cbs_rbproj$cbs_rectd, 0)
cbs_rbproj$cbs_rushtd <- round(cbs_rbproj$cbs_rushtd, 0)
cbs_rbproj$cbs_fpts <- ifelse(cbs_rbproj$cbs_rectd >= 1,cbs_rbproj$cbs_fpts + (cbs_rbproj$cbs_rectd*recTdsMultiplier), cbs_rbproj$cbs_fpts)
cbs_rbproj$cbs_fpts <- ifelse(cbs_rbproj$cbs_rushtd >= 1,cbs_rbproj$cbs_fpts + (cbs_rbproj$cbs_rushtd*rushTdsMultiplier), cbs_rbproj$cbs_fpts)

#resort data and reorder by name
cbs_rbproj <- subset(cbs_rbproj, select = c("name","cbs_team","cbs_rushatt","cbs_rushyd","cbs_rushtd","cbs_recpts","cbs_recyd",
                                            "cbs_rectd","cbs_fl","cbs_fpts"))
cbs_rbproj <- cbs_rbproj[order(cbs_rbproj$name),]

#ESPN #####
#ESPN
##Create data from urls
espn_rbproj <- readHTMLTable(url_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
espn_rbproj1 <- readHTMLTable(url_espn1, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
espn_rbproj2 <- readHTMLTable(url_espn2, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
espn_rbproj <- subset(espn_rbproj, select = c(1,8,9,10,11,12,13,14))
espn_rbproj1 <-  subset(espn_rbproj1, select = c(1,8,9,10,11,12,13,14))
espn_rbproj2 <-  subset(espn_rbproj2, select = c(1,8,9,10,11,12,13,14))
#clean headers
espn_rbproj <- espn_rbproj[2:(dim(espn_rbproj)[1]),]
espn_rbproj1 <- espn_rbproj1[2:(dim(espn_rbproj1)[1]),]
espn_rbproj2 <- espn_rbproj2[2:(dim(espn_rbproj2)[1]),]
#name the columns
names(espn_rbproj) <- names(espn_rbproj1) <- names(espn_rbproj2) <- c("player","espn_rushatt","espn_rushyd","espn_rushtd",
                                                                      "espn_recpts","espn_recyd","espn_rectd","espn_fpts")
#Merge and remove other dataframes to reduce clutter
espn_rbproj <- rbind(espn_rbproj, espn_rbproj1, espn_rbproj2)
rm(espn_rbproj1, espn_rbproj2)

#convert to numeric
espn_rbproj$espn_rushatt <- as.numeric(espn_rbproj$espn_rushatt)
espn_rbproj$espn_rushyd <- as.numeric(espn_rbproj$espn_rushyd)
espn_rbproj$espn_rushtd <- as.numeric(espn_rbproj$espn_rushtd)
espn_rbproj$espn_recpts <- as.numeric(espn_rbproj$espn_recpts)
espn_rbproj$espn_recyd <- as.numeric(espn_rbproj$espn_recyd)
espn_rbproj$espn_rectd <- gsub("--", "0",espn_rbproj$espn_rectd)
espn_rbproj$espn_rectd <- as.numeric(espn_rbproj$espn_rectd)
espn_rbproj$espn_fpts <- as.numeric(espn_rbproj$espn_fpts)

#Seperate names and team
espn_rbproj$name <- str_sub(espn_rbproj$player, end=str_locate(string=espn_rbproj$player, ',')[,1]-1)

#espn_rbproj$name <- str_replace_all(espn_rbproj$name, "\\*", "")
espn_rbproj$espn_team <- str_sub(espn_rbproj$player, start=str_locate(string=espn_rbproj$player, ',')[,1]+1)
espn_rbproj$espn_team <- str_to_upper(str_sub(espn_rbproj$espn_team, end=str_locate(string=espn_rbproj$espn_team, ' ')[,1]+3))

#Fix names to match rest of sources
espn_rbproj[espn_rbproj$name=="Stevie Johnson", "name"] <- "Steve Johnson"
espn_rbproj[espn_rbproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
espn_rbproj$espn_fpts <- (espn_rbproj$espn_recyd*recYdsMultiplier) + (espn_rbproj$espn_rushyd*rushYdsMultiplier) + (espn_rbproj$espn_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  espn_rbproj$espn_fpts <- ifelse(espn_rbproj$espn_recyd >= RecydBonus,espn_rbproj$espn_fpts + Rydbonus, espn_rbproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_rbproj$espn_fpts <- ifelse(espn_rbproj$espn_recyd >= RecydBonus,espn_rbproj$espn_fpts + Rydbonus, espn_rbproj$espn_fpts)
}
#Rushing Yd
if(DailyFantasy == T){
  espn_rbproj$espn_fpts <- ifelse(espn_rbproj$espn_rushyd >= RushBonus,espn_rbproj$espn_fpts + Rbonus, espn_rbproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_rbproj$espn_fpts <- ifelse(espn_rbproj$espn_rushyd >= RushBonus,espn_rbproj$espn_fpts + Rbonus, espn_rbproj$espn_fpts)
}

#Add in TD probability
espn_rbproj$espn_rectd <- round(espn_rbproj$espn_rectd, 0)
espn_rbproj$espn_rushtd <- round(espn_rbproj$espn_rushtd, 0)
espn_rbproj$espn_fpts <- ifelse(espn_rbproj$espn_rectd >= 1,espn_rbproj$espn_fpts + (espn_rbproj$espn_rectd*recTdsMultiplier), espn_rbproj$espn_fpts)
espn_rbproj$espn_fpts <- ifelse(espn_rbproj$espn_rushtd >= 1,espn_rbproj$espn_fpts + (espn_rbproj$espn_rushtd*rushTdsMultiplier), espn_rbproj$espn_fpts)

#resort and reorder by name
espn_rbproj <- subset(espn_rbproj, select = c("name","espn_team","espn_rushatt","espn_rushyd","espn_rushtd",
                                              "espn_recpts","espn_recyd","espn_rectd","espn_fpts"))
espn_rbproj <- espn_rbproj[order(espn_rbproj$name),]

#FANTASY SHARKS ####

#FANTASY SHARKS
##Create data from urls

ffs_rbproj <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=rb&l=12&format=csv")

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
ffs_rbproj <- subset(ffs_rbproj, select = c(3,4,6:8,10:13))

#name the columns
names(ffs_rbproj) <- c("player","ffs_team","ffs_rushatt","ffs_rushyd","ffs_rushtd","ffs_recpts","ffs_recyd","ffs_rectd","ffs_fpts")

#convert to numeric
ffs_rbproj$ffs_rushatt <- as.numeric(ffs_rbproj$ffs_rushatt)
ffs_rbproj$ffs_rushyd <- as.numeric(ffs_rbproj$ffs_rushyd)
ffs_rbproj$ffs_rushtd <- as.numeric(ffs_rbproj$ffs_rushtd)
ffs_rbproj$ffs_recpts <- as.numeric(ffs_rbproj$ffs_recpts)
ffs_rbproj$ffs_recyd <- as.numeric(ffs_rbproj$ffs_recyd)
ffs_rbproj$ffs_rectd <- as.numeric(ffs_rbproj$ffs_rectd)
ffs_rbproj$ffs_fpts <- as.numeric(ffs_rbproj$ffs_fpts)

#Add Recpts for PPR
ffs_rbproj$ffs_fpts <- ffs_rbproj$ffs_fpts + ffs_rbproj$ffs_recpts

#convert name and team to Char
ffs_rbproj$player <- as.character(ffs_rbproj$player)
ffs_rbproj$ffs_team <- as.character(ffs_rbproj$ffs_team)

#Seprate names and  fix 2 Char teams
ffs_rbproj$last <- str_sub(ffs_rbproj$player, end=str_locate(string=ffs_rbproj$player, ' ')[,1]-2)
ffs_rbproj$first <- str_sub(ffs_rbproj$player, start=str_locate(string=ffs_rbproj$player,' ')[,1]+1)
ffs_rbproj$name <- paste(ffs_rbproj$first, ffs_rbproj$last)
ffs_rbproj[ffs_rbproj$ffs_team=="NOS", "ffs_team"] <- "NO"
ffs_rbproj[ffs_rbproj$ffs_team=="KCC", "ffs_team"] <- "KC"
ffs_rbproj[ffs_rbproj$ffs_team=="GBP", "ffs_team"] <- "GB"
ffs_rbproj[ffs_rbproj$ffs_team=="SDC", "ffs_team"] <- "SD"
ffs_rbproj[ffs_rbproj$ffs_team=="SFF", "ffs_team"] <- "SF"
ffs_rbproj[ffs_rbproj$ffs_team=="TBB", "ffs_team"] <- "TB"

#Fix names to match rest of sources

#Calculate points based on League Settings
ffs_rbproj$ffs_fpts <- (ffs_rbproj$ffs_recyd*recYdsMultiplier) + (ffs_rbproj$ffs_rushyd*rushYdsMultiplier) + (ffs_rbproj$ffs_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  ffs_rbproj$ffs_fpts <- ifelse(ffs_rbproj$ffs_recyd >= RecydBonus,ffs_rbproj$ffs_fpts + Rydbonus, ffs_rbproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_rbproj$ffs_fpts <- ifelse(ffs_rbproj$ffs_recyd >= RecydBonus,ffs_rbproj$ffs_fpts + Rydbonus, ffs_rbproj$ffs_fpts)
}
#Rushing Yd
if(DailyFantasy == T){
  ffs_rbproj$ffs_fpts <- ifelse(ffs_rbproj$ffs_rushyd >= RushBonus,ffs_rbproj$ffs_fpts + Rbonus, ffs_rbproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_rbproj$ffs_fpts <- ifelse(ffs_rbproj$ffs_rushyd >= RushBonus,ffs_rbproj$ffs_fpts + Rbonus, ffs_rbproj$ffs_fpts)
}

#Add in TD probability
ffs_rbproj$ffs_rectd <- round(ffs_rbproj$ffs_rectd, 0)
ffs_rbproj$ffs_rushtd <- round(ffs_rbproj$ffs_rushtd, 0)
ffs_rbproj$ffs_fpts <- ifelse(ffs_rbproj$ffs_rectd >= 1,ffs_rbproj$ffs_fpts + (ffs_rbproj$ffs_rectd*recTdsMultiplier), ffs_rbproj$ffs_fpts)
ffs_rbproj$ffs_fpts <- ifelse(ffs_rbproj$ffs_rushtd >= 1,ffs_rbproj$ffs_fpts + (ffs_rbproj$ffs_rushtd*rushTdsMultiplier), ffs_rbproj$ffs_fpts)



#resort data and reorder by name
ffs_rbproj <- subset(ffs_rbproj, select = c("name","ffs_team","ffs_rushatt","ffs_rushyd","ffs_rushtd","ffs_recpts","ffs_recyd","ffs_rectd","ffs_fpts"))
ffs_rbproj <- ffs_rbproj[order(ffs_rbproj$name),]

#FANTASYPRO ####

#Create data from urls
data_ranks <- readHTMLTable(url_fantasy_pro_ranks, as.data.frame=TRUE, stringsAsFactors=FALSE)$data
fp_rbproj <- readHTMLTable(url_fantasy_pro, as.data.frame=TRUE, stringsAsFactors=FALSE)$data

##CLEAN AND STANDARDIZE DATA##
#get rid of unwanted columns
data_ranks <- subset(data_ranks, select = c(1:2,4:7))

#clean headers
names(data_ranks) <- c("rank","player","best","worst","avg","sd")
names(fp_rbproj) <- c("player","fp_rushatt","fp_rushyd","fp_rushtd","fp_recpts","fp_recyd","fp_rectd","fp_fl","fp_fpts")

#remove non complete ranks
data_ranks <- data_ranks[complete.cases(data_ranks),]

#convert to numeric
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$avg <- as.numeric(data_ranks$avg)
data_ranks$sd <- as.numeric(data_ranks$sd)
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$best <- as.numeric(data_ranks$best)
data_ranks$worst <- as.numeric(data_ranks$worst)
fp_rbproj$fp_rushatt <- as.numeric(fp_rbproj$fp_rushatt)
fp_rbproj$fp_rushyd <- as.numeric(fp_rbproj$fp_rushyd)
fp_rbproj$fp_rushtd <- as.numeric(fp_rbproj$fp_rushtd)
fp_rbproj$fp_recpts <- as.numeric(fp_rbproj$fp_recpts)
fp_rbproj$fp_recyd <- as.numeric(fp_rbproj$fp_recyd)
fp_rbproj$fp_rectd <- as.numeric(fp_rbproj$fp_rectd)
fp_rbproj$fp_fpts <- as.numeric(fp_rbproj$fp_fpts)
fp_rbproj$fp_fl <- as.numeric(fp_rbproj$fp_fl)

##Seperate Name and Team
data_ranks$name1 <- str_replace_all(data_ranks$player, " ", ",")
data_ranks$first <- str_sub(data_ranks$name1, end=str_locate(string=data_ranks$name1, ',')[,1]-1)
data_ranks$last <- str_sub(data_ranks$name1,start=str_locate(string=data_ranks$name1, ',')[,1]+1)
data_ranks$last <- str_sub(data_ranks$last,end=str_locate(string=data_ranks$last, ',')[,1]-1)
data_ranks$name <- paste(data_ranks$first,data_ranks$last, sep = " ")

fp_rbproj$name1 <- str_replace_all(fp_rbproj$player, " ", ",")
fp_rbproj$fp_team <- str_sub(fp_rbproj$player, start = -3)
fp_rbproj$first <- str_sub(fp_rbproj$name1, end=str_locate(string=fp_rbproj$name1, ',')[,1]-1)
fp_rbproj$last <- str_sub(fp_rbproj$name1,start=str_locate(string=fp_rbproj$name1, ',')[,1]+1)
fp_rbproj$last <- str_sub(fp_rbproj$last,end=str_locate(string=fp_rbproj$last, ',')[,1]-1)
fp_rbproj$name <- paste(fp_rbproj$first,fp_rbproj$last, sep = " ")

#Fix Teams or names
#data_ranks[data_ranks$fp_team=="Q", "fp_team"] <- "SEA"

#Calculate points based on League Settings
fp_rbproj$fp_fpts <- (fp_rbproj$fp_recyd*recYdsMultiplier) + (fp_rbproj$fp_rushyd*rushYdsMultiplier) + (fp_rbproj$fp_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  fp_rbproj$fp_fpts <- ifelse(fp_rbproj$fp_recyd >= RecydBonus,fp_rbproj$fp_fpts + Rydbonus, fp_rbproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_rbproj$fp_fpts <- ifelse(fp_rbproj$fp_recyd >= RecydBonus,fp_rbproj$fp_fpts + Rydbonus, fp_rbproj$fp_fpts)
}
#Rushing Yd
if(DailyFantasy == T){
  fp_rbproj$fp_fpts <- ifelse(fp_rbproj$fp_rushyd >= RushBonus,fp_rbproj$fp_fpts + Rbonus, fp_rbproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_rbproj$fp_fpts <- ifelse(fp_rbproj$fp_rushyd >= RushBonus,fp_rbproj$fp_fpts + Rbonus, fp_rbproj$fp_fpts)
}

#Add in TD probability
fp_rbproj$fp_rectd <- round(fp_rbproj$fp_rectd, 0)
fp_rbproj$fp_rushtd <- round(fp_rbproj$fp_rushtd, 0)
fp_rbproj$fp_fpts <- ifelse(fp_rbproj$fp_rectd >= 1,fp_rbproj$fp_fpts + (fp_rbproj$fp_rectd*recTdsMultiplier), fp_rbproj$fp_fpts)
fp_rbproj$fp_fpts <- ifelse(fp_rbproj$fp_rushtd >= 1,fp_rbproj$fp_fpts + (fp_rbproj$fp_rushtd*rushTdsMultiplier), fp_rbproj$fp_fpts)

#resort and reorder by name
data_ranks <- subset(data_ranks,select = c("rank","name","best","worst","avg","sd"))
fp_rbproj <- subset(fp_rbproj, select = c("name","fp_team","fp_rushatt","fp_rushyd","fp_rushtd","fp_recpts","fp_recyd","fp_rectd",
                                          "fp_fl","fp_fpts"))
data_ranks <- data_ranks[order(data_ranks$name),]
fp_rbproj <- fp_rbproj[order(fp_rbproj$name),]

#FFTODAY ####

#Create data from urls
fftoday_rbproj <- readHTMLTable(url_fftoday, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
fftoday_rbproj1 <- readHTMLTable(url_fftoday1, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
fftoday_rbproj <- subset(fftoday_rbproj, select = c(2,3,5,6,7,8,9,10,11))
fftoday_rbproj1 <- subset(fftoday_rbproj1, select = c(2,3,5,6,7,8,9,10,11))
#clean headers
fftoday_rbproj <- fftoday_rbproj[2:(dim(fftoday_rbproj)[1]-1),]
fftoday_rbproj1 <- fftoday_rbproj1[2:(dim(fftoday_rbproj1)[1]-1),]
#name the columns
names(fftoday_rbproj) <- names(fftoday_rbproj1) <- c("player","fftoday_team","fftoday_rushatt","fftoday_rushyd","fftoday_rushtd",
                                                     "fftoday_recpts","fftoday_recyd","fftoday_rectd","fftoday_fpts")

#Merge and remove other dataframes to reduce clutter
fftoday_rbproj <- rbind(fftoday_rbproj, fftoday_rbproj1)
rm(fftoday_rbproj1)

#Convert to numeric
fftoday_rbproj$fftoday_rushatt <- as.numeric(fftoday_rbproj$fftoday_rushatt)
fftoday_rbproj$fftoday_rushyd <- as.numeric(fftoday_rbproj$fftoday_rushyd)
fftoday_rbproj$fftoday_rushtd <- as.numeric(fftoday_rbproj$fftoday_rushtd)
fftoday_rbproj$fftoday_recpts <- as.numeric(fftoday_rbproj$fftoday_recpts)
fftoday_rbproj$fftoday_recyd <- as.numeric(fftoday_rbproj$fftoday_recyd)
fftoday_rbproj$fftoday_rectd <- as.numeric(fftoday_rbproj$fftoday_rectd)
fftoday_rbproj$fftoday_fpts <- as.numeric(fftoday_rbproj$fftoday_fpts)

#remove symbol from fftoday name
fftoday_rbproj$name <- str_sub(fftoday_rbproj$player, start = +3)

#fix names
fftoday_rbproj[fftoday_rbproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."
fftoday_rbproj[fftoday_rbproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
fftoday_rbproj$fftoday_fpts <- (fftoday_rbproj$fftoday_recyd*recYdsMultiplier) + (fftoday_rbproj$fftoday_rushyd*rushYdsMultiplier) + (fftoday_rbproj$fftoday_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  fftoday_rbproj$fftoday_fpts <- ifelse(fftoday_rbproj$fftoday_recyd >= RecydBonus,fftoday_rbproj$fftoday_fpts + Rydbonus, fftoday_rbproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_rbproj$fftoday_fpts <- ifelse(fftoday_rbproj$fftoday_recyd >= RecydBonus,fftoday_rbproj$fftoday_fpts + Rydbonus, fftoday_rbproj$fftoday_fpts)
}
#Rushing Yd
if(DailyFantasy == T){
  fftoday_rbproj$fftoday_fpts <- ifelse(fftoday_rbproj$fftoday_rushyd >= RushBonus,fftoday_rbproj$fftoday_fpts + Rbonus, fftoday_rbproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_rbproj$fftoday_fpts <- ifelse(fftoday_rbproj$fftoday_rushyd >= RushBonus,fftoday_rbproj$fftoday_fpts + Rbonus, fftoday_rbproj$fftoday_fpts)
}

#Add in TD probability
fftoday_rbproj$fftoday_rectd <- round(fftoday_rbproj$fftoday_rectd, 0)
fftoday_rbproj$fftoday_rushtd <- round(fftoday_rbproj$fftoday_rushtd, 0)
fftoday_rbproj$fftoday_fpts <- ifelse(fftoday_rbproj$fftoday_rectd >= 1,fftoday_rbproj$fftoday_fpts + (fftoday_rbproj$fftoday_rectd*recTdsMultiplier), fftoday_rbproj$fftoday_fpts)
fftoday_rbproj$fftoday_fpts <- ifelse(fftoday_rbproj$fftoday_rushtd >= 1,fftoday_rbproj$fftoday_fpts + (fftoday_rbproj$fftoday_rushtd*rushTdsMultiplier), fftoday_rbproj$fftoday_fpts)

#resort and order by name
fftoday_rbproj <- subset(fftoday_rbproj, select = c("name","fftoday_team","fftoday_rushatt","fftoday_rushyd","fftoday_rushtd",
                                                    "fftoday_recpts","fftoday_recyd","fftoday_rectd","fftoday_fpts"))
fftoday_rbproj <- fftoday_rbproj[order(fftoday_rbproj$name),]

#NUMBERFIRE #####

#NUMBERFIRE
##Create data from urls
numfire_html <- htmlParse(url_numfire)
numfire_html <- readHTMLTable(numfire_html)
numfire_rbproj <- do.call(rbind.data.frame, numfire_html)
rm(numfire_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
numfire_rbproj <- subset(numfire_rbproj, select = c(1,5:11,20,21))

#name the columns
names(numfire_rbproj) <- c("player","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts","numfire_dkcost")

#convert to Character so we dont remove the Decimal later
numfire_rbproj$player <- as.character(numfire_rbproj$player)
numfire_rbproj$numfire_rushatt <- as.character(numfire_rbproj$numfire_rushatt)
numfire_rbproj$numfire_rushyd <- as.character(numfire_rbproj$numfire_rushyd)
numfire_rbproj$numfire_rushtd <- as.character(numfire_rbproj$numfire_rushtd)
numfire_rbproj$numfire_recpts <- as.character(numfire_rbproj$numfire_recpts)
numfire_rbproj$numfire_recyd <- as.character(numfire_rbproj$numfire_recyd)
numfire_rbproj$numfire_rectd <- as.character(numfire_rbproj$numfire_rectd)
numfire_rbproj$numfire_fpts <- as.character(numfire_rbproj$numfire_fpts)

#convert to numeric
numfire_rbproj$numfire_rushatt <- as.numeric(numfire_rbproj$numfire_rushatt)
numfire_rbproj$numfire_rushyd <- as.numeric(numfire_rbproj$numfire_rushyd)
numfire_rbproj$numfire_rushtd <- as.numeric(numfire_rbproj$numfire_rushtd)
numfire_rbproj$numfire_recpts <- as.numeric(numfire_rbproj$numfire_recpts)
numfire_rbproj$numfire_recyd <- as.numeric(numfire_rbproj$numfire_recyd)
numfire_rbproj$numfire_rectd <- as.numeric(numfire_rbproj$numfire_rectd)
numfire_rbproj$numfire_fpts <- as.numeric(numfire_rbproj$numfire_fpts)

#Seprate names and team
numfire_rbproj$numfire_team <- str_sub(numfire_rbproj$player, start=str_locate(string=numfire_rbproj$player,',')[,1]+2)
numfire_rbproj$numfire_team <- gsub(pattern = ")", replacement = "", x = numfire_rbproj$numfire_team)
numfire_rbproj$name <- str_sub(numfire_rbproj$player, end=str_locate(string=numfire_rbproj$player,',')[,1]-5)

#Fix names to match rest of sources

#Calculate points based on League Settings
numfire_rbproj$numfire_fpts <- (numfire_rbproj$numfire_recyd*recYdsMultiplier) + (numfire_rbproj$numfire_rushyd*rushYdsMultiplier) + (numfire_rbproj$numfire_recpts*recMultiplier)

#Yardage bonus
#Rec Yds
if(DailyFantasy == T){
  numfire_rbproj$numfire_fpts <- ifelse(numfire_rbproj$numfire_recyd >= RecydBonus,numfire_rbproj$numfire_fpts + Rydbonus, numfire_rbproj$numfire_fpts)
}
if(DailyFantasy == F){
  numfire_rbproj$numfire_fpts <- ifelse(numfire_rbproj$numfire_recyd >= RecydBonus,numfire_rbproj$numfire_fpts + Rydbonus, numfire_rbproj$numfire_fpts)
}
#Rushing Yd
if(DailyFantasy == T){
  numfire_rbproj$numfire_fpts <- ifelse(numfire_rbproj$numfire_rushyd >= RushBonus,numfire_rbproj$numfire_fpts + Rbonus, numfire_rbproj$numfire_fpts)
}
if(DailyFantasy == F){
  numfire_rbproj$numfire_fpts <- ifelse(numfire_rbproj$numfire_rushyd >= RushBonus,numfire_rbproj$numfire_fpts + Rbonus, numfire_rbproj$numfire_fpts)
}

#Add in TD probability
numfire_rbproj$numfire_rectd <- round(numfire_rbproj$numfire_rectd, 0)
numfire_rbproj$numfire_rushtd <- round(numfire_rbproj$numfire_rushtd, 0)
numfire_rbproj$numfire_fpts <- ifelse(numfire_rbproj$numfire_rectd >= 1,numfire_rbproj$numfire_fpts + (numfire_rbproj$numfire_rectd*recTdsMultiplier), numfire_rbproj$numfire_fpts)
numfire_rbproj$numfire_fpts <- ifelse(numfire_rbproj$numfire_rushtd >= 1,numfire_rbproj$numfire_fpts + (numfire_rbproj$numfire_rushtd*rushTdsMultiplier), numfire_rbproj$numfire_fpts)

#resort data and reorder by name
numfire_rbproj <- subset(numfire_rbproj, select = c("name","numfire_team","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd",
                                                    "numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts","numfire_dkcost"))
numfire_rbproj <- numfire_rbproj[order(numfire_rbproj$name),]

#MERGE DATA & CALCULATIONS #####

##Create csv for Value Gap Analysis##
#need recyards, rec, rectd

##MERGE DATA##
projections <- merge(cbs_rbproj, espn_rbproj, by="name", all.x = T)
projections <- merge(projections, fftoday_rbproj, by="name", all.x = T)
projections <- merge(projections, ffs_rbproj, by="name", all.x = T)
projections <- merge(projections, fp_rbproj, by="name", all.x = T)
projections <- merge(projections, numfire_rbproj, by="name", all.x = T)

projections <- projections[order(projections$name),]

rbVGArecyd <- subset(projections, select = c("name","cbs_recyd","espn_recyd","ffs_recyd","fp_recyd","fftoday_recyd","numfire_recyd"))
columns_rbVGArecyd <- ncol(rbVGArecyd)
rbVGArecyd$recYds <- apply(rbVGArecyd[2:columns_rbVGArecyd], 1, mean, na.rm=TRUE)
rbVGArecyd$recYds <- round(rbVGArecyd$recYds, digits = 1)
rbVGArecyd <- subset(rbVGArecyd, select = c("name","recYds"))

rbVGArectd <- subset(projections, select = c("name","cbs_rectd","espn_rectd","ffs_rectd","fp_rectd","fftoday_rectd","numfire_rectd"))
rbVGArectd$recTds <- apply(rbVGArectd[2:columns_rbVGArecyd], 1, mean, na.rm=TRUE)
rbVGArectd$recTds <- round(rbVGArectd$recTds, digits = 1)
rbVGArectd <- subset(rbVGArectd, select = c("recTds"))

rbVGArec <- subset(projections, select = c("name","cbs_recpts","espn_recpts","ffs_recpts","fp_recpts","fftoday_recpts","numfire_recpts"))
rbVGArec$rec <- apply(rbVGArec[2:columns_rbVGArecyd], 1, mean, na.rm=TRUE)
rbVGArec$rec <- round(rbVGArec$rec, digits = 1)
rbVGArec <- subset(rbVGArec, select = c("rec"))

rbVGA <- cbind(rbVGArecyd,rbVGArectd,rbVGArec)

write.csv(rbVGA, file=paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Running Back.csv", sep=""), row.names=FALSE)

#Redo the merge so projections are only ones with all sources

projections <- merge(cbs_rbproj, espn_rbproj, by="name", all.x = F)
projections <- merge(projections, fftoday_rbproj, by="name", all.x = F)
projections <- merge(projections, ffs_rbproj, by="name", all.x = F)
projections <- merge(projections, fp_rbproj, by="name", all.x = F)
projections <- merge(projections, numfire_rbproj, by="name", all.x = F)

projections <- projections[order(projections$name),]

##Calculations##

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","numfire_dkcost","cbs_fpts","espn_fpts","ffs_fpts","fftoday_fpts","fp_fpts","numfire_fpts"))
fantasyproj$cbs_fpts <- as.numeric(fantasyproj$cbs_fpts)
fantasyproj$espn_fpts <- as.numeric(fantasyproj$espn_fpts)
fantasyproj$fftoday_fpts <- as.numeric(fantasyproj$fftoday_fpts)
fantasyproj$ffs_fpts <- as.numeric(fantasyproj$ffs_fpts)
fantasyproj$fp_fpts <- as.numeric(fantasyproj$fp_fpts)
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
    labs(title = paste("Week ", week , " Running Back Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == F && ppr == T){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/Week ",week," Running Back.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/PPR/Week ",week," Running Back.jpg", sep=""), width=10, height=10)
  #Save Data
  save(fantasyproj, file = paste(getwd(),"/Data/Weekly/PPR/Week",week," Running Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/PPR/Week",week," Running Back-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Running Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Running Back-fantasyproj.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," rbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," rbprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," rbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," rbprojections.html",sep=""))
  }
if(DailyFantasy == F && ppr == F){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/Week ",week," Running Back.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/STD/Week ",week," Running Back.jpg", sep=""), width=10, height=10)
  #Save Data
  save(projections, file = paste(getwd(),"/Data/Weekly/STD/Week",week,"-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Weekly/STD/Week",week,"-projections.csv", sep=""), row.names=FALSE)
  save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"-projections.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," rbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," rbprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," rbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," rbprojections.html",sep=""))
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
    labs(title = paste("Week ", week , " Running Back Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Running Back.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Running Back.jpg", sep=""), width=10, height=10)
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
    labs(title = paste("Week ", week , " Running Back Projections vs Cost", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Running Back Cost.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Running Back Cost.jpg", sep=""), width=10, height=10)
  #Save file
  save(fantasyproj, file = paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Running Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Running Back-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Running Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Running Back-fantasyproj.csv", sep=""), row.names=FALSE)
  #save the html file
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," rbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," rbprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," rbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," rbprojections.html",sep=""))
}
