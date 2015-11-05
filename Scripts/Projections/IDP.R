#Weekly Projections

#DATA SETTINGS ####

#URLS ####
url_cbs <- paste("http://www.cbssports.com/fantasy/football/stats/weeklyprojections/QB/", week, "/avg/ppr?&print_rows=9999", sep="")
url_espn <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=0&leagueId=",espn_std,sep="")
url_fftoday <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=10,&LeagueID=", fft_espnSTD, sep="")
url_fftoday1 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=10,&LeagueID=", fft_espnSTD,"&order_by=FFPts&sort_order=DESC&cur_page=1", sep="")
url_fantasy_pro_ranks <- ("http://www.fantasypros.com/nfl/rankings/qb.php")
url_fantasy_pro <- ("http://www.fantasypros.com/nfl/projections/qb.php")
url_numfire <- paste("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/qb")

##STANDARIZATION TIME##

#CBS ####
##Create data from urls
cbs_html <- htmlParse(url_cbs)
cbs_html <- readHTMLTable(cbs_html)
cbs_qbproj <- do.call(rbind.data.frame, cbs_html)
rm(cbs_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
cbs_qbproj <- subset(cbs_qbproj, select = c(1:6,9:10,12:14))
#clean headers
cbs_qbproj <- cbs_qbproj[3:(dim(cbs_qbproj)[1]-1),]
#name the columns
names(cbs_qbproj) <- c("player","cbs_passatt","cbs_passcomp","cbs_passyd","cbs_passtd",
                       "cbs_int","cbs_rushatt","cbs_rushyd","cbs_rushtd","cbs_fl","cbs_fpts")

#convert to Character so we dont remove the Decimal later
cbs_qbproj$cbs_passatt <- as.character(cbs_qbproj$cbs_passatt)
cbs_qbproj$cbs_passcomp <- as.character(cbs_qbproj$cbs_passcomp)
cbs_qbproj$cbs_passyd <- as.character(cbs_qbproj$cbs_passyd)
cbs_qbproj$cbs_passtd <- as.character(cbs_qbproj$cbs_passtd)
cbs_qbproj$cbs_int <- as.character(cbs_qbproj$cbs_int)
cbs_qbproj$cbs_rushatt <- as.character(cbs_qbproj$cbs_rushatt)
cbs_qbproj$cbs_rushyd <- as.character(cbs_qbproj$cbs_rushyd)
cbs_qbproj$cbs_rushtd <- as.character(cbs_qbproj$cbs_rushtd)
cbs_qbproj$cbs_fl <- as.character(cbs_qbproj$cbs_fl)
cbs_qbproj$cbs_fpts <- as.character(cbs_qbproj$cbs_fpts)

#convert to numeric
cbs_qbproj$cbs_passatt <- as.numeric(cbs_qbproj$cbs_passatt)
cbs_qbproj$cbs_passcomp <- as.numeric(cbs_qbproj$cbs_passcomp)
cbs_qbproj$cbs_passyd <- as.numeric(cbs_qbproj$cbs_passyd)
cbs_qbproj$cbs_passtd <- as.numeric(cbs_qbproj$cbs_passtd)
cbs_qbproj$cbs_int <- as.numeric(cbs_qbproj$cbs_int)
cbs_qbproj$cbs_rushatt <- as.numeric(cbs_qbproj$cbs_rushatt)
cbs_qbproj$cbs_rushyd <- as.numeric(cbs_qbproj$cbs_rushyd)
cbs_qbproj$cbs_rushtd <- as.numeric(cbs_qbproj$cbs_rushtd)
cbs_qbproj$cbs_fl <- as.numeric(cbs_qbproj$cbs_fl)
cbs_qbproj$cbs_fpts <- as.numeric(cbs_qbproj$cbs_fpts)

#Seprate names and team
cbs_qbproj$name <- str_sub(cbs_qbproj$player, end=str_locate(string=cbs_qbproj$player, ',')[,1]-1)
cbs_qbproj$cbs_team <- str_trim(str_sub(cbs_qbproj$player, start= -3))

#Fix names to match rest of sources
cbs_qbproj[cbs_qbproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."

#Calculate points based on League Settings
cbs_qbproj$cbs_fpts <- (cbs_qbproj$cbs_passyd*passYdsMultiplier) + (cbs_qbproj$cbs_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_passyd >= PassBonus,cbs_qbproj$cbs_fpts + Pbonus, cbs_qbproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_passyd >= PassBonus,cbs_qbproj$cbs_fpts + Pbonus, cbs_qbproj$cbs_fpts)
}

if(DailyFantasy == T){
  cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_rushyd >= RushBonus,cbs_qbproj$cbs_fpts + Rbonus, cbs_qbproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_rushyd >= RushBonus,cbs_qbproj$cbs_fpts + Rbonus, cbs_qbproj$cbs_fpts)
}

#Add in TD probability
cbs_qbproj$cbs_passtd <- round(cbs_qbproj$cbs_passtd, 0)
cbs_qbproj$cbs_rushtd <- round(cbs_qbproj$cbs_rushtd, 0)
cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_passtd >= 1,cbs_qbproj$cbs_fpts + (cbs_qbproj$cbs_passtd*passTdsMultiplier), cbs_qbproj$cbs_fpts)
cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_rushtd >= 1,cbs_qbproj$cbs_fpts + (cbs_qbproj$cbs_rushtd*rushTdsMultiplier), cbs_qbproj$cbs_fpts)

#Interception Probability
cbs_qbproj$cbs_int <- round(cbs_qbproj$cbs_int)
cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_int >= 1,cbs_qbproj$cbs_fpts + (cbs_qbproj$cbs_int*passIntMultiplier), cbs_qbproj$cbs_fpts)

#Remove all insginicant projections
cbs_qbproj1 <- subset(cbs_qbproj, cbs_qbproj$cbs_passatt > .99)
cbs_qbproj <- cbs_qbproj1
rm(cbs_qbproj1)

#resort data and reorder by name
cbs_qbproj <- subset(cbs_qbproj, select = c("name","cbs_team","cbs_passatt","cbs_passcomp","cbs_passyd","cbs_passtd",
                                            "cbs_int","cbs_rushatt","cbs_rushyd","cbs_rushtd","cbs_fl","cbs_fpts"))
cbs_qbproj <- cbs_qbproj[order(cbs_qbproj$name),]

#ESPN #####
#ESPN
##Create data from urls
espn_qbproj <- readHTMLTable(url_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
espn_qbproj <- subset(espn_qbproj, select = c(1,4:10,14))

#clean headers
espn_qbproj <- espn_qbproj[2:(dim(espn_qbproj)[1]),]

#name the columns
names(espn_qbproj) <- c("player","espn_ca","espn_passyd","espn_passtd",
                        "espn_int","espn_rushatt","espn_rushyd","espn_rushtd","espn_fpts")

#Split Completions and attemps
espn_qbproj$espn_passatt <- str_sub(espn_qbproj$espn_ca, end=str_locate(string=espn_qbproj$espn_ca, '/')[,1]-1)
espn_qbproj$espn_passcomp <- str_sub(espn_qbproj$espn_ca, start=str_locate(string=espn_qbproj$espn_ca, '/')[,1]+1)

#convert to numeric
espn_qbproj$espn_passatt <- as.numeric(espn_qbproj$espn_passatt)
espn_qbproj$espn_passcomp <- as.numeric(espn_qbproj$espn_passcomp)
espn_qbproj$espn_passyd <- as.numeric(espn_qbproj$espn_passyd)
espn_qbproj$espn_passtd <- as.numeric(espn_qbproj$espn_passtd)
espn_qbproj$espn_int <- as.numeric(espn_qbproj$espn_int)
espn_qbproj$espn_rushatt <- as.numeric(espn_qbproj$espn_rushatt)
espn_qbproj$espn_rushyd <- as.numeric(espn_qbproj$espn_rushyd)
espn_qbproj$espn_rushtd <- as.numeric(espn_qbproj$espn_rushtd)
espn_qbproj$espn_fpts <- as.numeric(espn_qbproj$espn_fpts)

#Seperate names and team
espn_qbproj$name <- str_sub(espn_qbproj$player, end=str_locate(string=espn_qbproj$player, ',')[,1]-1)

#espn_qbproj$name <- str_replace_all(espn_qbproj$name, "\\*", "")
espn_qbproj$espn_team <- str_sub(espn_qbproj$player, start=str_locate(string=espn_qbproj$player, ',')[,1]+1)
espn_qbproj$espn_team <- str_to_upper(str_sub(espn_qbproj$espn_team, end=str_locate(string=espn_qbproj$espn_team, ' ')[,1]+3))

#Fix names to match rest of sources
espn_qbproj[espn_qbproj$name=="Stevie Johnson", "name"] <- "Steve Johnson"
espn_qbproj[espn_qbproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
espn_qbproj$espn_fpts <- (espn_qbproj$espn_passyd*passYdsMultiplier) + (espn_qbproj$espn_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_passyd >= PassBonus,espn_qbproj$espn_fpts + Pbonus, espn_qbproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_passyd >= PassBonus,espn_qbproj$espn_fpts + Pbonus, espn_qbproj$espn_fpts)
}

if(DailyFantasy == T){
  espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_rushyd >= RushBonus,espn_qbproj$espn_fpts + Rbonus, espn_qbproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_rushyd >= RushBonus,espn_qbproj$espn_fpts + Rbonus, espn_qbproj$espn_fpts)
}

#Add in TD probability
espn_qbproj$espn_passtd <- round(espn_qbproj$espn_passtd, 0)
espn_qbproj$espn_rushtd <- round(espn_qbproj$espn_rushtd, 0)
espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_passtd >= 1,espn_qbproj$espn_fpts + (espn_qbproj$espn_passtd*passTdsMultiplier), espn_qbproj$espn_fpts)
espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_rushtd >= 1,espn_qbproj$espn_fpts + (espn_qbproj$espn_rushtd*rushTdsMultiplier), espn_qbproj$espn_fpts)

#Interception Probability
espn_qbproj$espn_int <- round(espn_qbproj$espn_int)
espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_int >= 1,espn_qbproj$espn_fpts + (espn_qbproj$espn_int*passIntMultiplier), espn_qbproj$espn_fpts)

#Remove all insginicant projections
espn_qbproj1 <- subset(espn_qbproj, espn_qbproj$espn_passatt > .99)
espn_qbproj <- espn_qbproj1
rm(espn_qbproj1)

#resort and reorder by name
espn_qbproj <- subset(espn_qbproj, select = c("name","espn_team","espn_passatt","espn_passcomp","espn_passyd","espn_passtd",
                                              "espn_int","espn_rushatt","espn_rushyd","espn_rushtd","espn_fpts"))
espn_qbproj <- espn_qbproj[order(espn_qbproj$name),]

#FANTASY SHARKS ####

#FANTASY SHARKS
##Create data from urls
ffs_qbproj <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=qb&l=12&format=csv")

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
ffs_qbproj <- subset(ffs_qbproj, select = c(3,4,6:9,11:13))

#name the columns
names(ffs_qbproj) <- c("player","ffs_team","ffs_passcomp","ffs_passyd","ffs_passtd","ffs_int","ffs_rushyd","ffs_rushtd","ffs_fpts")

#convert name and team to Char
ffs_qbproj$player <- as.character(ffs_qbproj$player)
ffs_qbproj$ffs_team <- as.character(ffs_qbproj$ffs_team)

#Seprate names and  fix 2 Char teams
ffs_qbproj$last <- str_sub(ffs_qbproj$player, end=str_locate(string=ffs_qbproj$player, ' ')[,1]-2)
ffs_qbproj$first <- str_sub(ffs_qbproj$player, start=str_locate(string=ffs_qbproj$player,' ')[,1]+1)
ffs_qbproj$name <- paste(ffs_qbproj$first, ffs_qbproj$last)
ffs_qbproj[ffs_qbproj$ffs_team=="NOS", "ffs_team"] <- "NO"
ffs_qbproj[ffs_qbproj$ffs_team=="KCC", "ffs_team"] <- "KC"
ffs_qbproj[ffs_qbproj$ffs_team=="GBP", "ffs_team"] <- "GB"
ffs_qbproj[ffs_qbproj$ffs_team=="SDC", "ffs_team"] <- "SD"
ffs_qbproj[ffs_qbproj$ffs_team=="SFO", "ffs_team"] <- "SF"
ffs_qbproj[ffs_qbproj$ffs_team=="TBB", "ffs_team"] <- "TB"

#Fix names to match rest of sources

#Calculate points based on League Settings
ffs_qbproj$ffs_fpts <- (ffs_qbproj$ffs_passyd*passYdsMultiplier) + (ffs_qbproj$ffs_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_passyd >= PassBonus,ffs_qbproj$ffs_fpts + Pbonus, ffs_qbproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_passyd >= PassBonus,ffs_qbproj$ffs_fpts + Pbonus, ffs_qbproj$ffs_fpts)
}

if(DailyFantasy == T){
  ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_rushyd >= RushBonus,ffs_qbproj$ffs_fpts + Rbonus, ffs_qbproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_rushyd >= RushBonus,ffs_qbproj$ffs_fpts + Rbonus, ffs_qbproj$ffs_fpts)
}

#Add in TD probability
ffs_qbproj$ffs_passtd <- round(ffs_qbproj$ffs_passtd, 0)
ffs_qbproj$ffs_rushtd <- round(ffs_qbproj$ffs_rushtd, 0)
ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_passtd >= 1,ffs_qbproj$ffs_fpts + (ffs_qbproj$ffs_passtd*passTdsMultiplier), ffs_qbproj$ffs_fpts)
ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_rushtd >= 1,ffs_qbproj$ffs_fpts + (ffs_qbproj$ffs_rushtd*rushTdsMultiplier), ffs_qbproj$ffs_fpts)
                                                            
#Interception Probability
ffs_qbproj$ffs_int <- round(ffs_qbproj$ffs_int)
ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_int >= 1,ffs_qbproj$ffs_fpts + (ffs_qbproj$ffs_int*passIntMultiplier), ffs_qbproj$ffs_fpts)
                                                            
#resort data and reorder by name
ffs_qbproj <- subset(ffs_qbproj, select = c("name","ffs_team","ffs_passcomp","ffs_passyd","ffs_passtd","ffs_int","ffs_rushyd","ffs_rushtd","ffs_fpts"))
ffs_qbproj <- ffs_qbproj[order(ffs_qbproj$name),]

#FFTODAY ####

#FFTODAY
#Create data from urls
fftoday_qbproj <- readHTMLTable(url_fftoday, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns##
fftoday_qbproj <- subset(fftoday_qbproj, select = c(2:3,5:13))
#clean headers
fftoday_qbproj <- fftoday_qbproj[2:(dim(fftoday_qbproj)[1]-1),]
#name the columns
names(fftoday_qbproj) <- c("player","fftoday_team","fftoday_passcomp","fftoday_passatt","fftoday_passyd","fftoday_passtd",
                           "fftoday_int","fftoday_rushatt","fftoday_rushyd","fftoday_rushtd","fftoday_fpts")

#Convert to numeric
fftoday_qbproj$fftoday_passcomp <- as.numeric(fftoday_qbproj$fftoday_passcomp)
fftoday_qbproj$fftoday_passatt <- as.numeric(fftoday_qbproj$fftoday_passatt)
fftoday_qbproj$fftoday_passyd <- as.numeric(fftoday_qbproj$fftoday_passyd)
fftoday_qbproj$fftoday_passtd <- as.numeric(fftoday_qbproj$fftoday_passtd)
fftoday_qbproj$fftoday_int <- as.numeric(fftoday_qbproj$fftoday_int)
fftoday_qbproj$fftoday_rushatt <- as.numeric(fftoday_qbproj$fftoday_rushatt)
fftoday_qbproj$fftoday_rushyd <- as.numeric(fftoday_qbproj$fftoday_rushyd)
fftoday_qbproj$fftoday_rushtd <- as.numeric(fftoday_qbproj$fftoday_rushtd)
fftoday_qbproj$fftoday_fpts <- as.numeric(fftoday_qbproj$fftoday_fpts)

#remove symbol from fftoday name
fftoday_qbproj$name <- str_sub(fftoday_qbproj$player, start = +3)

#fix names
fftoday_qbproj[fftoday_qbproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."
fftoday_qbproj[fftoday_qbproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
fftoday_qbproj$fftoday_fpts <- (fftoday_qbproj$fftoday_passyd*passYdsMultiplier) + (fftoday_qbproj$fftoday_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  fftoday_qbproj$fftoday_fpts <- ifelse(fftoday_qbproj$fftoday_passyd >= PassBonus,fftoday_qbproj$fftoday_fpts + Pbonus, fftoday_qbproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_qbproj$fftoday_fpts <- ifelse(fftoday_qbproj$fftoday_passyd >= PassBonus,fftoday_qbproj$fftoday_fpts + Pbonus, fftoday_qbproj$fftoday_fpts)
}

if(DailyFantasy == T){
  fftoday_qbproj$fftoday_fpts <- ifelse(fftoday_qbproj$fftoday_rushyd >= RushBonus,fftoday_qbproj$fftoday_fpts + Rbonus, fftoday_qbproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_qbproj$fftoday_fpts <- ifelse(fftoday_qbproj$fftoday_rushyd >= RushBonus,fftoday_qbproj$fftoday_fpts + Rbonus, fftoday_qbproj$fftoday_fpts)
}

#Add in TD probability
fftoday_qbproj$fftoday_passtd <- round(fftoday_qbproj$fftoday_passtd, 0)
fftoday_qbproj$fftoday_rushtd <- round(fftoday_qbproj$fftoday_rushtd, 0)
fftoday_qbproj$fftoday_fpts <- ifelse(fftoday_qbproj$fftoday_passtd >= 1,fftoday_qbproj$fftoday_fpts + (fftoday_qbproj$fftoday_passtd*passTdsMultiplier), fftoday_qbproj$fftoday_fpts)
fftoday_qbproj$fftoday_fpts <- ifelse(fftoday_qbproj$fftoday_rushtd >= 1,fftoday_qbproj$fftoday_fpts + (fftoday_qbproj$fftoday_rushtd*rushTdsMultiplier), fftoday_qbproj$fftoday_fpts)

#Interception Probability
fftoday_qbproj$fftoday_int <- round(fftoday_qbproj$fftoday_int)
fftoday_qbproj$fftoday_fpts <- ifelse(fftoday_qbproj$fftoday_int >= 1,fftoday_qbproj$fftoday_fpts + (fftoday_qbproj$fftoday_int*passIntMultiplier), fftoday_qbproj$fftoday_fpts)

#Remove all insginicant projections
fftoday_qbproj <- subset(fftoday_qbproj, fftoday_qbproj$fftoday_passatt > .99)

#resort and order by name
fftoday_qbproj <- subset(fftoday_qbproj, select = c("name","fftoday_team","fftoday_team","fftoday_passcomp","fftoday_passatt","fftoday_passyd","fftoday_passtd",
                                                    "fftoday_int","fftoday_rushatt","fftoday_rushyd","fftoday_rushtd","fftoday_fpts"))
fftoday_qbproj <- fftoday_qbproj[order(fftoday_qbproj$name),]

#FANTASYPRO ####

#FANTASY PROS Ranks & Projections
#Create data from urls
data_ranks <- readHTMLTable(url_fantasy_pro_ranks, as.data.frame=TRUE, stringsAsFactors=FALSE)$data
fp_qbproj <- readHTMLTable(url_fantasy_pro, as.data.frame=TRUE, stringsAsFactors=FALSE)$data

##CLEAN AND STANDARDIZE DATA##
#get rid of unwanted columns
data_ranks <- subset(data_ranks, select = c(1:2,4:7))

#clean headers
names(data_ranks) <- c("rank","player","best","worst","avg","sd")
names(fp_qbproj) <- c("player","fp_passatt","fp_passcomp","fp_passyd","fp_passtd",
                      "fp_int","fp_rushatt","fp_rushyd","fp_rushtd","fp_fl","fp_fpts")

#remove non complete ranks
data_ranks <- data_ranks[complete.cases(data_ranks),]

#convert to numeric
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$avg <- as.numeric(data_ranks$avg)
data_ranks$sd <- as.numeric(data_ranks$sd)
data_ranks$rank <- as.numeric(data_ranks$rank)
data_ranks$best <- as.numeric(data_ranks$best)
data_ranks$worst <- as.numeric(data_ranks$worst)

fp_qbproj$fp_passatt <- as.numeric(fp_qbproj$fp_passatt)
fp_qbproj$fp_passcomp <- as.numeric(fp_qbproj$fp_passcomp)
fp_qbproj$fp_passyd <- as.numeric(fp_qbproj$fp_passyd)
fp_qbproj$fp_passtd <- as.numeric(fp_qbproj$fp_passtd)
fp_qbproj$fp_int <- as.numeric(fp_qbproj$fp_int)
fp_qbproj$fp_rushatt <- as.numeric(fp_qbproj$fp_rushatt)
fp_qbproj$fp_rushyd <- as.numeric(fp_qbproj$fp_rushyd)
fp_qbproj$fp_rushtd <- as.numeric(fp_qbproj$fp_rushtd)
fp_qbproj$fp_fl <- as.numeric(fp_qbproj$fp_fl)
fp_qbproj$fp_fpts <- as.numeric(fp_qbproj$fp_fpts)

##Seperate Name and Team
data_ranks$name1 <- str_replace_all(data_ranks$player, " ", ",")
data_ranks$first <- str_sub(data_ranks$name1, end=str_locate(string=data_ranks$name1, ',')[,1]-1)
data_ranks$last <- str_sub(data_ranks$name1,start=str_locate(string=data_ranks$name1, ',')[,1]+1)
data_ranks$last <- str_sub(data_ranks$last,end=str_locate(string=data_ranks$last, ',')[,1]-1)
data_ranks$name <- paste(data_ranks$first,data_ranks$last, sep = " ")

fp_qbproj$name1 <- str_replace_all(fp_qbproj$player, " ", ",")
fp_qbproj$fp_team <- str_sub(fp_qbproj$player, start = -3)
fp_qbproj$first <- str_sub(fp_qbproj$name1, end=str_locate(string=fp_qbproj$name1, ',')[,1]-1)
fp_qbproj$last <- str_sub(fp_qbproj$name1,start=str_locate(string=fp_qbproj$name1, ',')[,1]+1)
fp_qbproj$last <- str_sub(fp_qbproj$last,end=str_locate(string=fp_qbproj$last, ',')[,1]-1)
fp_qbproj$name <- paste(fp_qbproj$first,fp_qbproj$last, sep = " ")

#Fix Teams or names

#Calculate points based on League Settings
fp_qbproj$fp_fpts <- (fp_qbproj$fp_passyd*passYdsMultiplier) + (fp_qbproj$fp_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_passyd >= PassBonus,fp_qbproj$fp_fpts + Pbonus, fp_qbproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_passyd >= PassBonus,fp_qbproj$fp_fpts + Pbonus, fp_qbproj$fp_fpts)
}

if(DailyFantasy == T){
  fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_rushyd >= RushBonus,fp_qbproj$fp_fpts + Rbonus, fp_qbproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_rushyd >= RushBonus,fp_qbproj$fp_fpts + Rbonus, fp_qbproj$fp_fpts)
}

#Add in TD probability
fp_qbproj$fp_passtd <- round(fp_qbproj$fp_passtd, 0)
fp_qbproj$fp_rushtd <- round(fp_qbproj$fp_rushtd, 0)
fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_passtd >= 1,fp_qbproj$fp_fpts + (fp_qbproj$fp_passtd*passTdsMultiplier), fp_qbproj$fp_fpts)
fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_rushtd >= 1,fp_qbproj$fp_fpts + (fp_qbproj$fp_rushtd*rushTdsMultiplier), fp_qbproj$fp_fpts)

#Interception Probability
fp_qbproj$fp_int <- round(fp_qbproj$fp_int)
fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_int >= 1,fp_qbproj$fp_fpts + (fp_qbproj$fp_int*passIntMultiplier), fp_qbproj$fp_fpts)

#Remove all insginicant projections
fp_qbproj1 <- subset(fp_qbproj, fp_qbproj$fp_passatt > .99)
fp_qbproj <- fp_qbproj1
rm(fp_qbproj1)
#resort and reorder by name
data_ranks <- subset(data_ranks,select = c("rank","name","best","worst","avg","sd"))
fp_qbproj <- subset(fp_qbproj, select = c("name","fp_team","fp_passatt","fp_passcomp","fp_passyd","fp_passtd",
                                          "fp_int","fp_rushatt","fp_rushyd","fp_rushtd","fp_fl","fp_fpts"))
data_ranks <- data_ranks[order(data_ranks$name),]
fp_qbproj <- fp_qbproj[order(fp_qbproj$name),]

#NUMBERFIRE #####

#NUMBERFIRE
##Create data from urls
numfire_html <- htmlParse(url_numfire)
numfire_html <- readHTMLTable(numfire_html)
numfire_qbproj <- do.call(rbind.data.frame, numfire_html)
rm(numfire_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
numfire_qbproj <- subset(numfire_qbproj, select = c(1,5:12,14,22))

#name the columns
names(numfire_qbproj) <- c("player","numfire_rank","numfire_ca","numfire_passyd","numfire_passtd","numfire_int",
                           "numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_fpts","numfire_dkcost")

#Split Completions and attemps
numfire_qbproj$numfire_passatt <- str_sub(numfire_qbproj$numfire_ca, end=str_locate(string=numfire_qbproj$numfire_ca, '/')[,1]-1)
numfire_qbproj$numfire_passcomp <- str_sub(numfire_qbproj$numfire_ca, start=str_locate(string=numfire_qbproj$numfire_ca, '/')[,1]+1)

#convert to Character so we dont remove the Decimal later
numfire_qbproj$numfire_dkcost <- as.character(numfire_qbproj$numfire_dkcost)
numfire_qbproj$player <- as.character(numfire_qbproj$player)
numfire_qbproj$numfire_passatt <- as.character(numfire_qbproj$numfire_passatt)
numfire_qbproj$numfire_passcomp <- as.character(numfire_qbproj$numfire_passcomp)
numfire_qbproj$numfire_passyd <- as.character(numfire_qbproj$numfire_passyd)
numfire_qbproj$numfire_passtd <- as.character(numfire_qbproj$numfire_passtd)
numfire_qbproj$numfire_int <- as.character(numfire_qbproj$numfire_int)
numfire_qbproj$numfire_rushatt <- as.character(numfire_qbproj$numfire_rushatt)
numfire_qbproj$numfire_rushyd <- as.character(numfire_qbproj$numfire_rushyd)
numfire_qbproj$numfire_rushtd <- as.character(numfire_qbproj$numfire_rushtd)
numfire_qbproj$numfire_fpts <- as.character(numfire_qbproj$numfire_fpts)

#convert to numeric
numfire_qbproj$numfire_passatt <- as.numeric(numfire_qbproj$numfire_passatt)
numfire_qbproj$numfire_passcomp <- as.numeric(numfire_qbproj$numfire_passcomp)
numfire_qbproj$numfire_passyd <- as.numeric(numfire_qbproj$numfire_passyd)
numfire_qbproj$numfire_passtd <- as.numeric(numfire_qbproj$numfire_passtd)
numfire_qbproj$numfire_int <- as.numeric(numfire_qbproj$numfire_int)
numfire_qbproj$numfire_rushatt <- as.numeric(numfire_qbproj$numfire_rushatt)
numfire_qbproj$numfire_rushyd <- as.numeric(numfire_qbproj$numfire_rushyd)
numfire_qbproj$numfire_rushtd <- as.numeric(numfire_qbproj$numfire_rushtd)
numfire_qbproj$numfire_fpts <- as.numeric(numfire_qbproj$numfire_fpts)

#Seprate names and team
numfire_qbproj$numfire_team <- str_sub(numfire_qbproj$player, start=str_locate(string=numfire_qbproj$player,',')[,1]+2)
numfire_qbproj$numfire_team <- gsub(pattern = ")", replacement = "", x = numfire_qbproj$numfire_team)
numfire_qbproj$name <- str_sub(numfire_qbproj$player, end=str_locate(string=numfire_qbproj$player,',')[,1]-5)
#Fix names to match rest of sources

#Calculate points based on League Settings
numfire_qbproj$numfire_fpts <- (numfire_qbproj$numfire_passyd*passYdsMultiplier) + (numfire_qbproj$numfire_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_passyd >= PassBonus,numfire_qbproj$numfire_fpts + Pbonus, numfire_qbproj$numfire_fpts)
}
if(DailyFantasy == F){
  numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_passyd >= PassBonus,numfire_qbproj$numfire_fpts + Pbonus, numfire_qbproj$numfire_fpts)
}

if(DailyFantasy == T){
  numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_rushyd >= RushBonus,numfire_qbproj$numfire_fpts + Rbonus, numfire_qbproj$numfire_fpts)
}
if(DailyFantasy == F){
  numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_rushyd >= RushBonus,numfire_qbproj$numfire_fpts + Rbonus, numfire_qbproj$numfire_fpts)
}

#Add in TD probability
numfire_qbproj$numfire_passtd <- round(numfire_qbproj$numfire_passtd, 0)
numfire_qbproj$numfire_rushtd <- round(numfire_qbproj$numfire_rushtd, 0)
numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_passtd >= 1,numfire_qbproj$numfire_fpts + (numfire_qbproj$numfire_passtd*passTdsMultiplier), numfire_qbproj$numfire_fpts)
numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_rushtd >= 1,numfire_qbproj$numfire_fpts + (numfire_qbproj$numfire_rushtd*rushTdsMultiplier), numfire_qbproj$numfire_fpts)

#Interception Probability
numfire_qbproj$numfire_int <- round(numfire_qbproj$numfire_int)
numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_int >= 1,numfire_qbproj$numfire_fpts + (numfire_qbproj$numfire_int*passIntMultiplier), numfire_qbproj$numfire_fpts)

#Remove all insginicant projections
numfire_qbproj <- subset(numfire_qbproj, numfire_qbproj$numfire_passatt > .99)

#resort data and reorder by name

numfire_qbproj <- subset(numfire_qbproj, select = c("name","numfire_team","numfire_rank","numfire_passatt","numfire_passcomp","numfire_passyd","numfire_passtd","numfire_int",
                                                    "numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_fpts","numfire_dkcost"))
numfire_qbproj <- numfire_qbproj[order(numfire_qbproj$name),]

#MERGE DATA & CALCULATIONS #####

##MERGE DATA##

projections <- merge(cbs_qbproj, espn_qbproj, by="name", all.x = T)
projections <- merge(projections, fftoday_qbproj, by="name", all.x = T)
projections <- merge(projections, ffs_qbproj, by="name", all.x = T)
projections <- merge(projections, fp_qbproj, by="name", all.x = T)
projections <- merge(projections, numfire_qbproj, by="name", all.x = T)

projections <- projections[order(projections$name),]

##Create csv for Value Gap Analysis##
#need pass yards, tds, pass comp

qbVGApassyd <- subset(projections, select = c("name","cbs_passyd","espn_passyd","ffs_passyd","fp_passyd","fftoday_passyd","numfire_passyd"))
columns_qbVGApassyd <- ncol(qbVGApassyd)
qbVGApassyd$passYds <- apply(qbVGApassyd[2:columns_qbVGApassyd], 1, mean, na.rm=TRUE)
qbVGApassyd$passYds <- round(qbVGApassyd$passYds, digits = 1)
qbVGApassyd <- subset(qbVGApassyd, select = c("name","passYds"))

qbVGApasstd <- subset(projections, select = c("name","cbs_passtd","espn_passtd","ffs_passtd","fp_passtd","fftoday_passtd","numfire_passtd"))
qbVGApasstd$passTds <- apply(qbVGApasstd[2:columns_qbVGApassyd], 1, mean, na.rm=TRUE)
qbVGApasstd$passTds <- round(qbVGApasstd$passTds, digits = 1)
qbVGApasstd <- subset(qbVGApasstd, select = c("passTds"))

qbVGApasscomp <- subset(projections, select = c("name","cbs_passcomp","espn_passcomp","ffs_passcomp","fp_passcomp","fftoday_passcomp","numfire_passcomp"))
qbVGApasscomp$passComp <- apply(qbVGApasscomp[2:columns_qbVGApassyd], 1, mean, na.rm=TRUE)
qbVGApasscomp$passComp <- round(qbVGApasscomp$passComp, digits = 1)
qbVGApasscomp <- subset(qbVGApasscomp, select = c("passComp"))

qbVGA <- cbind(qbVGApassyd,qbVGApasstd,qbVGApasscomp)

write.csv(qbVGA, file=paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Quarter Back.csv", sep=""), row.names=FALSE)

projections <- projections[order(projections$name),]

##Calculations##

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","numfire_dkcost","cbs_fpts","espn_fpts","ffs_fpts","fftoday_fpts","fp_fpts","numfire_fpts"))
fantasyproj$cbs_fpts <- as.numeric(fantasyproj$cbs_fpts)
fantasyproj$espn_fpts <- as.numeric(fantasyproj$espn_fpts)
fantasyproj$ffs_fpts <- as.numeric(fantasyproj$ffs_fpts)
fantasyproj$fftoday_fpts <- as.numeric(fantasyproj$fftoday_fpts)
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
fantasyproj2 <- subset(fantasyproj, fantasyproj$rank <= 32)
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
    labs(title = paste("Week ", week , " Quarter Back Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$Floor)-5),(max(fantasyproj$mean)+10)))
}
if(DailyFantasy == F && ppr == T){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/PPR/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
  #Save Data
  save(fantasyproj, file = paste(getwd(),"/Data/Weekly/PPR/Week",week," Quarter Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/PPR/Week",week," Quarter Back-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Quarter Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week",week," Quarter Back-fantasyproj.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," qbprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," qbprojections.html",sep=""))
}
if(DailyFantasy == F && ppr == F){
  #Save Graphs
  ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Weekly/STD/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
  #Save Data
  save(projections, file = paste(getwd(),"/Data/Weekly/STD/Week",week,"qb-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Weekly/STD/Week",week,"qb-projections.csv", sep=""), row.names=FALSE)
  save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"qb-projections.RData", sep=""))
  write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week",week,"qb-projections.csv", sep=""), row.names=FALSE)
  #Save HTML File
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," qbprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," qbprojections.html",sep=""))
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
    labs(title = paste("Week ", week , " Quarter Back Projections", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$Floor)-5),(max(fantasyproj$mean)+10)))
  }
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
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
    labs(title = paste("Week ", week , " Quarter Back Projections vs Cost", sep="")) +
    coord_cartesian(xlim =c((min(fantasyproj$Floor)-5),(max(fantasyproj$mean)+10)))
  }
if(DailyFantasy == T){
  ggsave(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back Cost.jpg", sep=""), width=10, height=10)
  ggsave(paste(getwd(),"/Figures/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back Cost.jpg", sep=""), width=10, height=10)
  #Save file
  save(fantasyproj, file = paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back-fantasyproj.csv", sep=""), row.names=FALSE)
  save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back-fantasyproj.RData", sep=""))
  write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," Quarter Back-fantasyproj.csv", sep=""), row.names=FALSE)
  #save the html file
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," qbprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," qbprojections.html",sep=""))
}

