#Weekly Projections

#DATA SETTINGS ####

#URLS ####
url_cbs <- paste("http://www.cbssports.com/fantasy/football/stats/weeklyprojections/K/", week, "/avg/standard", sep="")
url_espn <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=17&leagueId=",espn_std,sep="")
url_fftoday <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=80,&LeagueID=", fft_espnSTD, sep="")
url_fantasy_pro_ranks <- ("http://www.fantasypros.com/nfl/rankings/k.php")
url_fantasy_pro <- ("http://www.fantasypros.com/nfl/projections/k.php")
url_numfire <- paste("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/k")

##STANDARIZATION TIME##

#CBS ####
##Create data from urls
cbs_html <- htmlParse(url_cbs)
cbs_html <- readHTMLTable(cbs_html)
cbs_kproj <- do.call(rbind.data.frame, cbs_html)
rm(cbs_html)

##CLEAN AND STANDARDIZE DATA##

#clean headers
cbs_kproj <- cbs_kproj[2:(dim(cbs_kproj)[1]-1),]
#name the columns
names(cbs_kproj) <- c("player","cbs_FG","cbs_FGA","cbs_XP","cbs_FPTS")

#convert to Character so we dont remove the Decimal later
cbs_kproj$cbs_FG <- as.character(cbs_kproj$cbs_FG)
cbs_kproj$cbs_FGA <- as.character(cbs_kproj$cbs_FGA)
cbs_kproj$cbs_XP <- as.character(cbs_kproj$cbs_XP)
cbs_kproj$cbs_FPTS <- as.character(cbs_kproj$cbs_FPTS)

#convert to numeric
cbs_kproj$cbs_FG <- as.character(cbs_kproj$cbs_FG)
cbs_kproj$cbs_FGA <- as.character(cbs_kproj$cbs_FGA)
cbs_kproj$cbs_XP <- as.character(cbs_kproj$cbs_XP)
cbs_kproj$cbs_FPTS <- as.character(cbs_kproj$cbs_FPTS)


#Seprate names and team
cbs_kproj$name <- str_sub(cbs_kproj$player, end=str_locate(string=cbs_kproj$player, ',')[,1]-1)
cbs_kproj$cbs_team <- str_trim(str_sub(cbs_kproj$player, start= -3))

#Fix names to match rest of sources
#cbs_kproj[cbs_kproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."

#Add points to league settings and update

#Calculate points based on League Settings
cbs_kproj$cbs_fpts <- (cbs_kproj$cbs_passyd*passYdsMultiplier) + (cbs_kproj$cbs_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  cbs_kproj$cbs_fpts <- ifelse(cbs_kproj$cbs_passyd >= PassBonus,cbs_kproj$cbs_fpts + Pbonus, cbs_kproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_kproj$cbs_fpts <- ifelse(cbs_kproj$cbs_passyd >= PassBonus,cbs_kproj$cbs_fpts + Pbonus, cbs_kproj$cbs_fpts)
}

if(DailyFantasy == T){
  cbs_kproj$cbs_fpts <- ifelse(cbs_kproj$cbs_rushyd >= RushBonus,cbs_kproj$cbs_fpts + Rbonus, cbs_kproj$cbs_fpts)
}
if(DailyFantasy == F){
  cbs_kproj$cbs_fpts <- ifelse(cbs_kproj$cbs_rushyd >= RushBonus,cbs_kproj$cbs_fpts + Rbonus, cbs_kproj$cbs_fpts)
}

#Add in TD probability
cbs_kproj$cbs_passtd <- round(cbs_kproj$cbs_passtd, 0)
cbs_kproj$cbs_rushtd <- round(cbs_kproj$cbs_rushtd, 0)
cbs_kproj$cbs_fpts <- ifelse(cbs_kproj$cbs_passtd >= 1,cbs_kproj$cbs_fpts + (cbs_kproj$cbs_passtd*passTdsMultiplier), cbs_kproj$cbs_fpts)
cbs_kproj$cbs_fpts <- ifelse(cbs_kproj$cbs_rushtd >= 1,cbs_kproj$cbs_fpts + (cbs_kproj$cbs_rushtd*rushTdsMultiplier), cbs_kproj$cbs_fpts)

#Interception Probability
cbs_kproj$cbs_int <- round(cbs_kproj$cbs_int)
cbs_kproj$cbs_fpts <- ifelse(cbs_kproj$cbs_int >= 1,cbs_kproj$cbs_fpts + (cbs_kproj$cbs_int*passIntMultiplier), cbs_kproj$cbs_fpts)

#Remove all insginicant projections
cbs_kproj1 <- subset(cbs_kproj, cbs_kproj$cbs_passatt > .99)
cbs_kproj <- cbs_kproj1
rm(cbs_kproj1)

#resort data and reorder by name
cbs_kproj <- subset(cbs_kproj, select = c("name","cbs_team","cbs_passatt","cbs_passcomp","cbs_passyd","cbs_passtd",
                                            "cbs_int","cbs_rushatt","cbs_rushyd","cbs_rushtd","cbs_fl","cbs_fpts"))
cbs_kproj <- cbs_kproj[order(cbs_kproj$name),]

#ESPN #####
#ESPN
##Create data from urls
espn_kproj <- readHTMLTable(url_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
espn_kproj <- subset(espn_kproj, select = c(1,4:10,14))

#clean headers
espn_kproj <- espn_kproj[2:(dim(espn_kproj)[1]),]

#name the columns
names(espn_kproj) <- c("player","espn_ca","espn_passyd","espn_passtd",
                        "espn_int","espn_rushatt","espn_rushyd","espn_rushtd","espn_fpts")

#Split Completions and attemps
espn_kproj$espn_passatt <- str_sub(espn_kproj$espn_ca, end=str_locate(string=espn_kproj$espn_ca, '/')[,1]-1)
espn_kproj$espn_passcomp <- str_sub(espn_kproj$espn_ca, start=str_locate(string=espn_kproj$espn_ca, '/')[,1]+1)

#convert to numeric
espn_kproj$espn_passatt <- as.numeric(espn_kproj$espn_passatt)
espn_kproj$espn_passcomp <- as.numeric(espn_kproj$espn_passcomp)
espn_kproj$espn_passyd <- as.numeric(espn_kproj$espn_passyd)
espn_kproj$espn_passtd <- as.numeric(espn_kproj$espn_passtd)
espn_kproj$espn_int <- as.numeric(espn_kproj$espn_int)
espn_kproj$espn_rushatt <- as.numeric(espn_kproj$espn_rushatt)
espn_kproj$espn_rushyd <- as.numeric(espn_kproj$espn_rushyd)
espn_kproj$espn_rushtd <- as.numeric(espn_kproj$espn_rushtd)
espn_kproj$espn_fpts <- as.numeric(espn_kproj$espn_fpts)

#Seperate names and team
espn_kproj$name <- str_sub(espn_kproj$player, end=str_locate(string=espn_kproj$player, ',')[,1]-1)

#espn_kproj$name <- str_replace_all(espn_kproj$name, "\\*", "")
espn_kproj$espn_team <- str_sub(espn_kproj$player, start=str_locate(string=espn_kproj$player, ',')[,1]+1)
espn_kproj$espn_team <- str_to_upper(str_sub(espn_kproj$espn_team, end=str_locate(string=espn_kproj$espn_team, ' ')[,1]+3))

#Fix names to match rest of sources
espn_kproj[espn_kproj$name=="Stevie Johnson", "name"] <- "Steve Johnson"
espn_kproj[espn_kproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
espn_kproj$espn_fpts <- (espn_kproj$espn_passyd*passYdsMultiplier) + (espn_kproj$espn_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  espn_kproj$espn_fpts <- ifelse(espn_kproj$espn_passyd >= PassBonus,espn_kproj$espn_fpts + Pbonus, espn_kproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_kproj$espn_fpts <- ifelse(espn_kproj$espn_passyd >= PassBonus,espn_kproj$espn_fpts + Pbonus, espn_kproj$espn_fpts)
}

if(DailyFantasy == T){
  espn_kproj$espn_fpts <- ifelse(espn_kproj$espn_rushyd >= RushBonus,espn_kproj$espn_fpts + Rbonus, espn_kproj$espn_fpts)
}
if(DailyFantasy == F){
  espn_kproj$espn_fpts <- ifelse(espn_kproj$espn_rushyd >= RushBonus,espn_kproj$espn_fpts + Rbonus, espn_kproj$espn_fpts)
}

#Add in TD probability
espn_kproj$espn_passtd <- round(espn_kproj$espn_passtd, 0)
espn_kproj$espn_rushtd <- round(espn_kproj$espn_rushtd, 0)
espn_kproj$espn_fpts <- ifelse(espn_kproj$espn_passtd >= 1,espn_kproj$espn_fpts + (espn_kproj$espn_passtd*passTdsMultiplier), espn_kproj$espn_fpts)
espn_kproj$espn_fpts <- ifelse(espn_kproj$espn_rushtd >= 1,espn_kproj$espn_fpts + (espn_kproj$espn_rushtd*rushTdsMultiplier), espn_kproj$espn_fpts)

#Interception Probability
espn_kproj$espn_int <- round(espn_kproj$espn_int)
espn_kproj$espn_fpts <- ifelse(espn_kproj$espn_int >= 1,espn_kproj$espn_fpts + (espn_kproj$espn_int*passIntMultiplier), espn_kproj$espn_fpts)

#Remove all insginicant projections
espn_kproj1 <- subset(espn_kproj, espn_kproj$espn_passatt > .99)
espn_kproj <- espn_kproj1
rm(espn_kproj1)

#resort and reorder by name
espn_kproj <- subset(espn_kproj, select = c("name","espn_team","espn_passatt","espn_passcomp","espn_passyd","espn_passtd",
                                              "espn_int","espn_rushatt","espn_rushyd","espn_rushtd","espn_fpts"))
espn_kproj <- espn_kproj[order(espn_kproj$name),]

#FANTASY SHARKS ####

#FANTASY SHARKS
##Create data from urls
ffs_kproj <- read.csv("http://www.fantasysharks.com/apps/Projections/WeeklyProjections.php?pos=qb&l=12&format=csv")

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
ffs_kproj <- subset(ffs_kproj, select = c(3,4,6:9,11:13))

#name the columns
names(ffs_kproj) <- c("player","ffs_team","ffs_passcomp","ffs_passyd","ffs_passtd","ffs_int","ffs_rushyd","ffs_rushtd","ffs_fpts")

#convert name and team to Char
ffs_kproj$player <- as.character(ffs_kproj$player)
ffs_kproj$ffs_team <- as.character(ffs_kproj$ffs_team)

#Seprate names and  fix 2 Char teams
ffs_kproj$last <- str_sub(ffs_kproj$player, end=str_locate(string=ffs_kproj$player, ' ')[,1]-2)
ffs_kproj$first <- str_sub(ffs_kproj$player, start=str_locate(string=ffs_kproj$player,' ')[,1]+1)
ffs_kproj$name <- paste(ffs_kproj$first, ffs_kproj$last)
ffs_kproj[ffs_kproj$ffs_team=="NOS", "ffs_team"] <- "NO"
ffs_kproj[ffs_kproj$ffs_team=="KCC", "ffs_team"] <- "KC"
ffs_kproj[ffs_kproj$ffs_team=="GBP", "ffs_team"] <- "GB"
ffs_kproj[ffs_kproj$ffs_team=="SDC", "ffs_team"] <- "SD"
ffs_kproj[ffs_kproj$ffs_team=="SFO", "ffs_team"] <- "SF"
ffs_kproj[ffs_kproj$ffs_team=="TBB", "ffs_team"] <- "TB"

#Fix names to match rest of sources

#Calculate points based on League Settings
ffs_kproj$ffs_fpts <- (ffs_kproj$ffs_passyd*passYdsMultiplier) + (ffs_kproj$ffs_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  ffs_kproj$ffs_fpts <- ifelse(ffs_kproj$ffs_passyd >= PassBonus,ffs_kproj$ffs_fpts + Pbonus, ffs_kproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_kproj$ffs_fpts <- ifelse(ffs_kproj$ffs_passyd >= PassBonus,ffs_kproj$ffs_fpts + Pbonus, ffs_kproj$ffs_fpts)
}

if(DailyFantasy == T){
  ffs_kproj$ffs_fpts <- ifelse(ffs_kproj$ffs_rushyd >= RushBonus,ffs_kproj$ffs_fpts + Rbonus, ffs_kproj$ffs_fpts)
}
if(DailyFantasy == F){
  ffs_kproj$ffs_fpts <- ifelse(ffs_kproj$ffs_rushyd >= RushBonus,ffs_kproj$ffs_fpts + Rbonus, ffs_kproj$ffs_fpts)
}

#Add in TD probability
ffs_kproj$ffs_passtd <- round(ffs_kproj$ffs_passtd, 0)
ffs_kproj$ffs_rushtd <- round(ffs_kproj$ffs_rushtd, 0)
ffs_kproj$ffs_fpts <- ifelse(ffs_kproj$ffs_passtd >= 1,ffs_kproj$ffs_fpts + (ffs_kproj$ffs_passtd*passTdsMultiplier), ffs_kproj$ffs_fpts)
ffs_kproj$ffs_fpts <- ifelse(ffs_kproj$ffs_rushtd >= 1,ffs_kproj$ffs_fpts + (ffs_kproj$ffs_rushtd*rushTdsMultiplier), ffs_kproj$ffs_fpts)
                                                            
#Interception Probability
ffs_kproj$ffs_int <- round(ffs_kproj$ffs_int)
ffs_kproj$ffs_fpts <- ifelse(ffs_kproj$ffs_int >= 1,ffs_kproj$ffs_fpts + (ffs_kproj$ffs_int*passIntMultiplier), ffs_kproj$ffs_fpts)
                                                            
#resort data and reorder by name
ffs_kproj <- subset(ffs_kproj, select = c("name","ffs_team","ffs_passcomp","ffs_passyd","ffs_passtd","ffs_int","ffs_rushyd","ffs_rushtd","ffs_fpts"))
ffs_kproj <- ffs_kproj[order(ffs_kproj$name),]

#FFTODAY ####

#FFTODAY
#Create data from urls
fftoday_kproj <- readHTMLTable(url_fftoday, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns##
fftoday_kproj <- subset(fftoday_kproj, select = c(2:3,5:13))
#clean headers
fftoday_kproj <- fftoday_kproj[2:(dim(fftoday_kproj)[1]-1),]
#name the columns
names(fftoday_kproj) <- c("player","fftoday_team","fftoday_passcomp","fftoday_passatt","fftoday_passyd","fftoday_passtd",
                           "fftoday_int","fftoday_rushatt","fftoday_rushyd","fftoday_rushtd","fftoday_fpts")

#Convert to numeric
fftoday_kproj$fftoday_passcomp <- as.numeric(fftoday_kproj$fftoday_passcomp)
fftoday_kproj$fftoday_passatt <- as.numeric(fftoday_kproj$fftoday_passatt)
fftoday_kproj$fftoday_passyd <- as.numeric(fftoday_kproj$fftoday_passyd)
fftoday_kproj$fftoday_passtd <- as.numeric(fftoday_kproj$fftoday_passtd)
fftoday_kproj$fftoday_int <- as.numeric(fftoday_kproj$fftoday_int)
fftoday_kproj$fftoday_rushatt <- as.numeric(fftoday_kproj$fftoday_rushatt)
fftoday_kproj$fftoday_rushyd <- as.numeric(fftoday_kproj$fftoday_rushyd)
fftoday_kproj$fftoday_rushtd <- as.numeric(fftoday_kproj$fftoday_rushtd)
fftoday_kproj$fftoday_fpts <- as.numeric(fftoday_kproj$fftoday_fpts)

#remove symbol from fftoday name
fftoday_kproj$name <- str_sub(fftoday_kproj$player, start = +3)

#fix names
fftoday_kproj[fftoday_kproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."
fftoday_kproj[fftoday_kproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Calculate points based on League Settings
fftoday_kproj$fftoday_fpts <- (fftoday_kproj$fftoday_passyd*passYdsMultiplier) + (fftoday_kproj$fftoday_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  fftoday_kproj$fftoday_fpts <- ifelse(fftoday_kproj$fftoday_passyd >= PassBonus,fftoday_kproj$fftoday_fpts + Pbonus, fftoday_kproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_kproj$fftoday_fpts <- ifelse(fftoday_kproj$fftoday_passyd >= PassBonus,fftoday_kproj$fftoday_fpts + Pbonus, fftoday_kproj$fftoday_fpts)
}

if(DailyFantasy == T){
  fftoday_kproj$fftoday_fpts <- ifelse(fftoday_kproj$fftoday_rushyd >= RushBonus,fftoday_kproj$fftoday_fpts + Rbonus, fftoday_kproj$fftoday_fpts)
}
if(DailyFantasy == F){
  fftoday_kproj$fftoday_fpts <- ifelse(fftoday_kproj$fftoday_rushyd >= RushBonus,fftoday_kproj$fftoday_fpts + Rbonus, fftoday_kproj$fftoday_fpts)
}

#Add in TD probability
fftoday_kproj$fftoday_passtd <- round(fftoday_kproj$fftoday_passtd, 0)
fftoday_kproj$fftoday_rushtd <- round(fftoday_kproj$fftoday_rushtd, 0)
fftoday_kproj$fftoday_fpts <- ifelse(fftoday_kproj$fftoday_passtd >= 1,fftoday_kproj$fftoday_fpts + (fftoday_kproj$fftoday_passtd*passTdsMultiplier), fftoday_kproj$fftoday_fpts)
fftoday_kproj$fftoday_fpts <- ifelse(fftoday_kproj$fftoday_rushtd >= 1,fftoday_kproj$fftoday_fpts + (fftoday_kproj$fftoday_rushtd*rushTdsMultiplier), fftoday_kproj$fftoday_fpts)

#Interception Probability
fftoday_kproj$fftoday_int <- round(fftoday_kproj$fftoday_int)
fftoday_kproj$fftoday_fpts <- ifelse(fftoday_kproj$fftoday_int >= 1,fftoday_kproj$fftoday_fpts + (fftoday_kproj$fftoday_int*passIntMultiplier), fftoday_kproj$fftoday_fpts)

#Remove all insginicant projections
fftoday_kproj <- subset(fftoday_kproj, fftoday_kproj$fftoday_passatt > .99)

#resort and order by name
fftoday_kproj <- subset(fftoday_kproj, select = c("name","fftoday_team","fftoday_team","fftoday_passcomp","fftoday_passatt","fftoday_passyd","fftoday_passtd",
                                                    "fftoday_int","fftoday_rushatt","fftoday_rushyd","fftoday_rushtd","fftoday_fpts"))
fftoday_kproj <- fftoday_kproj[order(fftoday_kproj$name),]

#FANTASYPRO ####

#FANTASY PROS Ranks & Projections
#Create data from urls
data_ranks <- readHTMLTable(url_fantasy_pro_ranks, as.data.frame=TRUE, stringsAsFactors=FALSE)$data
fp_kproj <- readHTMLTable(url_fantasy_pro, as.data.frame=TRUE, stringsAsFactors=FALSE)$data

##CLEAN AND STANDARDIZE DATA##
#get rid of unwanted columns
data_ranks <- subset(data_ranks, select = c(1:2,4:7))

#clean headers
names(data_ranks) <- c("rank","player","best","worst","avg","sd")
names(fp_kproj) <- c("player","fp_passatt","fp_passcomp","fp_passyd","fp_passtd",
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

fp_kproj$fp_passatt <- as.numeric(fp_kproj$fp_passatt)
fp_kproj$fp_passcomp <- as.numeric(fp_kproj$fp_passcomp)
fp_kproj$fp_passyd <- as.numeric(fp_kproj$fp_passyd)
fp_kproj$fp_passtd <- as.numeric(fp_kproj$fp_passtd)
fp_kproj$fp_int <- as.numeric(fp_kproj$fp_int)
fp_kproj$fp_rushatt <- as.numeric(fp_kproj$fp_rushatt)
fp_kproj$fp_rushyd <- as.numeric(fp_kproj$fp_rushyd)
fp_kproj$fp_rushtd <- as.numeric(fp_kproj$fp_rushtd)
fp_kproj$fp_fl <- as.numeric(fp_kproj$fp_fl)
fp_kproj$fp_fpts <- as.numeric(fp_kproj$fp_fpts)

##Seperate Name and Team
data_ranks$name1 <- str_replace_all(data_ranks$player, " ", ",")
data_ranks$first <- str_sub(data_ranks$name1, end=str_locate(string=data_ranks$name1, ',')[,1]-1)
data_ranks$last <- str_sub(data_ranks$name1,start=str_locate(string=data_ranks$name1, ',')[,1]+1)
data_ranks$last <- str_sub(data_ranks$last,end=str_locate(string=data_ranks$last, ',')[,1]-1)
data_ranks$name <- paste(data_ranks$first,data_ranks$last, sep = " ")

fp_kproj$name1 <- str_replace_all(fp_kproj$player, " ", ",")
fp_kproj$fp_team <- str_sub(fp_kproj$player, start = -3)
fp_kproj$first <- str_sub(fp_kproj$name1, end=str_locate(string=fp_kproj$name1, ',')[,1]-1)
fp_kproj$last <- str_sub(fp_kproj$name1,start=str_locate(string=fp_kproj$name1, ',')[,1]+1)
fp_kproj$last <- str_sub(fp_kproj$last,end=str_locate(string=fp_kproj$last, ',')[,1]-1)
fp_kproj$name <- paste(fp_kproj$first,fp_kproj$last, sep = " ")

#Fix Teams or names

#Calculate points based on League Settings
fp_kproj$fp_fpts <- (fp_kproj$fp_passyd*passYdsMultiplier) + (fp_kproj$fp_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  fp_kproj$fp_fpts <- ifelse(fp_kproj$fp_passyd >= PassBonus,fp_kproj$fp_fpts + Pbonus, fp_kproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_kproj$fp_fpts <- ifelse(fp_kproj$fp_passyd >= PassBonus,fp_kproj$fp_fpts + Pbonus, fp_kproj$fp_fpts)
}

if(DailyFantasy == T){
  fp_kproj$fp_fpts <- ifelse(fp_kproj$fp_rushyd >= RushBonus,fp_kproj$fp_fpts + Rbonus, fp_kproj$fp_fpts)
}
if(DailyFantasy == F){
  fp_kproj$fp_fpts <- ifelse(fp_kproj$fp_rushyd >= RushBonus,fp_kproj$fp_fpts + Rbonus, fp_kproj$fp_fpts)
}

#Add in TD probability
fp_kproj$fp_passtd <- round(fp_kproj$fp_passtd, 0)
fp_kproj$fp_rushtd <- round(fp_kproj$fp_rushtd, 0)
fp_kproj$fp_fpts <- ifelse(fp_kproj$fp_passtd >= 1,fp_kproj$fp_fpts + (fp_kproj$fp_passtd*passTdsMultiplier), fp_kproj$fp_fpts)
fp_kproj$fp_fpts <- ifelse(fp_kproj$fp_rushtd >= 1,fp_kproj$fp_fpts + (fp_kproj$fp_rushtd*rushTdsMultiplier), fp_kproj$fp_fpts)

#Interception Probability
fp_kproj$fp_int <- round(fp_kproj$fp_int)
fp_kproj$fp_fpts <- ifelse(fp_kproj$fp_int >= 1,fp_kproj$fp_fpts + (fp_kproj$fp_int*passIntMultiplier), fp_kproj$fp_fpts)

#Remove all insginicant projections
fp_kproj1 <- subset(fp_kproj, fp_kproj$fp_passatt > .99)
fp_kproj <- fp_kproj1
rm(fp_kproj1)
#resort and reorder by name
data_ranks <- subset(data_ranks,select = c("rank","name","best","worst","avg","sd"))
fp_kproj <- subset(fp_kproj, select = c("name","fp_team","fp_passatt","fp_passcomp","fp_passyd","fp_passtd",
                                          "fp_int","fp_rushatt","fp_rushyd","fp_rushtd","fp_fl","fp_fpts"))
data_ranks <- data_ranks[order(data_ranks$name),]
fp_kproj <- fp_kproj[order(fp_kproj$name),]

#NUMBERFIRE #####

#NUMBERFIRE
##Create data from urls
numfire_html <- htmlParse(url_numfire)
numfire_html <- readHTMLTable(numfire_html)
numfire_kproj <- do.call(rbind.data.frame, numfire_html)
rm(numfire_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
numfire_kproj <- subset(numfire_kproj, select = c(1,5:12,14,22))

#name the columns
names(numfire_kproj) <- c("player","numfire_rank","numfire_ca","numfire_passyd","numfire_passtd","numfire_int",
                           "numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_fpts","numfire_dkcost")

#Split Completions and attemps
numfire_kproj$numfire_passatt <- str_sub(numfire_kproj$numfire_ca, end=str_locate(string=numfire_kproj$numfire_ca, '/')[,1]-1)
numfire_kproj$numfire_passcomp <- str_sub(numfire_kproj$numfire_ca, start=str_locate(string=numfire_kproj$numfire_ca, '/')[,1]+1)

#convert to Character so we dont remove the Decimal later
numfire_kproj$numfire_dkcost <- as.character(numfire_kproj$numfire_dkcost)
numfire_kproj$player <- as.character(numfire_kproj$player)
numfire_kproj$numfire_passatt <- as.character(numfire_kproj$numfire_passatt)
numfire_kproj$numfire_passcomp <- as.character(numfire_kproj$numfire_passcomp)
numfire_kproj$numfire_passyd <- as.character(numfire_kproj$numfire_passyd)
numfire_kproj$numfire_passtd <- as.character(numfire_kproj$numfire_passtd)
numfire_kproj$numfire_int <- as.character(numfire_kproj$numfire_int)
numfire_kproj$numfire_rushatt <- as.character(numfire_kproj$numfire_rushatt)
numfire_kproj$numfire_rushyd <- as.character(numfire_kproj$numfire_rushyd)
numfire_kproj$numfire_rushtd <- as.character(numfire_kproj$numfire_rushtd)
numfire_kproj$numfire_fpts <- as.character(numfire_kproj$numfire_fpts)

#convert to numeric
numfire_kproj$numfire_passatt <- as.numeric(numfire_kproj$numfire_passatt)
numfire_kproj$numfire_passcomp <- as.numeric(numfire_kproj$numfire_passcomp)
numfire_kproj$numfire_passyd <- as.numeric(numfire_kproj$numfire_passyd)
numfire_kproj$numfire_passtd <- as.numeric(numfire_kproj$numfire_passtd)
numfire_kproj$numfire_int <- as.numeric(numfire_kproj$numfire_int)
numfire_kproj$numfire_rushatt <- as.numeric(numfire_kproj$numfire_rushatt)
numfire_kproj$numfire_rushyd <- as.numeric(numfire_kproj$numfire_rushyd)
numfire_kproj$numfire_rushtd <- as.numeric(numfire_kproj$numfire_rushtd)
numfire_kproj$numfire_fpts <- as.numeric(numfire_kproj$numfire_fpts)

#Seprate names and team
numfire_kproj$numfire_team <- str_sub(numfire_kproj$player, start=str_locate(string=numfire_kproj$player,',')[,1]+2)
numfire_kproj$numfire_team <- gsub(pattern = ")", replacement = "", x = numfire_kproj$numfire_team)
numfire_kproj$name <- str_sub(numfire_kproj$player, end=str_locate(string=numfire_kproj$player,',')[,1]-5)
#Fix names to match rest of sources

#Calculate points based on League Settings
numfire_kproj$numfire_fpts <- (numfire_kproj$numfire_passyd*passYdsMultiplier) + (numfire_kproj$numfire_rushyd*rushYdsMultiplier)

#Yardage bonus
if(DailyFantasy == T){
  numfire_kproj$numfire_fpts <- ifelse(numfire_kproj$numfire_passyd >= PassBonus,numfire_kproj$numfire_fpts + Pbonus, numfire_kproj$numfire_fpts)
}
if(DailyFantasy == F){
  numfire_kproj$numfire_fpts <- ifelse(numfire_kproj$numfire_passyd >= PassBonus,numfire_kproj$numfire_fpts + Pbonus, numfire_kproj$numfire_fpts)
}

if(DailyFantasy == T){
  numfire_kproj$numfire_fpts <- ifelse(numfire_kproj$numfire_rushyd >= RushBonus,numfire_kproj$numfire_fpts + Rbonus, numfire_kproj$numfire_fpts)
}
if(DailyFantasy == F){
  numfire_kproj$numfire_fpts <- ifelse(numfire_kproj$numfire_rushyd >= RushBonus,numfire_kproj$numfire_fpts + Rbonus, numfire_kproj$numfire_fpts)
}

#Add in TD probability
numfire_kproj$numfire_passtd <- round(numfire_kproj$numfire_passtd, 0)
numfire_kproj$numfire_rushtd <- round(numfire_kproj$numfire_rushtd, 0)
numfire_kproj$numfire_fpts <- ifelse(numfire_kproj$numfire_passtd >= 1,numfire_kproj$numfire_fpts + (numfire_kproj$numfire_passtd*passTdsMultiplier), numfire_kproj$numfire_fpts)
numfire_kproj$numfire_fpts <- ifelse(numfire_kproj$numfire_rushtd >= 1,numfire_kproj$numfire_fpts + (numfire_kproj$numfire_rushtd*rushTdsMultiplier), numfire_kproj$numfire_fpts)

#Interception Probability
numfire_kproj$numfire_int <- round(numfire_kproj$numfire_int)
numfire_kproj$numfire_fpts <- ifelse(numfire_kproj$numfire_int >= 1,numfire_kproj$numfire_fpts + (numfire_kproj$numfire_int*passIntMultiplier), numfire_kproj$numfire_fpts)

#Remove all insginicant projections
numfire_kproj <- subset(numfire_kproj, numfire_kproj$numfire_passatt > .99)

#resort data and reorder by name

numfire_kproj <- subset(numfire_kproj, select = c("name","numfire_team","numfire_rank","numfire_passatt","numfire_passcomp","numfire_passyd","numfire_passtd","numfire_int",
                                                    "numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_fpts","numfire_dkcost"))
numfire_kproj <- numfire_kproj[order(numfire_kproj$name),]

#MERGE DATA & CALCULATIONS #####

##MERGE DATA##

projections <- merge(cbs_kproj, espn_kproj, by="name", all.x = T)
projections <- merge(projections, fftoday_kproj, by="name", all.x = T)
projections <- merge(projections, ffs_kproj, by="name", all.x = T)
projections <- merge(projections, fp_kproj, by="name", all.x = T)
projections <- merge(projections, numfire_kproj, by="name", all.x = T)

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
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/Week ",week," kprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week," kprojections.html",sep=""))
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
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/STD/Week ",week," kprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week," kprojections.html",sep=""))
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
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Daily Fantasy/Week ",week,"/Week ",week," kprojections.html",sep=""))
  print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," qbtier.html",sep=""))
  print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week,"/Week ",week," kprojections.html",sep=""))
}

