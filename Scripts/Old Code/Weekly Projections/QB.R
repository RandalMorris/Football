#Weekly Projections
#Todo
#Switch from Mclust to k-means

#DATA SETTINGS ####
season <- "2015"

#League Ids
LeagueID <- 368495
fft_espnSTD <- 26955
espn_std <- 0

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
cbs_qbproj$cbs_fpts <- (cbs_qbproj$cbs_passyd*passYdsMultiplier) + (cbs_qbproj$cbs_rushyd*rushYdsMultiplier) + (cbs_qbproj$cbs_fl*fumlMultiplier) + (cbs_qbproj$cbs_int*interception)
#Pass yard bonus
cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_passyd >= 400,cbs_qbproj$cbs_fpts + pass400, cbs_qbproj$cbs_fpts)

#Add in TD probability
cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_passtd >= .5,cbs_qbproj$cbs_fpts + passTdsMultiplier, cbs_qbproj$cbs_fpts)
cbs_qbproj$cbs_fpts <- ifelse(cbs_qbproj$cbs_rushtd >= .5,cbs_qbproj$cbs_fpts + rushTdsMultiplier, cbs_qbproj$cbs_fpts)

#Remove all insginicant projections
cbs_qbproj1 <- subset(cbs_qbproj, cbs_qbproj$cbs_fpts > 1.99)
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
espn_qbproj$espn_fpts <- (espn_qbproj$espn_passyd*passYdsMultiplier) + (espn_qbproj$espn_rushyd*rushYdsMultiplier) + (espn_qbproj$espn_int*interception)
#Add in TD probability
espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_passtd >= .5,espn_qbproj$espn_fpts + passTdsMultiplier, espn_qbproj$espn_fpts)
espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_rushtd >= .5,espn_qbproj$espn_fpts + rushTdsMultiplier, espn_qbproj$espn_fpts)
#Pass yard bonus
espn_qbproj$espn_fpts <- ifelse(espn_qbproj$espn_passyd >= 400,espn_qbproj$espn_fpts + pass400, espn_qbproj$espn_fpts)

#Remove all insginicant projections
espn_qbproj1 <- subset(espn_qbproj, espn_qbproj$espn_fpts > 1.99)
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

#Remove all insginicant projections
ffs_qbproj2 <- subset(ffs_qbproj, ffs_qbproj$ffs_fpts > 2)
ffs_qbproj <- ffs_qbproj2
rm(ffs_qbproj2)

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
ffs_qbproj$ffs_fpts <- (ffs_qbproj$ffs_passyd*passYdsMultiplier) + (ffs_qbproj$ffs_rushyd*rushYdsMultiplier) + (ffs_qbproj$ffs_int*interception)
#Add in TD probability
ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_passtd >= .5,ffs_qbproj$ffs_fpts + passTdsMultiplier, ffs_qbproj$ffs_fpts)
ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_rushtd >= .5,ffs_qbproj$ffs_fpts + rushTdsMultiplier, ffs_qbproj$ffs_fpts)
#Pass yard bonus
ffs_qbproj$ffs_fpts <- ifelse(ffs_qbproj$ffs_passyd >= 400,ffs_qbproj$ffs_fpts + pass400, ffs_qbproj$ffs_fpts)



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
names(fftoday_qbproj) <- c("player","fft_team","fft_passcomp","fft_passatt","fft_passyd","fft_passtd",
                           "fft_int","fft_rushatt","fft_rushyd","fft_rushtd","fft_fpts")

#Convert to numeric

fftoday_qbproj$fft_passcomp <- as.numeric(fftoday_qbproj$fft_passcomp)
fftoday_qbproj$fft_passatt <- as.numeric(fftoday_qbproj$fft_passatt)
fftoday_qbproj$fft_passyd <- as.numeric(fftoday_qbproj$fft_passyd)
fftoday_qbproj$fft_passtd <- as.numeric(fftoday_qbproj$fft_passtd)
fftoday_qbproj$fft_int <- as.numeric(fftoday_qbproj$fft_int)
fftoday_qbproj$fft_rushatt <- as.numeric(fftoday_qbproj$fft_rushatt)
fftoday_qbproj$fft_rushyd <- as.numeric(fftoday_qbproj$fft_rushyd)
fftoday_qbproj$fft_rushtd <- as.numeric(fftoday_qbproj$fft_rushtd)
fftoday_qbproj$fft_fpts <- as.numeric(fftoday_qbproj$fft_fpts)

#remove symbol from fftoday name
fftoday_qbproj$name <- str_replace_all(fftoday_qbproj$player, "Â", "")
fftoday_qbproj$name <- str_replace_all(fftoday_qbproj$name, "^\\s+", "")

#Calculate points based on League Settings
fftoday_qbproj$fft_fpts <- (fftoday_qbproj$fft_passyd*passYdsMultiplier) + (fftoday_qbproj$fft_rushyd*rushYdsMultiplier) + (fftoday_qbproj$fft_int*interception)
#Add in TD probability
fftoday_qbproj$fft_fpts <- ifelse(fftoday_qbproj$fft_passtd >= .5,fftoday_qbproj$fft_fpts + passTdsMultiplier, fftoday_qbproj$fft_fpts)
fftoday_qbproj$fft_fpts <- ifelse(fftoday_qbproj$fft_rushtd >= .5,fftoday_qbproj$fft_fpts + rushTdsMultiplier, fftoday_qbproj$fft_fpts)
#Pass yard bonus
fftoday_qbproj$fft_fpts <- ifelse(fftoday_qbproj$fft_passyd >= 400,fftoday_qbproj$fft_fpts + pass400, fftoday_qbproj$fft_fpts)



#Remove all insginicant projections
fftoday_qbproj <- subset(fftoday_qbproj, fftoday_qbproj$fft_fpts > 1.99)
#resort and order by name
fftoday_qbproj <- subset(fftoday_qbproj, select = c("name","fft_team","fft_team","fft_passcomp","fft_passatt","fft_passyd","fft_passtd",
                                                    "fft_int","fft_rushatt","fft_rushyd","fft_rushtd","fft_fpts"))
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
fp_qbproj$fp_fpts <- (fp_qbproj$fp_passyd*passYdsMultiplier) + (fp_qbproj$fp_rushyd*rushYdsMultiplier) + (fp_qbproj$fp_fl*fumlMultiplier) + (fp_qbproj$fp_int*interception)
#Add in TD probability
fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_passtd >= .5,fp_qbproj$fp_fpts + passTdsMultiplier, fp_qbproj$fp_fpts)
fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_rushtd >= .5,fp_qbproj$fp_fpts + rushTdsMultiplier, fp_qbproj$fp_fpts)
#Pass yard bonus
fp_qbproj$fp_fpts <- ifelse(fp_qbproj$fp_passyd >= 400,fp_qbproj$fp_fpts + pass400, fp_qbproj$fp_fpts)



#Remove all insginicant projections
fp_qbproj1 <- subset(fp_qbproj, fp_qbproj$fp_fpts > 1.99)
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

numfire_qbproj <- subset(numfire_qbproj, select = c(1,5:12,14))

#name the columns
names(numfire_qbproj) <- c("player","numfire_rank","numfire_ca","numfire_passyd","numfire_passtd","numfire_int",
                           "numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_fpts")

#Split Completions and attemps
numfire_qbproj$numfire_passatt <- str_sub(numfire_qbproj$numfire_ca, end=str_locate(string=numfire_qbproj$numfire_ca, '/')[,1]-1)
numfire_qbproj$numfire_passcomp <- str_sub(numfire_qbproj$numfire_ca, start=str_locate(string=numfire_qbproj$numfire_ca, '/')[,1]+1)

#convert to Character so we dont remove the Decimal later
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
numfire_qbproj$numfire_fpts <- (numfire_qbproj$numfire_passyd*passYdsMultiplier) + (numfire_qbproj$numfire_rushyd*rushYdsMultiplier) + (numfire_qbproj$numfire_int*interception)
#Add in TD probability
numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_passtd >= .5,numfire_qbproj$numfire_fpts + passTdsMultiplier, numfire_qbproj$numfire_fpts)
numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_rushtd >= .5,numfire_qbproj$numfire_fpts + rushTdsMultiplier, numfire_qbproj$numfire_fpts)

#Pass yard bonus
numfire_qbproj$numfire_fpts <- ifelse(numfire_qbproj$numfire_passyd >= 400,numfire_qbproj$numfire_fpts + pass400, numfire_qbproj$numfire_fpts)


#Remove all insginicant projections
numfire_qbproj <- subset(numfire_qbproj, numfire_qbproj$numfire_fpts  > 1.99)
#resort data and reorder by name

numfire_qbproj <- subset(numfire_qbproj, select = c("name","numfire_team","numfire_rank","numfire_passatt","numfire_passcomp","numfire_passyd","numfire_passtd","numfire_int",
                                                    "numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_fpts"))
numfire_qbproj <- numfire_qbproj[order(numfire_qbproj$name),]

#MERGE DATA & CALCULATIONS #####

##MERGE DATA##
projections <- merge(cbs_qbproj, espn_qbproj, by="name", all.x = FALSE)
projections <- merge(projections, fftoday_qbproj, by="name", all.x = FALSE)
projections <- merge(projections, ffs_qbproj, by="name", all.x = FALSE)
projections <- merge(projections, fp_qbproj, by="name", all.x = FALSE)
projections <- merge(projections, numfire_qbproj, by="name", all.x = FALSE)

#projections <- cbind(cbs_qbproj,espn_qbproj,fftoday_qbproj,ffs_qbproj,fp_qbproj)
projections <- projections[order(projections$name),]
  
##Calculations##

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","cbs_fpts","espn_fpts","ffs_fpts","fp_fpts","fft_fpts","numfire_fpts"))
fantasyproj$cbs_fpts <- as.numeric(fantasyproj$cbs_fpts)
fantasyproj$espn_fpts <- as.numeric(fantasyproj$espn_fpts)
fantasyproj$fft_fpts <- as.numeric(fantasyproj$fft_fpts)
fantasyproj$ffs_fpts <- as.numeric(fantasyproj$ffs_fpts)
fantasyproj$fp_fpts <- as.numeric(fantasyproj$fp_fpts)
fantasyproj$numfire_fpts <- as.numeric(fantasyproj$numfire_fpts)
fantasyproj <- fantasyproj[order(fantasyproj$name),]

#Average

#Hodges-Lehmann estimator
fantasyproj$hodges.lehmann <- apply(fantasyproj[,c("cbs_fpts","espn_fpts","ffs_fpts","fp_fpts","fft_fpts","numfire_fpts")],1, function(x) wilcox.test(x, conf.int=T)$estimate)
fantasyproj$hodges.lehmann <- round(fantasyproj[,'hodges.lehmann'], 1)


#ncol count
columns_fantasyproj <- ncol(fantasyproj)
rows_fantasyproj <- nrow(fantasyproj)


#find standard deviation of projections
fantasyproj_stdev <- apply(fantasyproj[2:columns_fantasyproj], 1, sd, na.rm=TRUE)


#find mean of projections
fantasyproj_mean <- apply(fantasyproj[2:columns_fantasyproj], 1, mean, na.rm=TRUE)

#round mean/stdev
fantasyproj_mean <- round(fantasyproj_mean, digits = 1)
fantasyproj_stdev <- round(fantasyproj_stdev, digits = 1)

#add mean/stdev
fantasyproj$mean <- fantasyproj_mean
fantasyproj$risk <- fantasyproj_stdev

#remove players without projections
#fantasyproj <- fantasyproj[!(fantasyproj$SD==0),]

##SORT AND ORDER##

#sort players by stdev
#fantasyproj <- fantasyproj[order(fantasyproj$sd), , drop = FALSE]

#add in ranks to fantasyprojections
#fantasyprojs <- merge(fantasyproj, data_ranks, by="name")


#add ceiling / floor columns
fantasyproj$Ceiling <- fantasyproj$mean + fantasyproj$risk
fantasyproj$Floor <- fantasyproj$mean - fantasyproj$risk

#Make NAs & Remove Floor below 2
fantasyproj$Floor[is.na(fantasyproj$Floor)] <- 0
fantasyproj <- subset(fantasyproj, fantasyproj$Floor > 1.99)
#training data
training <- subset(fantasyproj, select = c("Floor"))

#clustering
#cluster <- Mclust(training$Floor, G=7)
cluster <- kmeans(fantasyproj[,c("Floor")],centers = 4)
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

#combine data ranks?
#fantasyproj <- merge(fantasyproj, data_ranks, by="name", all.x = TRUE)

#graphing
ggplot(fantasyproj, aes(x=mean, y=rank, color=factor(Tier))) +
  geom_errorbarh(aes(xmin=Floor,xmax=Ceiling),height=.3)+
  geom_point(size=5,color="white")+
  geom_text(aes(x=mean,label=round(mean,0)),size=3)+
  geom_text(aes(x=mean, label=name, hjust=-1, vjust=(.5), angle=(0), size=1))+
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
  coord_cartesian(xlim =c((min(fantasyproj$mean)-5),(max(fantasyproj$mean)+10)))
ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/QB/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/QB/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
ggsave(paste(getwd(),"/Figures/Weekly/PPR/QB/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)
ggsave(paste(getwd(),"/Figures/Weekly/STD/QB/Week ",week," Quarter Back.jpg", sep=""), width=10, height=10)

#Save file
save(fantasyproj, file = paste(getwd(),"/Data/Weekly/PPR/QB/Week",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/PPR/QB/Week",week,"-fantasyproj.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Weekly/STD/QB/Week",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Weekly/STD/QB/Week",week,"-projections.csv", sep=""), row.names=FALSE)
          
save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/QB/Week",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/QB/Week",week,"-fantasyproj.csv", sep=""), row.names=FALSE)
          
save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/STD/QB/Week",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/QB/Week",week,"-projections.csv", sep=""), row.names=FALSE)

#put table into html
htmltable_fantasyproj <- xtable(fantasyproj)
htmltable_projections <- xtable(projections)

#save the html file
print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/PPR/QB/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/QB/Week ",week," qbprojections.html",sep=""))
print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/STD/QB/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/STD/QB/Week ",week," qbprojections.html",sep=""))

print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/QB/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/QB/Week ",week," qbprojections.html",sep=""))
print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/QB/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/QB/Week ",week," qbprojections.html",sep=""))

