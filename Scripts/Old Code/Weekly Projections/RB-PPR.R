#Weekly Projections

#Load libraries ####

library("XML")
library("seqinr")
library("stringr")
library("xtable")
library("mclust")
library("ggplot2")
library("plyr")
library("data.table")

#DATA SETTINGS ####
season <- "2015"

#League Ids
LeagueID <- 368495
fft_espnSTD <- 26955
espn_std <- 0
ppr <- 1 #1 = PPR, 0 = Non-PPR, 2 = Half-PPR

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
cbs_rbproj$cbs_fpts <- (cbs_rbproj$cbs_rushyd*rushYdsMultiplier) + (cbs_rbproj$cbs_rushtd*rushTdsMultiplier) + (cbs_rbproj$cbs_recyd*recYdsMultiplier) + (cbs_rbproj$cbs_rectd*recTdsMultiplier) + (cbs_rbproj$cbs_fl*fumlMultiplier) + (cbs_rbproj$cbs_int*interception)
#Pass yard bonus
cbs_rbproj$cbs_fpts <- ifelse(cbs_rbproj$cbs_passyd >= 400,cbs_rbproj$cbs_fpts + pass400, cbs_rbproj$cbs_fpts)


#Remove all insginicant projections
cbs_rbproj1 <- subset(cbs_rbproj, cbs_rbproj$cbs_fpts > 1.99)
cbs_rbproj <- cbs_rbproj1
rm(cbs_rbproj1)
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
#Add rec points for PPR
espn_rbproj$espn_fpts <- espn_rbproj$espn_fpts + espn_rbproj$espn_recpts
#Remove all insginicant projections
espn_rbproj1 <- subset(espn_rbproj, espn_rbproj$espn_fpts > 1.99)
espn_rbproj <- espn_rbproj1
rm(espn_rbproj1)
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

#Remove all insginicant projections
ffs_rbproj2 <- subset(ffs_rbproj, ffs_rbproj$ffs_fpts > 2)
ffs_rbproj <- ffs_rbproj2
rm(ffs_rbproj2)

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

#resort data and reorder by name
ffs_rbproj <- subset(ffs_rbproj, select = c("name","ffs_team","ffs_rushatt","ffs_rushyd","ffs_rushtd","ffs_recpts","ffs_recyd","ffs_rectd","ffs_fpts"))
ffs_rbproj <- ffs_rbproj[order(ffs_rbproj$name),]

#FANTASYPRO ####

#FANTASY PROS Ranks & Projections
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

#add rec to points for PPR
fp_rbproj$fp_fpts <- fp_rbproj$fp_fpts + fp_rbproj$fp_recpts
#Remove all insginicant projections
fp_rbproj1 <- subset(fp_rbproj, fp_rbproj$fp_fpts > 1.99)
fp_rbproj <- fp_rbproj1
rm(fp_rbproj1)
#resort and reorder by name
data_ranks <- subset(data_ranks,select = c("rank","name","best","worst","avg","sd"))
fp_rbproj <- subset(fp_rbproj, select = c("name","fp_team","fp_rushatt","fp_rushyd","fp_rushtd","fp_recpts","fp_recyd","fp_rectd",
                                          "fp_fl","fp_fpts"))
data_ranks <- data_ranks[order(data_ranks$name),]
fp_rbproj <- fp_rbproj[order(fp_rbproj$name),]

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


#NUMBERFIRE #####

#NUMBERFIRE
##Create data from urls
numfire_html <- htmlParse(url_numfire)
numfire_html <- readHTMLTable(numfire_html)
numfire_rbproj <- do.call(rbind.data.frame, numfire_html)
rm(numfire_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns

numfire_rbproj <- subset(numfire_rbproj, select = c(1,5:11,13))

#name the columns
names(numfire_rbproj) <- c("player","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts")

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

#Remove all insginicant projections
numfire_rbproj <- subset(numfire_rbproj, numfire_rbproj$numfire_fpts  > 1.99)
#resort data and reorder by name

numfire_rbproj <- subset(numfire_rbproj, select = c("name","numfire_team","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd",
                                                    "numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts"))
numfire_rbproj <- numfire_rbproj[order(numfire_rbproj$name),]

#MERGE DATA & CALCULATIONS #####

##MERGE DATA##
projections <- merge(cbs_rbproj, espn_rbproj, by="name", all.x = FALSE)
#projections <- merge(projections, fftoday_rbproj, by="name", all.x = FALSE)
projections <- merge(projections, ffs_rbproj, by="name", all.x = FALSE)
projections <- merge(projections, fp_rbproj, by="name", all.x = FALSE)
projections <- merge(projections, numfire_rbproj, by="name", all.x = FALSE)

#projections <- cbind(cbs_rbproj,espn_rbproj,fftoday_rbproj,ffs_rbproj,fp_rbproj)
projections <- projections[order(projections$name),]
  
##Calculations##

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","cbs_fpts","espn_fpts","ffs_fpts","fp_fpts","numfire_fpts"))
fantasyproj$cbs_fpts <- as.numeric(fantasyproj$cbs_fpts)
fantasyproj$espn_fpts <- as.numeric(fantasyproj$espn_fpts)
#fantasyproj$fft_fpts <- as.numeric(fantasyproj$fft_fpts)
fantasyproj$ffs_fpts <- as.numeric(fantasyproj$ffs_fpts)
fantasyproj$fp_fpts <- as.numeric(fantasyproj$fp_fpts)
fantasyproj$numfire_fpts <- as.numeric(fantasyproj$numfire_fpts)

fantasyproj <- fantasyproj[order(fantasyproj$name),]

#Average

#Hodges-Lehmann estimator
fantasyproj$hodges.lehmann <- apply(fantasyproj[,c("cbs_fpts","espn_fpts","ffs_fpts","fp_fpts","numfire_fpts")],1, function(x) wilcox.test(x, conf.int=T)$estimate)
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
cluster <- Mclust(training$Floor, G=7)
fantasyproj$Tier <- cluster$classification

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
#geom_point(size=3)+
ggplot(fantasyproj, aes(x=hodges.lehmann, y=rank, color=factor(Tier))) +
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
  labs(title = paste("Week ", week , " PPR Running Back Projections", sep="")) +
  coord_cartesian(xlim =c(0,(max(fantasyproj$mean)+10))) 
ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/RB/Week ",week," Running Back.jpg", sep=""), width=10, height=10)
ggsave(paste(getwd(),"/Figures/Weekly/PPR/RB/Week ",week," Running Back.jpg", sep=""), width=10, height=10)

#Save file
save(fantasyproj, file = paste(getwd(),"/Data/Weekly/PPR/RB/Week",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/PPR/RB/Week",week,"-fantasyproj.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Weekly/PPR/RB/Week",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Weekly/PPR/RB/Week",week,"-projections.csv", sep=""), row.names=FALSE)
          
save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/RB/Week",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/RB/Week",week,"-fantasyproj.csv", sep=""), row.names=FALSE)
          
save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/RB/Week",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/RB/Week",week,"-projections.csv", sep=""), row.names=FALSE)

#put table into html
htmltable_fantasyproj <- xtable(fantasyproj)
htmltable_projections <- xtable(projections)

#save the html file
print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/PPR/RB/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/RB/Week ",week," rbprojections.html",sep=""))

print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/RB/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/RB/Week ",week," rbprojections.html",sep=""))
