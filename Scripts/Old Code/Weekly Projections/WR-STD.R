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
url_cbs <- paste("http://www.cbssports.com/fantasy/football/stats/weeklyprojections/WR/", week, "/avg?&print_rows=9999", sep="")
url_espn <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=4&leagueId=",espn_std,sep="")
url_espn1 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=4&leagueId=",espn_std,"&startIndex=40",sep="")
url_espn2 <- paste("http://games.espn.go.com/ffl/tools/projections?&scoringPeriodId=",week,"&seasonId=",season,"&slotCategoryId=4&leagueId=",espn_std,"&startIndex=80",sep="")
url_fftoday <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=20,&LeagueID=", fft_espnSTD, sep="")
url_fftoday1 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=", week, "&PosID=20,&LeagueID=", fft_espnSTD,"&order_by=FFPts&sort_order=DESC&cur_page=1", sep="")
url_fantasy_pro_ranks <- ("http://www.fantasypros.com/nfl/rankings/wr.php")
url_fantasy_pro <- ("http://www.fantasypros.com/nfl/projections/wr.php")
url_numfire <- paste("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/wr")

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
#Fix CBS' broken points
cbs_wrproj$cbs_fpts <- cbs_wrproj$cbs_fpts + (cbs_wrproj$cbs_yd/10) + (cbs_wrproj$cbs_td*6) + (cbs_wrproj$cbs_fl*-2)
#Remove all insginicant projections
cbs_wrproj1 <- subset(cbs_wrproj, cbs_wrproj$cbs_fpts > 1.99)
cbs_wrproj <- cbs_wrproj1
rm(cbs_wrproj1)
#resort data and reorder by name
cbs_wrproj <- subset(cbs_wrproj, select = c("name","cbs_team","cbs_recpts","cbs_yd","cbs_td","cbs_fl","cbs_fpts"))
cbs_wrproj <- cbs_wrproj[order(cbs_wrproj$name),]

#ESPN #####
#ESPN
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

#Remove all insginicant projections
espn_wrproj1 <- subset(espn_wrproj, espn_wrproj$espn_fpts > 1.99)
espn_wrproj <- espn_wrproj1
rm(espn_wrproj1)
#resort and reorder by name
espn_wrproj <- subset(espn_wrproj, select = c("name","espn_team","espn_recpts","espn_yd","espn_td","espn_fpts"))
espn_wrproj <- espn_wrproj[order(espn_wrproj$name),]

#FANTASY SHARKS ####

#FANTASY SHARKS
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


#convert name and team to Char
ffs_wrproj$player <- as.character(ffs_wrproj$player)
ffs_wrproj$ffs_team <- as.character(ffs_wrproj$ffs_team)

#Remove all insginicant projections
ffs_wrproj2 <- subset(ffs_wrproj, ffs_wrproj$ffs_fpts > 2)
ffs_wrproj <- ffs_wrproj2
rm(ffs_wrproj2)

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

#resort data and reorder by name
ffs_wrproj <- subset(ffs_wrproj, select = c("name","ffs_team","ffs_recpts","ffs_yd","ffs_td","ffs_fpts"))
ffs_wrproj <- ffs_wrproj[order(ffs_wrproj$name),]

#FFTODAY ####

#FFTODAY
#Create data from urls
fftoday_wrproj <- readHTMLTable(url_fftoday, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
fftoday_wrproj1 <- readHTMLTable(url_fftoday1, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns
fftoday_wrproj <- subset(fftoday_wrproj, select = c(2,3,5,6,7,8,9,10,11))
fftoday_wrproj1 <- subset(fftoday_wrproj1, select = c(2,3,5,6,7,8,9,10,11))
#clean headers
fftoday_wrproj <- fftoday_wrproj[2:(dim(fftoday_wrproj)[1]-1),]
fftoday_wrproj1 <- fftoday_wrproj1[2:(dim(fftoday_wrproj1)[1]-1),]
#name the columns
names(fftoday_wrproj) <- names(fftoday_wrproj1) <- c("player","fft_team","fft_rushatt","fft_rushyd","fft_rushtd",
                                                     "fft_recpts","fft_recyd","fft_rectd","fft_fpts")
#Merge and remove other dataframes to reduce clutter
fftoday_wrproj <- rbind(fftoday_wrproj, fftoday_wrproj1)
rm(fftoday_wrproj1)
#Convert to numeric
fftoday_wrproj$fft_rushatt <- as.numeric(fftoday_wrproj$fft_rushatt)
fftoday_wrproj$fft_rushyd <- as.numeric(fftoday_wrproj$fft_rushyd)
fftoday_wrproj$fft_rushtd <- as.numeric(fftoday_wrproj$fft_rushtd)
fftoday_wrproj$fft_recpts <- as.numeric(fftoday_wrproj$fft_recpts)
fftoday_wrproj$fft_recyd <- as.numeric(fftoday_wrproj$fft_recyd)
fftoday_wrproj$fft_rectd <- as.numeric(fftoday_wrproj$fft_rectd)
fftoday_wrproj$fft_fpts <- as.numeric(fftoday_wrproj$fft_fpts)
#remove symbol from fftoday name

fftoday_wrproj$name <- str_replace_all(fftoday_wrproj$player, "Â", "")
fftoday_wrproj$name <- str_replace_all(fftoday_wrproj$name, "^\\s+", "")

#fftoday_wrproj$name <- gsub(pattern = "Â", replacement = "", x = fftoday_wrproj$player)
#fftoday_wrproj$name <- str_sub(fftoday_wrproj$player, start= +3)

fftoday_wrproj[fftoday_wrproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."
fftoday_wrproj[fftoday_wrproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"

#Remove all insginicant projections
fftoday_wrproj1 <- subset(fftoday_wrproj, fftoday_wrproj$fft_fpts > 1.99)
fftoday_wrproj <- fftoday_wrproj1
rm(fftoday_wrproj1)
#resort and order by name
fftoday_wrproj <- subset(fftoday_wrproj, select = c("name","fft_team","fft_rushatt","fft_rushyd","fft_rushtd",
                                                    "fft_recpts","fft_recyd","fft_rectd","fft_fpts"))
fftoday_wrproj <- fftoday_wrproj[order(fftoday_wrproj$name),]

#FANTASYPRO ####

#FANTASY PROS Ranks & Projections
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


#Remove all insginicant projections
fp_wrproj1 <- subset(fp_wrproj, fp_wrproj$fp_fpts > 1.99)
fp_wrproj <- fp_wrproj1
rm(fp_wrproj1)
#resort and reorder by name
data_ranks <- subset(data_ranks,select = c("rank","name","best","worst","avg","sd"))
fp_wrproj <- subset(fp_wrproj, select = c("name","fp_team","fp_rushatt","fp_rushyd","fp_rushtd","fp_recpts","fp_recyd","fp_rectd",
                                          "fp_fl","fp_fpts"))
data_ranks <- data_ranks[order(data_ranks$name),]
fp_wrproj <- fp_wrproj[order(fp_wrproj$name),]

#NUMBERFIRE #####

#NUMBERFIRE
##Create data from urls
numfire_html <- htmlParse(url_numfire)
numfire_html <- readHTMLTable(numfire_html)
numfire_wrproj <- do.call(rbind.data.frame, numfire_html)
rm(numfire_html)

##CLEAN AND STANDARDIZE DATA##

#get rid of unwanted columns

numfire_wrproj <- subset(numfire_wrproj, select = c(1,5:11,13))

#name the columns
names(numfire_wrproj) <- c("player","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd","numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts")

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

#Remove all insginicant projections
numfire_wrproj <- subset(numfire_wrproj, numfire_wrproj$numfire_fpts  > 1.99)
#resort data and reorder by name

numfire_wrproj <- subset(numfire_wrproj, select = c("name","numfire_team","numfire_rank","numfire_rushatt","numfire_rushyd","numfire_rushtd",
                                                    "numfire_recpts","numfire_recyd","numfire_rectd","numfire_fpts"))
numfire_wrproj <- numfire_wrproj[order(numfire_wrproj$name),]

#MERGE DATA & CALCULATIONS #####

##MERGE DATA##
projections <- merge(cbs_wrproj, espn_wrproj, by="name", all.x = F)
projections <- merge(projections, ffs_wrproj, by="name", all.x = F)
projections <- merge(projections, fp_wrproj, by="name", all.x = F)
projections <- merge(projections, numfire_wrproj, by="name", all.x = F)
#projections <- merge(projections, fftoday_wrproj, by="name", all.x = F)

#projections <- cbind(cbs_wrproj,espn_wrproj,fftoday_wrproj,ffs_wrproj,fp_wrproj)
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
#fantasyproj$hodges.lehmann <- round(fantasyproj[,'hodges.lehmann'], 1)


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
#fantasyproj <- merge(fantasyproj, data_ranks, by="name", all.x = F)

#graphing
#geom_point(size=3)+
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
  labs(title = paste("Week ", week , " STD Wide Reciever Projections", sep="")) +
  coord_cartesian(xlim =c(0,(max(fantasyproj$mean)+10))) 

ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/WR/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)
ggsave(paste(getwd(),"/Figures/Weekly/STD/WR/Week ",week," Wide Reciever.jpg", sep=""), width=10, height=10)

#Save file
save(fantasyproj, file = paste(getwd(),"/Data/Weekly/STD/WR/Week ",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/STD/WR/Week ",week,"-fantasyproj.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Weekly/STD/WR/Week ",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Weekly/STD/WR/Week ",week,"-projections.csv", sep=""), row.names=FALSE)

save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/STD/WR/Week ",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/WR/Week ",week,"-fantasyproj.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/STD/WR/Week ",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/WR/Week ",week,"-projections.csv", sep=""), row.names=FALSE)

#put table into html
htmltable_fantasyproj <- xtable(fantasyproj)
htmltable_projections <- xtable(projections)

#save the html file
print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/STD/WR/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/STD/WR/Week ",week," wrprojections.html",sep=""))

print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/WR/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/STD/WR/Week ",week," wrprojections.html",sep=""))

