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
#Fix CBS' broken points
cbs_teproj$cbs_fpts <- cbs_teproj$cbs_fpts + (cbs_teproj$cbs_yd/10) + (cbs_teproj$cbs_td*6) + (cbs_teproj$cbs_fl*-2)

#Remove all insginicant projections
cbs_teproj1 <- subset(cbs_teproj, cbs_teproj$cbs_fpts > 1.99)
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
#Add rec points for PPR
espn_teproj$espn_fpts <- espn_teproj$espn_fpts + espn_teproj$espn_recpts
#Remove all insginicant projections
espn_teproj1 <- subset(espn_teproj, espn_teproj$espn_fpts > 1.99)
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

#Remove all insginicant projections
ffs_teproj2 <- subset(ffs_teproj, ffs_teproj$ffs_fpts > 2)
ffs_teproj <- ffs_teproj2
rm(ffs_teproj2)

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
names(fftoday_teproj) <- c("player","fft_team","fft_recpts","fft_yd","fft_td","fft_fpts")

#Convert to numeric
fftoday_teproj$fft_recpts <- as.numeric(fftoday_teproj$fft_recpts)
fftoday_teproj$fft_yd <- as.numeric(fftoday_teproj$fft_yd)
fftoday_teproj$fft_td <- as.numeric(fftoday_teproj$fft_td)
fftoday_teproj$fft_fpts <- as.numeric(fftoday_teproj$fft_fpts)
#remove symbol from fftoday name
fftoday_teproj$name <- str_replace_all(fftoday_teproj$player, "Â", "")
fftoday_teproj$name <- str_replace_all(fftoday_teproj$name, "^\\s+", "")
fftoday_teproj[fftoday_teproj$name=="Steve Smith", "name"] <- "Steve Smith Sr."
fftoday_teproj[fftoday_teproj$name=="Odell Beckham Jr.", "name"] <- "Odell Beckham"
#add rec to points for PPR 
fftoday_teproj$fft_fpts <- fftoday_teproj$fft_fpts  + fftoday_teproj$fft_recpts
#Remove all insginicant projections
fftoday_teproj <- subset(fftoday_teproj, fftoday_teproj$fft_fpts > 1.99)
#resort and order by name
fftoday_teproj <- subset(fftoday_teproj, select = c("name","fft_team","fft_recpts","fft_yd","fft_td","fft_fpts"))
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

#add rec to points for PPR
fp_teproj$fp_fpts <- fp_teproj$fp_fpts + fp_teproj$fp_recpts
#Remove all insginicant projections
fp_teproj1 <- subset(fp_teproj, fp_teproj$fp_fpts > 1.99)
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

numfire_teproj <- subset(numfire_teproj, select = c(1,9:11,13))

#name the columns
names(numfire_teproj) <- c("player","numfire_recpts","numfire_yd","numfire_td","numfire_fpts")

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

#Remove all insginicant projections
numfire_teproj <- subset(numfire_teproj, numfire_teproj$numfire_fpts  > 1.99)
#resort data and reorder by name

numfire_teproj <- subset(numfire_teproj, select = c("name","numfire_team","numfire_recpts","numfire_yd","numfire_td","numfire_fpts"))
numfire_teproj <- numfire_teproj[order(numfire_teproj$name),]

#MERGE DATA & CALCULATIONS #####

##MERGE DATA##
projections <- merge(cbs_teproj, espn_teproj, by="name", all.x = F)
projections <- merge(projections, fftoday_teproj, by="name", all.x = F)
projections <- merge(projections, ffs_teproj, by="name", all.x = F)
projections <- merge(projections, fp_teproj, by="name", all.x = F)
projections <- merge(projections, numfire_teproj, by="name", all.x = F)

#projections <- cbind(cbs_teproj,espn_teproj,fftoday_teproj,ffs_teproj,fp_teproj)
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
#fantasyproj$hodges.lehmann <- apply(fantasyproj[,c("cbs_fpts","espn_fpts","fft_fpts","ffs_fpts")],1, function(x) wilcox.test(x, conf.int=T)$estimate)
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
fantasyproj2 <- subset(fantasyproj, fantasyproj$rank <= 75)
fantasyproj <- fantasyproj2
rm(fantasyproj2)

#combine data ranks?
#fantasyproj <- merge(fantasyproj, data_ranks, by="name", all.x = TRUE)

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
  labs(title = paste("Week ", week , " PPR Tight End Projections", sep="")) +
  coord_cartesian(xlim =c(0,(max(fantasyproj$mean)+10))) 
ggsave(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/TE/Week ",week," Tight End.jpg", sep=""), width=10, height=10)
ggsave(paste(getwd(),"/Figures/Weekly/PPR/TE/Week ",week," Tight End.jpg", sep=""), width=10, height=10)

#Save file
save(fantasyproj, file = paste(getwd(),"/Data/Weekly/PPR/TE/Week",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Weekly/PPR/TE/Week",week,"-fantasyproj.csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Weekly/PPR/TE/Week",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Weekly/PPR/TE/Week",week,"-projections.csv", sep=""), row.names=FALSE)
          
save(fantasyproj, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/TE/Week",week,"-fantasyproj.RData", sep=""))
write.csv(fantasyproj, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/TE/Week",week,"-fantasyproj.csv", sep=""), row.names=FALSE)
          
save(projections, file = paste(getwd(),"/Data/Historical Projections/Weekly/PPR/TE/Week",week,"-projections.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/TE/Week",week,"-projections.csv", sep=""), row.names=FALSE)

#put table into html
htmltable_fantasyproj <- xtable(fantasyproj)
htmltable_projections <- xtable(projections)

#save the html file
print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Weekly/PPR/TE/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Weekly/PPR/TE/Week ",week," teprojections.html",sep=""))

print.xtable(htmltable_fantasyproj, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/TE/Week ",week," tier.html",sep=""))
print.xtable(htmltable_projections, type="html", file=paste(getwd(),"/Data/Historical Projections/Weekly/PPR/TE/Week ",week," teprojections.html",sep=""))

