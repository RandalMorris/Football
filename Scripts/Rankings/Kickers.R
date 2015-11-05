#Load libraries
library("XML")
library("stringr")

#Functions
source(paste(getwd(),"/Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/Scripts/Functions/League Settings.R", sep=""))

#Risk - "Experts"
kickers <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/k-cheatsheets.php", stringsAsFactors = FALSE)$data

byeweek <- str_sub(kickers[,c("Player (team, bye)")], start=str_locate(kickers[,c("Player (team, bye)")], ",")[,1]+1)
name <- str_sub(kickers[,c("Player (team, bye)")], end=str_locate(kickers[,c("Player (team, bye)")], ",")[,1]-1)
name = as.matrix(name)
name <- str_sub(name,0,-4)
team <- str_sub(kickers[,c("Player (team, bye)")], start=str_locate(kickers[,c("Player (team, bye)")], ",")[,1]-3)
team = as.matrix(team)
team <- str_sub(team,0, end=str_locate(team, ",")[,1]-1)

team

#name


#name1 <- str_sub(kickers[,c("Player (team, bye)")], end=str_locate(kickers[,c("Player (team, bye)")], '\\(')[,1]-2)
#name2 <- str_sub(kickers[,c("Player (team, bye)")], end=str_locate(kickers[,c("Player (team, bye)")], '\\(')[,1]-1)

#name1[is.na(name1)] <- name2[is.na(name1)]

kickers$Player<- name
#kickers$Name <- nameMerge(kickers$Player)
kickers$Team <- team
kickers$Bye = byeweek
kickers$Rank <- as.numeric(kickers[,"Avg"])
kickers$Risk <- as.numeric(kickers[,"Std Dev"])

#Subset columns
kickers <- kickers[,c("Player","Team","Bye","Rank","Risk")]

#Remove rows with all NAs
kickers <- kickers[!is.na(kickers[,1]),]


#Sort by rank
kickers <- kickers[order(kickers$Rank),]

#View Rankings
kickers

#Save file
save(kickers, file = paste(getwd(), "/Data/kickers.RData", sep=""))
write.csv(kickers, file=paste(getwd(), "/Data/kickers.csv", sep=""), row.names=FALSE)

save(kickers, file = paste(getwd(), "/Data/Historical Rankings/kickers-", season, ".RData", sep=""))
write.csv(kickers, file=paste(getwd(), "/Data/Historical Rankings/kickers-", season, ".csv", sep=""), row.names=FALSE)

