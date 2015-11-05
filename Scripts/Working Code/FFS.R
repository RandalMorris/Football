#Number Fire working code

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
