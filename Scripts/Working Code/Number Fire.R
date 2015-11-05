#Number Fire working code


url_numfire <- paste("http://www.numberfire.com/nfl/fantasy/fantasy-football-ppr-projections/rb")

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
