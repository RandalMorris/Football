#Season
season <- 2015
weekNo <- 3   # Set weekNo = 0 for seasonal projections

#Roster
numQBstarters <- 2
numRBstarters <- 2
numWRstarters <- 2
numTEstarters <- 1
numTotalStarters <- 12
numTotalPlayers <- 20

#League settings
defaultCap <- 200           #what the typical cap is for your service (ESPN, Yahoo, etc.) -- used for placing "avg cost" in context
leagueCap <- 300           #your league's cap
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Variable names
prefix <- c("name","pos","sourceName","team")
sourceSpecific <- c("name","team")
scoreCategories <- c("passAtt","passYds","passTds","passInt",
                     "rushAtt","rushYds","rushTds",
                     "rec","recYds","recTds",
                     "returnTds","twoPts","fumbles",
                     "dstPtsAllowed","dstYdsAllowed","dstSack","dstSafety","dstInt",
                     "dstFumlRec","dstFumlForce","dstBlk","dstTd","fg","fgAtt","fg019",
                     "fg2029","fg3039","fg4049","fg50","xp",
                     "dstFumlRec","dstFumlForce","dstBlk","dstTd","fg","fga","xp")
WeeklySources <- c("cbs_fpts","espn_fpts","ffs_fpts","fftoday_fpts")
calculatedVars <- c("positionRank","overallRank","points")
varNames <- c(calculatedVars, scoreCategories)
finalVarNames <- c("name","pos","team","sourceName","player","playerID","season", "playerId", "analystId", varNames)



#Scoring
passAttMultiplier <- 0      #0 pts per passing attempt
passCompMultiplier <- 0     #0 pts per passing completion
passIncompMultiplier <- 0   #0 pts per passing incompletion
passYdsMultiplier <- (1/25) #1 pt per 25 passing yds
passTdsMultiplier <- 4      #4 pts per passing td
passIntMultiplier <- -2     #-3 pts per passing interception
rushAttMultiplier <- 0      #0 pts per rushing attempt
rushYdsMultiplier <- (1/10) #1 pt per 10 rushing yds
rushTdsMultiplier <- 6      #6 pts per rushing touchdown
recMultiplier <- 1          #0 pts per reception
recYdsMultiplier <- (1/10)   #1 pt per 10 receiving yds
recTdsMultiplier <- 6       #6 pts per receiving touchdown
returnTdsMultiplier <- 6    #6 pts per return touchdown
twoPtsMultiplier <- 2       #2 pts per 2-point conversion
fumlMultiplier <- -2        #-3 pts per fumble lost

scoringRules <- list(
    QB = data.frame(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                    multiplier = c(1/25, 4, -3, 1/10, 6, 2, -2 )),
    RB = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)), 
    WR = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)),
    TE = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(1/10, 6, 0, 1/10, 6, 6, 2, -2)),
    K = data.frame(dataCol = c("xp", "fg", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                   multiplier = c(1, 3, 3, 3, 3, 4, 5)),
    DST = data.frame(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                     multiplier = c(2, 2, 2, 1, 6, 2)),
    ptsBracket = data.frame(threshold = c(0, 6, 13, 17, 27, 34, 45, 99),
                             points = c(5, 4, 3, 1, 0, -1, -3, -5))
  )


#Projections
#("Accuscore","EDSfootball","Footballguys1", "Footballguys2", "Footballguys3", "Footballguys4", "FOX", "NFL", "numberFire", "WalterFootball", "Yahoo")
sourcesOfProjections <- c("CBS","ESPN","FFtoday", "FantasyPros","FantasyFootballNerd","FantasySharks","EDSfootball") 

#, "Dodds-Norton", "Dodds", "Tremblay", "Herman", "Henry", "Wood", "Bloom") 
sourcesOfProjectionsAbbreviation <- c("cbs", "espn", "ffn", "fp", "fs", "fftoday","eds") #c("accu", "cbs1", "cbs2", "eds", "espn", "ffn", "fp", "fs", "fftoday", "fbg1", "fbg2", "fbg3", "fbg4", "fox", "nfl", "nf", "wf", "yahoo")

#Weights applied to each source in calculation of weighted average of projections
weight_accu <- 1    #Accuscore
weight_eds <- 1     #EDS Football
weight_espn <- 1    #ESPN
weight_ffn <- 1     #Fantasy Football Nerd
weight_fbg1 <- 1    #Footballguys: David Dodds
weight_fbg2 <- 1    #Footballguys: Bob Henry
weight_fbg3 <- 1    #Footballguys: Maurile Tremblay
weight_fbg4 <- 1    #Footballguys: Jason Wood
weight_fox <- 1    #FOX
weight_fp <- 1      #FantasyPros
weight_fs <- 1      #FantasySharks
weight_fftoday <- 1 #FFtoday
weight_nfl <- 1     #NFL.com
weight_nf <- 1      #numberFire
weight_wf <- 1      #WalterFootball
weight_yahoo <- 1   #Yahoo 

#Number of players at each position drafted in Top 100 (adjust for your league)
#qbReplacements <- 15
#rbReplacements <- 37
#wrReplacements <- 36
#teReplacements <- 11

#Alternative way of calculating the number of players at each position drafted in Top 100 based on league settings
numTeams <- 6  #number of teams in league
numQB <- 1      #number of avg QBs in starting lineup
numRB <- 2.5    #number of avg RBs in starting lineup
numWR <- 2.5    #number of avg WRs in starting lineup
numTE <- 1      #number of avg TEs in starting lineup

qbReplacements <- print(ceiling(numQB*numTeams*1.7))
rbReplacements <- print(ceiling(numRB*numTeams*1.4))
wrReplacements <- print(ceiling(numWR*numTeams*1.4))
teReplacements <- print(ceiling(numTE*numTeams*1.3))