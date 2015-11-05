#Value Gap Analysis

## Read the data

qbProjections <- data.table(read.csv(paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Quarter Back.csv", sep=""), stringsAsFactors = FALSE))
rbProjections <- data.table(read.csv(paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Running Back.csv", sep=""), stringsAsFactors = FALSE))
wrProjections <- data.table(read.csv(paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Wide Reciever.csv", sep=""), stringsAsFactors = FALSE))
teProjections <- data.table(read.csv(paste(getwd(),"/Data/Value Gap Analysis/Week ",week," Tight End.csv", sep=""), stringsAsFactors = FALSE))

playerTeams <- data.table(read.csv(paste(getwd(),"/Data/Value Gap Analysis/playerteams.csv", sep=""), stringsAsFactors = FALSE))
players <- data.table(read.csv(paste(getwd(),"/Data/Value Gap Analysis/players.csv", sep=""), stringsAsFactors = FALSE))

# Add playerId to projections
qbProjections <- merge(qbProjections, players[, c("playerId","name"), with = FALSE], by = "name")
rbProjections <- merge(rbProjections, players[, c("playerId","name"), with = FALSE], by = "name")
wrProjections <- merge(wrProjections, players[, c("playerId","name"), with = FALSE], by = "name")
teProjections <- merge(teProjections, players[, c("playerId","name"), with = FALSE], by = "name")

# Merge team information
qbProjections <- merge(qbProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")
rbProjections <- merge(rbProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")
wrProjections <- merge(wrProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")
teProjections <- merge(teProjections, playerTeams[, c("playerId", "team"), with = FALSE], by = "playerId")

## Summarize data by team and analyst
qbProjections[, c("qbTeamPassYds", "qbTeamPassTds", "qbTeamPassComp") := list(sum(passYds, na.rm = TRUE), sum(passTds, na.rm = TRUE), sum(passComp, na.rm = TRUE)), by = "team"]
rbProjections[, c("rbTeamRecYds", "rbTeamRecTds", "rbTeamRec") := list(sum(recYds, na.rm = TRUE), sum(recTds, na.rm = TRUE), sum(rec, na.rm = TRUE)), by = "team"]
wrProjections[, c("wrTeamRecYds", "wrTeamRecTds", "wrTeamRec") := list(sum(recYds, na.rm = TRUE), sum(recTds, na.rm = TRUE), sum(rec, na.rm = TRUE)), by = "team"]
teProjections[, c("teTeamRecYds", "teTeamRecTds", "teTeamRec") := list(sum(recYds, na.rm = TRUE), sum(recTds, na.rm = TRUE), sum(rec, na.rm = TRUE)), by = "team"]

## Generate team data set
teamData <- merge(unique(qbProjections[, c("team", "qbTeamPassYds", "qbTeamPassTds", "qbTeamPassComp"), with = FALSE]),
                  unique(rbProjections[, c("team", "rbTeamRecYds", "rbTeamRecTds", "rbTeamRec"), with = FALSE]), by = "team")
teamData <- merge(teamData, unique(wrProjections[, c("team", "wrTeamRecYds", "wrTeamRecTds", "wrTeamRec"), with = FALSE]), by = "team")
teamData <- merge(teamData, unique(teProjections[, c("team", "teTeamRecYds", "teTeamRecTds", "teTeamRec"), with = FALSE]), by = "team")

## Add stats across receiver positions
teamData[, teamRecYds := rbTeamRecYds + wrTeamRecYds + teTeamRecYds]
teamData[, teamRecTds := rbTeamRecTds + wrTeamRecTds + teTeamRecTds]
teamData[, teamRec := rbTeamRec + wrTeamRec + teTeamRec]

## Calculate the receiving yard share for NO receievers
newOrlRec <- rbindlist(list(rbProjections[team == "NO", c("playerId","name", "recYds"), with = FALSE], 
                            wrProjections[team == "NO", c("playerId","name", "recYds"), with = FALSE],
                            teProjections[team == "NO", c("playerId","name", "recYds"), with = FALSE]))
newOrlRec[, projRecYds := mean(recYds, na.rm = TRUE), by = "name"]
newOrlRecPlayers <- merge(players, unique(newOrlRec[, c("name", "projRecYds"), with = FALSE]), by = "name")
newOrlRecPlayers[, ydShare:= projRecYds/sum(projRecYds)]


## Calculate the receiving td share for NO receievers
newOrlTds <- rbindlist(list(rbProjections[team == "NO", c("playerId","name", "recTds"), with = FALSE], 
                            wrProjections[team == "NO", c("playerId","name", "recTds"), with = FALSE],
                            teProjections[team == "NO", c("playerId","name", "recTds"), with = FALSE]))
newOrlTds[, projRecTds := mean(recTds, na.rm = TRUE), by = "name"]
newOrlTdsPlayers <- merge(players, unique(newOrlTds[, c("name", "projRecTds"), with = FALSE]), by = "name")
newOrlTdsPlayers[, tdShare:= projRecTds/sum(projRecTds)]


## Calculate differeces between passing and receiving stats
teamData[, passYdsDiff := qbTeamPassYds - teamRecYds]
teamData[, passTdDiff := qbTeamPassTds - teamRecTds]
teamData[, passRecDiff := qbTeamPassComp - teamRec]

## Generate bar plot for receiving yards difference
tblData <- unique(teamData[, c("team", "passYdsDiff"), with = FALSE])
tblData <- data.table(aggregate(teamData$passYdsDiff, by = list(teamData$team), FUN = mean, data = teamData))
setnames(tblData, 1:2,  c("team", "passYdsDiff"))
tblData <- tblData[team != "FA" ]
tblData <- tblData[,team := reorder(team, passYdsDiff, function(x)-x)]
ggplot(tblData, aes(x =team , y=passYdsDiff, fill = passYdsDiff > 0),  position = 'dodge' ) + 
  geom_bar(stat = "identity") + xlab("Team") + ylab("PassYds - RecYds") +scale_fill_discrete(guide = 'none') + ggtitle("Pass and Receiving Yard difference")
ggsave(paste(getwd(),"/Figures/Value Gap Analysis/Week ",week," passYdsDiffernce.jpg", sep=""), width=10, height=10)

## Generate bar plot for receiving td difference
tdData <-  data.table(aggregate(teamData$passTdDiff, by = list(teamData$team), FUN = mean, data = teamData))
setnames(tdData, 1:2,  c("team", "passTdDiff"))
tdData <- tdData[team != "FA" ]
tdData <- tdData[,team := reorder(team, passTdDiff, function(x)-x)]

ggplot(tdData, aes(x =team , y=passTdDiff, fill = passTdDiff > 0),  position = 'dodge' ) + 
  geom_bar(stat = "identity") + xlab("Team") + ylab("PassTds - RecTds") +scale_fill_discrete(guide = 'none') + ggtitle("Pass and Receiving Touchdown difference")
ggsave(paste(getwd(),"/Figures/Value Gap Analysis/Week ",week," passTdDiffernce.jpg", sep=""), width=10, height=10)

## Generate bar plot for reception difference
recData <-  data.table(aggregate(teamData$passRecDiff, by = list(teamData$team), FUN = mean, data = teamData))
setnames(recData, 1:2,  c("team", "passRecDiff"))
recData <- recData[team != "FA" ]
recData <- recData[,team := reorder(team, passRecDiff, function(x)-x)]
ggplot(recData, aes(x =team , y=passRecDiff, fill = passRecDiff > 0),  position = 'dodge' ) + 
  geom_bar(stat = "identity") + xlab("Team") + ylab("Completions - Recepetions") +scale_fill_discrete(guide = 'none') + ggtitle("Pass Completions and Receptions")
ggsave(paste(getwd(),"/Figures/Value Gap Analysis/Week ",week," passCompDiffernce.jpg", sep=""), width=10, height=10)
