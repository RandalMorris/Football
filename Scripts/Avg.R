
library(plyr)
ProjectionsAvg <- rbind.fill(Projections_2014,Projections_2013,Projections_2012,Projections_2011)

#subset data
QB_Avg <- subset(ProjectionsAvg, position == "QB" & actualPoints != "null")
RB_Avg <- subset(ProjectionsAvg, position == "RB" & actualPoints != "null")
WR_Avg <- subset(ProjectionsAvg, position == "WR" & actualPoints != "null")
TE_Avg <- subset(ProjectionsAvg, position == "TE" & actualPoints != "null")
K_Avg <- subset(ProjectionsAvg, position == "K" & actualPoints != "null")
DST_Avg <- subset(ProjectionsAvg, position == "DST" & actualPoints != "null")


DensityAvg <- ggplot(ProjectionsAvg, aes(x = actualPoints))
m + geom_density(aes(colour="QB"), fill="red", alpha=.3, data=QB_Avg) + 
  geom_density(aes(colour="RB"), fill="blue", alpha=.3, data=RB_Avg) + 
  geom_density(aes(colour="WR"), fill="green", alpha=.3, data=WR_Avg) + 
  geom_density(aes(colour="TE"), fill="yellow", alpha=.3, data=TE_Avg) + 
  geom_density(aes(colour="K"), fill="black", alpha=.3, data=K_Avg) + 
  geom_density(aes(colour="DST"), fill="orange", alpha=.3, data=DST_Avg) + 
  ggtitle("Density Plot of Projections Positions") +
  scale_colour_manual(values=c("QB"="green", "RB"="red", "WR"="blue", "TE"="yellow", "K"="orange", "DST"="black"), name="Positions")
