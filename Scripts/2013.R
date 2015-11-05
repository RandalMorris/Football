#Set WD
setwd("E:/League Stats/")

#Load Libraries
library(ggplot2)

#Load data
Projections_2013 = read.csv("Data/Historical Projections/Projections Data-2013.csv",header = T)
season = 2013

Projections_2013$points <- as.numeric(Projections_2013$points)

#Create indivlual
QB_2013 <- subset(Projections_2013, position == "QB" & actualPoints != "null")
RB_2013 <- subset(Projections_2013, position == "RB" & actualPoints != "null")
WR_2013 <- subset(Projections_2013, position == "WR" & actualPoints != "null")
TE_2013 <- subset(Projections_2013, position == "TE" & actualPoints != "null")

#indivdual density plots for current year
ggplot(QB_2013, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of QB Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/QB Density Projections.jpg", sep=""), width=10, height=10)

ggplot(RB_2013, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of RB Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/RB Density Projections.jpg", sep=""), width=10, height=10)

ggplot(WR_2013, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of WR Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/WR Density Projections.jpg", sep=""), width=10, height=10)

ggplot(TE_2013, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of TE Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/TE Density Projections.jpg", sep=""), width=10, height=10)

#Density plot of Projections
d2013 <- ggplot(Projections_2013, aes(x = points))
d2013 + geom_density(aes(colour="QB"), fill="red", alpha=.3, data=QB_2013) + 
  geom_density(aes(colour="RB"), fill="blue", alpha=.3, data=RB_2013) + 
  geom_density(aes(colour="WR"), fill="green", alpha=.3, data=WR_2013) + 
  geom_density(aes(colour="TE"), fill="yellow", alpha=.3, data=TE_2013) + 
  ggtitle("Density Plot of Projections Positions") +
  scale_colour_manual(values=c("QB"="green", "RB"="red", "WR"="blue", "TE"="yellow"), name="Positions")
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/Density Projections.jpg", sep=""), width=10, height=10)


save(Projections_2013, file = paste(getwd(), "/Data/Historical Projections/Projections Data-", season, ".RData", sep=""))
write.csv(Projections_2013, file=paste(getwd(), "/Data/Historical Projections/Projections Data-", season, ".csv", sep=""), row.names=FALSE)
