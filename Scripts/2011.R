#Set WD
setwd("E:/League Stats/")

#Load Libraries
library(ggplot2)

#Load data
Projections_2011 = read.csv("Data/Historical Projections/Projections Data-2011.csv",header = T)
season = 2011

Projections_2011$points <- as.numeric(Projections_2011$points)

#Create indivlual
QB_2011 <- subset(Projections_2011, position == "QB" & actualPoints != "null")
RB_2011 <- subset(Projections_2011, position == "RB" & actualPoints != "null")
WR_2011 <- subset(Projections_2011, position == "WR" & actualPoints != "null")
TE_2011 <- subset(Projections_2011, position == "TE" & actualPoints != "null")

#indivdual density plots for current year
ggplot(QB_2011, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of QB Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/QB Density Projections.jpg", sep=""), width=10, height=10)

ggplot(RB_2011, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of RB Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/RB Density Projections.jpg", sep=""), width=10, height=10)

ggplot(WR_2011, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of WR Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/WR Density Projections.jpg", sep=""), width=10, height=10)

ggplot(TE_2011, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of TE Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/TE Density Projections.jpg", sep=""), width=10, height=10)

#Density plot of Projections
d2011 <- ggplot(Projections_2011, aes(x = points))
d2011 + geom_density(aes(colour="QB"), fill="red", alpha=.3, data=QB_2011) + 
  geom_density(aes(colour="RB"), fill="blue", alpha=.3, data=RB_2011) + 
  geom_density(aes(colour="WR"), fill="green", alpha=.3, data=WR_2011) + 
  geom_density(aes(colour="TE"), fill="yellow", alpha=.3, data=TE_2011) + 
  ggtitle("Density Plot of Projections Positions") +
  scale_colour_manual(values=c("QB"="green", "RB"="red", "WR"="blue", "TE"="yellow"), name="Positions")
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/Density Projections.jpg", sep=""), width=10, height=10)


save(Projections_2011, file = paste(getwd(), "/Data/Historical Projections/Projections Data-", season, ".RData", sep=""))
write.csv(Projections_2011, file=paste(getwd(), "/Data/Historical Projections/Projections Data-", season, ".csv", sep=""), row.names=FALSE)
