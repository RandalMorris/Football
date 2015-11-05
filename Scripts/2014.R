#Set WD
setwd("E:/League Stats/")

#Load Libraries
library(ggplot2)

#Load data
Projections_2014 = read.csv("Data/Historical Projections/Projections Data-2014.csv",header = T)
season = 2014

#CreaTE_2014 indivlual
QB_2014 <- subset(Projections_2014, position == "QB" & actualPoints != "null")
RB_2014 <- subset(Projections_2014, position == "RB" & actualPoints != "null")
WR_2014 <- subset(Projections_2014, position == "WR" & actualPoints != "null")
TE_2014 <- subset(Projections_2014, position == "TE" & actualPoints != "null")
K_2014 <- subset(Projections_2014, position == "K" & actualPoints != "null")
DST_2014 <- subset(Projections_2014, position == "DST" & actualPoints != "null")

#indivdual density plots for current year
ggplot(QB_2014, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of QB Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/QB Density Projections.jpg", sep=""), width=10, height=10)

ggplot(RB_2014, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of RB Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/RB Density Projections.jpg", sep=""), width=10, height=10)

ggplot(WR_2014, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of WR Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/WR Density Projections.jpg", sep=""), width=10, height=10)

ggplot(TE_2014, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of TE Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/TE Density Projections.jpg", sep=""), width=10, height=10)

ggplot(K_2014, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of K Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/K Density Projections.jpg", sep=""), width=10, height=10)

ggplot(DST_2014, aes(x=points)) + geom_density(fill="red", alpha=.3) + 
  xlab("Player's Projected Points") + ggtitle("Density Plot of DST Projected Points") 
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/DST Density Projections.jpg", sep=""), width=10, height=10)

#Density plot of Projections
d2014 <- ggplot(Projections_2014, aes(x = points))
d2014 + geom_density(aes(colour="QB"), fill="red", alpha=.3, data=QB_2014) + 
  geom_density(aes(colour="RB"), fill="blue", alpha=.3, data=RB_2014) + 
  geom_density(aes(colour="WR"), fill="green", alpha=.3, data=WR_2014) + 
  geom_density(aes(colour="TE"), fill="yellow", alpha=.3, data=TE_2014) + 
  geom_density(aes(colour="K"), fill="black", alpha=.3, data=K_2014) + 
  geom_density(aes(colour="DST"), fill="orange", alpha=.3, data=DST_2014) + 
  ggtitle("Density Plot of Projections Positions") +
  scale_colour_manual(values=c("QB"="green", "RB"="red", "WR"="blue", "TE"="yellow", "K"="orange", "DST"="black"), name="Positions")
ggsave(paste(getwd(),"/Figures/Historical Figures/",season,"/Density Projections.jpg", sep=""), width=10, height=10)

save(Projections_2014, file = paste(getwd(), "/Data/Historical Projections/Projections Data-", season, ".RData", sep=""))
write.csv(Projections_2014, file=paste(getwd(), "/Data/Historical Projections/Projections Data-", season, ".csv", sep=""), row.names=FALSE)
