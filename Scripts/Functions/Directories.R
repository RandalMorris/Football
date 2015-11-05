#Create Directory needed

#Figures
ifelse(!dir.exists(file.path(paste(getwd(),"/Figures/Daily Fantasy/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Figures/Daily Fantasy/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Figures/Historical Figures/Daily Fantasy/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Figures/Weekly/PPR/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Figures/Weekly/PPR/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Figures/Weekly/STD/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Figures/Weekly/STD/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Figures/Historical Figures/Weekly/PPR/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Figures/Historical Figures/Weekly/STD/Week ",week, sep=""))),FALSE)

#Data
ifelse(!dir.exists(file.path(paste(getwd(),"/Data/Daily Fantasy/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Data/Daily Fantasy/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Data/Historical Projections/Daily Fantasy/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Data/Weekly/PPR/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Data/Weekly/PPR/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Data/Weekly/STD/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Data/Weekly/STD/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Data/Historical Projections/Weekly/PPR/Week ",week, sep=""))),FALSE)
ifelse(!dir.exists(file.path(paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Data/Historical Projections/Weekly/STD/Week ",week, sep=""))),FALSE)

#Value Gap Analysis
#Data
#ifelse(!dir.exists(file.path(paste(getwd(),"/Data/Value Gap Analysis/Week ",week, sep=""))),dir.create(file.path(paste(getwd(),"/Data/Value Gap Analysis/Week ",week, sep=""))),FALSE)