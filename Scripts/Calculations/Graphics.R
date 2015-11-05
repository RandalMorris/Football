#Footbal Graphics

library(lattice)
#Smooth Scatter plot on mean against DraftKings Cost
smoothScatter(fantasyproj$mean,fantasyproj$numfire_dkcost)

#Smooth Scatter plot on mean against K-means tiers
smoothScatter(fantasyproj$mean,fantasyproj$Tier)

#Subset points and yards
PassYd <- subset(projections, select = c("name","cbs_passyd","espn_passyd","ffs_passyd","fftoday_passyd",
                                    "fp_passyd","numfire_passyd"))

x <- subset(fantasyproj,select = c("name","mean"))

PassYd <- merge(y,x, by ="name")
rm(x)
#Reorder
PassYd <- subset(PassYd, select = c("name","mean","cbs_passyd","espn_passyd","ffs_passyd","fftoday_passyd",
                                    "fp_passyd","numfire_passyd"))

#ncol count
columns_passyard <- ncol(PassYd)
rows_passyard <- nrow(PassYd)


#find standard deviation of projections
PassYd$PassSD <- apply(PassYd[3:columns_passyard], 1, sd, na.rm=TRUE)


#find mean of projections
PassYd$PassMean <- apply(PassYd[3:columns_passyard], 1, mean, na.rm=TRUE)

#round mean/stdev
PassYd$PassMean <- round(PassYd$PassMean, digits = 1)
PassYd$PassSD <- round(PassYd$PassSD, digits = 1)

smoothScatter(PassYd$mean,PassYd$PassMean)
smoothScatter(fantasyproj$rank,fantasyproj$numfire_dkcost)
smoothScatter(fantasyproj$rank,fantasyproj$Ceiling)
smoothScatter(fantasyproj$mean,fantasyproj$sd)
