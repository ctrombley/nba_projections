source('calcProjections.R')

# Calculate projections
projections = calcProjections()

# Only select the columns we want to see
abbr_projections = subset(projections, select=c('DATE', 'HOME_TEAM', 'HOME_PROJ', 'AWAY_TEAM', 'AWAY_PROJ'))

# Get the date in YYYYMMDD format
yyyymmdd = format(Sys.time(), "%Y%m%d")

# Write the full output to a file
write.csv(projections, file=paste("output/", yyyymmdd, "_full.csv", sep=""))

# Write the output to a file
write.csv(abbr_projections, file=paste("output/", yyyymmdd, ".csv", sep=""))